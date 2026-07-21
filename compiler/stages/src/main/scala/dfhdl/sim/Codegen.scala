package dfhdl.sim

import java.io.{ByteArrayOutputStream, OutputStream}
import java.net.URI
import javax.tools.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Codegen tier: runtime code generation — generate straight-line Java from the netlist, compile it
  * in-memory with the JDK compiler, and hot-load it as a [[SimKernel]] over the same signal array
  * as the interpreter tier.
  *
  * This is the JDK-17-portable mockup of the planned bytecode backend (ClassFile API / ASM): the
  * shape of the emitted code is identical to what the bytecode generator would produce —
  * combinational values in method locals, constants folded into the instruction stream, and bodies
  * split into sub-methods.
  *
  * State is kernel-owned and width-typed: registers and cross-method (spilled) comb values live in
  * fields typed `int` when the value is at most 32 bits wide and `long` otherwise. The int form is
  * what makes large 32-bit-dominated designs fast — 32-bit adds wrap for free with no mask AND, and
  * 32-bit rotate patterns compile to a single `ror` instruction — and measurements show it only
  * pays with int-typed STORAGE: computing in int over a long-typed signal array loses more to the
  * per-access truncate/widen boundary than the arithmetic saves. The shared signal array is
  * therefore only made coherent at kernel entry (registers load — pokes land in the array) and exit
  * (registers and spills store — peeks, scheduler wait bounds, and text-output arguments read the
  * array after the call returns), which the bulk-run contract permits; the sync cost amortizes over
  * the cycles of each call.
  *
  * HotSpot never JIT-compiles a method over `-XX:HugeMethodLimit` (8000 bytecodes) — one oversized
  * method leaves the whole per-cycle path in the JVM bytecode interpreter and collapses
  * large-netlist throughput — so op chunks are cut by an estimated bytecode cost with a wide safety
  * margin, and the register commit is likewise split into slice methods. The commit is emitted in
  * consumer-first topological order over the reg-to-reg next edges, so acyclic move chains (shift
  * registers) are direct field-to-field copies; only dependency cycles stage a read through a
  * scratch array, self-move holds are skipped, and comb/const next values always commit directly
  * (comb values are never written during the commit).
  *
  * Move chains of eight or more int-typed moves (shift registers, delay pipelines) commit as
  * overlapping 8-lane vector moves over a shared chain array instead of scalar field-to-field
  * copies, which profiling shows are a large share of big-design cycle time: every lane loads
  * before any lane stores, the same read-before-overwrite guarantee the consumer-first order gives
  * the scalar form. Chain registers live in the array full-time (compute reads them by constant
  * index); all other state stays in fields, which measure faster for compute. This engages only
  * when the host JVM resolves the incubator vector module (start the JVM with
  * `--add-modules jdk.incubator.vector`); otherwise the commit silently stays scalar.
  */
object Codegen:
  private var counter = 0

  /** Lane count of the host JVM's preferred int vector species; 0 when the incubator vector module
    * is not resolvable (it must be added explicitly at JVM start with
    * `--add-modules jdk.incubator.vector`). Probed reflectively so this project never compiles
    * against the incubator API.
    */
  private lazy val intVectorLanes: Int =
    try
      val species =
        Class.forName("jdk.incubator.vector.IntVector").getField("SPECIES_PREFERRED").get(null)
      Class.forName("jdk.incubator.vector.VectorSpecies").getMethod("length")
        .invoke(species).asInstanceOf[Int]
    catch case _: Throwable => 0

  private def vectorCommitAvailable: Boolean =
    intVectorLanes >= 8 && !java.lang.Boolean.getBoolean("dfhdl.sim.codegen.noVector")

  def compile(
      nl: Netlist,
      maxOpsPerMethod: Int = 500,
      dumpSource: Boolean = false,
      observed: Set[Int] = Set.empty,
      watchNode: Int = -1
  ): SimKernel =
    val className = synchronized {
      counter += 1
      s"dfhdl.sim.gen.Kernel$counter"
    }
    val source = generateJava(nl, className, maxOpsPerMethod, observed, watchNode)
    if dumpSource then println(source)
    val cls = compileJava(className, source, source.contains("jdk.incubator.vector"))
    cls.getDeclaredConstructor().newInstance().asInstanceOf[SimKernel]
  end compile

  private def generateJava(
      nl: Netlist,
      className: String,
      maxOpsPerMethod: Int,
      observed: Set[Int],
      watchNode: Int
  ): String =
    val comb = nl.combNodeIds
    def isCombNode(id: Int): Boolean =
      nl.opcodes(id) != Op.REG && nl.opcodes(id) != Op.CONST

    // ---- width-typed emission --------------------------------------------------------------
    def isIntN(id: Int): Boolean = nl.widths(id) <= 32
    def jt(id: Int): String = if isIntN(id) then "int" else "long"
    // ops with a dedicated int form; anything else falls back to long compute + an (int) cast
    val intOps = Set(Op.ADD, Op.SUB, Op.MUL, Op.XOR, Op.AND, Op.OR, Op.NOT, Op.ROTR, Op.SHL,
      Op.SHR, Op.MUX, Op.ROM, Op.RESIZE, Op.EQ, Op.NEQ, Op.ULT, Op.MOV)
    def intMode(id: Int): Boolean =
      isIntN(id) && intOps.contains(nl.opcodes(id)) && nl.nodeInputs(id).forall(isIntN)

    // ---- bytecode-cost model for method sizing ----------------------------------------------
    val regNextSpill = nl.regNextIds.iterator.filter(isCombNode).toSet
    def rdCost(id: Int): Int = nl.opcodes(id) match
      case Op.CONST => 3 // folded literal
      case Op.REG   => 7 // signal-array read
      case _        => 5 // local or spilled read
    def opCost(id: Int): Int =
      val extra = nl.opcodes(id) match
        case Op.MUX                  => 4
        case Op.ROTR                 => 8
        case Op.SHLV | Op.SHRV       => 14
        case Op.SRAV                 => 22
        case Op.SDIV | Op.SREM       => 48
        case Op.UDIV | Op.UREM       => 16
        case Op.SLT                  => 12
        case Op.ULT                  => 8
        case Op.REV | Op.EQ | Op.NEQ => 6
        case Op.ROM                  => 8
        case _                       => 0
      val store = if observed.contains(id) || regNextSpill.contains(id) then 8 else 0
      val cost = 14 + nl.nodeInputs(id).map(rdCost).sum + extra + store
      // int-form ops over int-typed fields emit roughly two thirds of the long form's bytecode
      // (no wide operands, no mask ANDs at width 32), so a chunk fits proportionally more
      if intMode(id) then cost * 2 / 3 else cost
    end opCost
    // stay well under the 8000-bytecode JIT limit — the estimate is rough and cross-chunk
    // spill stores are only known after the boundaries are fixed; throughput is flat across
    // the 3000..7500 range, so the pick favors estimation-error margin
    val methodBudget = 5000

    val chunks =
      val out = Vector.newBuilder[Vector[Int]]
      val cur = Vector.newBuilder[Int]
      var curCost = 0
      var curCount = 0
      for id <- comb do
        val c = opCost(id)
        if curCount > 0 && (curCost + c > methodBudget || curCount >= maxOpsPerMethod) then
          out += cur.result(); cur.clear(); curCost = 0; curCount = 0
        cur += id
        curCost += c
        curCount += 1
      if curCount > 0 then out += cur.result()
      out.result()
    // single-method mode (the small-design fast path): ops and commit all inline in the run loop
    val nSelfMoves = nl.regNextIds.iterator.zipWithIndex.count((n, i) => n == nl.regIds(i))
    val single =
      chunks.sizeIs <= 1 &&
        comb.iterator.map(opCost).sum + (nl.regIds.length - nSelfMoves) * 22 <= methodBudget
    val chunkOf: Map[Int, Int] =
      if single then Map.empty
      else (for (c, ci) <- chunks.zipWithIndex; id <- c yield id -> ci).toMap
    // Values that must be materialized in the signal array: externally observed (peek), consumed
    // across chunk boundaries, or consumed by the register commit outside the producing method.
    val spill = mutable.Set.empty[Int]
    spill ++= observed.filter(isCombNode)
    if !single then
      for id <- comb; in <- nl.nodeInputs(id) do
        if isCombNode(in) && chunkOf(in) != chunkOf(id) then spill += in
      spill ++= regNextSpill

    // register commit ordering: profiling shows plain data movement dominating large designs, so
    // the commit avoids staging wherever possible. A self-move (an unassigned hold) is skipped
    // entirely. A register that reads another register commits BEFORE its source is overwritten
    // (consumer-first topological order over the reg-to-reg next edges), so acyclic move chains
    // (shift registers) are direct signal-to-signal copies; only an edge on a dependency cycle
    // (e.g. a register swap) is broken by staging its consumer's read through the `nxt` scratch
    // array, gathered before any commit write. Comb/const next values are always direct (comb
    // slots are never written during the commit).
    val regIdx = nl.regIds.indices.toVector
    val selfMove = regIdx.filter(i => nl.regNextIds(i) == nl.regIds(i)).toSet
    val regIndexOfNode = nl.regIds.zipWithIndex.toMap
    // the single successor edge: consumer i reads register srcOf(i) (self-moves excluded on both
    // sides — a skipped register is never written, so reading it needs no ordering)
    val srcOf: Map[Int, Int] =
      regIdx.iterator.filterNot(selfMove).flatMap { i =>
        regIndexOfNode.get(nl.regNextIds(i)).filterNot(selfMove).map(i -> _)
      }.toMap
    val stagedSet = mutable.Set.empty[Int]
    val commitOrder =
      val color = Array.ofDim[Int](nl.regIds.length) // 0 white, 1 gray, 2 black
      val post = mutable.ArrayBuffer.empty[Int]
      for start <- regIdx do
        if !selfMove(start) && color(start) == 0 then
          var chain = List.empty[Int] // head = deepest visited
          var cur = start
          var walking = true
          while walking do
            color(cur) = 1
            chain ::= cur
            srcOf.get(cur) match
              case Some(s) if color(s) == 0 => cur = s
              case Some(s) if color(s) == 1 =>
                stagedSet += cur // gray source closes a cycle: this consumer reads through nxt
                walking = false
              case _ => walking = false // no edge, or an already-ordered (black) source
          for i <- chain do color(i) = 2
          post ++= chain // per-tree postorder (deepest first)
      end for
      post.reverseIterator.toVector // reverse postorder: every consumer before its source
    end commitOrder
    val hasStaged = stagedSet.nonEmpty
    val gatherSlices = regIdx.filter(stagedSet).grouped(400).toVector

    // Move chains of at least 8 int-typed moves keep their registers in the shared CHI array
    // (slots assigned along the chain, so every move is slot k <- slot k+1) and commit as a few
    // overlapping 8-lane vector moves, loads all issued before stores. Hoisting a member's write
    // up to the chain head's commit position is safe: any external consumer of a chain register
    // commits before the head (consumer-first order puts every consumer before its source's
    // write, and chains emit contiguously). Cycle-staged links and non-int registers never join
    // a chain; a design is only vectorized when the host JVM resolves jdk.incubator.vector.
    val vecPaths: Vector[Vector[Int]] = // consumer-first register indices per chain
      if single || !vectorCommitAvailable then Vector.empty
      else
        val claimed = mutable.Set.empty[Int]
        val srcsInUse = srcOf.values.toSet
        val out = Vector.newBuilder[Vector[Int]]
        for head <- regIdx do
          if srcOf.contains(head) && !srcsInUse(head) && !claimed(head)
            && isIntN(nl.regIds(head))
          then
            val buf = Vector.newBuilder[Int]
            var cur = head
            var walking = true
            while walking do
              buf += cur
              claimed += cur
              val next = srcOf.get(cur)
                .filter(s => !stagedSet(cur) && !claimed(s) && isIntN(nl.regIds(s)))
              next match
                case Some(s) => cur = s
                case None    => walking = false
            val p = buf.result()
            if p.length - 1 >= 8 then out += p
        end for
        out.result()
    val chiSlot: Map[Int, Int] = // register NODE id -> shared chain-array slot
      val m = Map.newBuilder[Int, Int]
      var base = 0
      for p <- vecPaths do
        for (i, k) <- p.zipWithIndex do m += nl.regIds(i) -> (base + k)
        base += p.length
      m.result()
    val chiSize = vecPaths.map(_.length).sum
    val vecStart: Map[Int, Vector[Int]] = vecPaths.iterator.map(p => p.head -> p).toMap
    // members whose move a vector block emits (all but the last member, whose own next value
    // commits separately at its own position)
    val vecOwned: Set[Int] = vecPaths.iterator.flatMap(_.dropRight(1)).toSet

    // commit slices: a vectorized chain is one atomic item anchored at its head's position
    val commitSlices: Vector[Vector[Int]] =
      val out = Vector.newBuilder[Vector[Int]]
      val cur = Vector.newBuilder[Int]
      var w = 0
      def flush(): Unit =
        if w > 0 then
          out += cur.result(); cur.clear(); w = 0
      for i <- commitOrder do
        if !vecOwned(i) || vecStart.contains(i) then
          val cost = vecStart.get(i).map(p => p.length / 4 + 2).getOrElse(1)
          if w > 0 && w + cost > 350 then flush()
          cur += i
          w += cost
      flush()
      out.result()
    end commitSlices
    val dirtySlices = commitOrder.grouped(150).toVector

    def hexL(v: Long): String = "0x%XL".format(v)
    def hexI(v: Long): String = "0x%X".format(v & 0xffffffffL)
    def isLocal(id: Int, ci: Int): Boolean = single || (ci >= 0 && chunkOf(id) == ci)
    // an int register's storage reference: chain-array slot or its own field
    def regRefI(id: Int): String = chiSlot.get(id) match
      case Some(s) => s"CHI[$s]"
      case None    => s"f$id"
    // a register's commit/sync write target in its own storage type
    def regLhs(id: Int): String = if isIntN(id) then regRefI(id) else s"f$id"
    // Read a node's value as a long from within chunk `ci` (-1 = outside all chunks). State
    // lives in width-typed fields; int values are always stored masked, so widening is unsigned.
    def rd(id: Int, ci: Int): String = nl.opcodes(id) match
      case Op.CONST => hexL(nl.initVals(id))
      case Op.REG   => if isIntN(id) then s"Integer.toUnsignedLong(${regRefI(id)})" else s"f$id"
      case _        =>
        val ref = if isLocal(id, ci) then s"v$id" else s"f$id"
        if isIntN(id) then s"Integer.toUnsignedLong($ref)" else ref
    // Read an int-typed node's value as an int (callers guarantee isIntN(id))
    def rdI(id: Int, ci: Int): String = nl.opcodes(id) match
      case Op.CONST => hexI(nl.initVals(id))
      case Op.REG   => regRefI(id)
      case _        => if isLocal(id, ci) then s"v$id" else s"f$id"

    def emitOp(id: Int, ci: Int): String =
      val w = nl.widths(id)
      val a = nl.inA(id)
      val b = nl.inB(id)
      val c = nl.inC(id)
      val expr =
        if intMode(id) then
          val mI = hexI(nl.maskOf(id))
          def rd(x: Int): String = rdI(x, ci)
          def mask(e: String): String = if w == 32 then e else s"($e) & $mI"
          nl.opcodes(id) match
            case Op.ADD  => mask(s"${rd(a)} + ${rd(b)}")
            case Op.SUB  => mask(s"${rd(a)} - ${rd(b)}")
            case Op.MUL  => mask(s"${rd(a)} * ${rd(b)}")
            case Op.XOR  => s"${rd(a)} ^ ${rd(b)}"
            case Op.AND  => s"${rd(a)} & ${rd(b)}"
            case Op.OR   => s"${rd(a)} | ${rd(b)}"
            case Op.NOT  => mask(s"~${rd(a)}")
            case Op.ROTR =>
              if w == 32 then s"Integer.rotateRight(${rd(a)}, $b)"
              else s"((${rd(a)} >>> $b) | (${rd(a)} << ${w - b})) & $mI"
            case Op.SHL    => mask(s"${rd(a)} << $b")
            case Op.SHR    => s"${rd(a)} >>> $b"
            case Op.MUX    => s"(${rd(a)} != 0 ? ${rd(b)} : ${rd(c)})"
            case Op.ROM    => s"(int) ROM$b[${rd(a)} & ${nl.romTables(b).length - 1}]"
            case Op.RESIZE => mask(rd(a))
            case Op.EQ     => s"(${rd(a)} == ${rd(b)} ? 1 : 0)"
            case Op.NEQ    => s"(${rd(a)} != ${rd(b)} ? 1 : 0)"
            case Op.ULT    => s"(Integer.compareUnsigned(${rd(a)}, ${rd(b)}) < 0 ? 1 : 0)"
            case Op.MOV    => rd(a)
            case other     => throw new IllegalStateException(s"bad int-form opcode $other")
          end match
        else
          val m = hexL(nl.maskOf(id))
          val longExpr = nl.opcodes(id) match
            case Op.ADD    => s"(${rd(a, ci)} + ${rd(b, ci)}) & $m"
            case Op.XOR    => s"${rd(a, ci)} ^ ${rd(b, ci)}"
            case Op.AND    => s"${rd(a, ci)} & ${rd(b, ci)}"
            case Op.OR     => s"${rd(a, ci)} | ${rd(b, ci)}"
            case Op.NOT    => s"(~${rd(a, ci)}) & $m"
            case Op.ROTR   => s"((${rd(a, ci)} >>> $b) | (${rd(a, ci)} << ${w - b})) & $m"
            case Op.SHL    => s"(${rd(a, ci)} << $b) & $m"
            case Op.SHR    => s"${rd(a, ci)} >>> $b"
            case Op.MUX    => s"(${rd(a, ci)} != 0L ? ${rd(b, ci)} : ${rd(c, ci)})"
            case Op.ROM    => s"ROM$b[(int) (${rd(a, ci)} & ${nl.romTables(b).length - 1}L)]"
            case Op.RESIZE => s"${rd(a, ci)} & $m"
            case Op.EQ     => s"(${rd(a, ci)} == ${rd(b, ci)} ? 1L : 0L)"
            case Op.NEQ    => s"(${rd(a, ci)} != ${rd(b, ci)} ? 1L : 0L)"
            case Op.MOV    => rd(a, ci)
            case Op.SUB    => s"(${rd(a, ci)} - ${rd(b, ci)}) & $m"
            case Op.SHLV   =>
              s"(${rd(b, ci)} >= 64L ? 0L : (${rd(a, ci)} << ${rd(b, ci)}) & $m)"
            case Op.SHRV =>
              s"(${rd(b, ci)} >= 64L ? 0L : ${rd(a, ci)} >>> ${rd(b, ci)})"
            case Op.SRAV =>
              val s = 64 - w
              s"((((${rd(a, ci)} << $s) >> $s) >> Math.min(${rd(b, ci)}, 63L)) & $m)"
            case Op.ULT =>
              s"(Long.compareUnsigned(${rd(a, ci)}, ${rd(b, ci)}) < 0 ? 1L : 0L)"
            case Op.SLT =>
              val s = 64 - nl.widths(a)
              s"(((${rd(a, ci)} << $s) >> $s) < ((${rd(b, ci)} << $s) >> $s) ? 1L : 0L)"
            case Op.MUL  => s"(${rd(a, ci)} * ${rd(b, ci)}) & $m"
            case Op.UDIV =>
              s"(${rd(b, ci)} == 0L ? 0L : Long.divideUnsigned(${rd(a, ci)}, ${rd(b, ci)}))"
            case Op.SDIV =>
              val s = 64 - w
              s"(((${rd(b, ci)} << $s) >> $s) == 0L ? 0L : " +
                s"(((${rd(a, ci)} << $s) >> $s) / ((${rd(b, ci)} << $s) >> $s)) & $m)"
            case Op.UREM =>
              s"(${rd(b, ci)} == 0L ? 0L : Long.remainderUnsigned(${rd(a, ci)}, ${rd(b, ci)}))"
            case Op.SREM =>
              val s = 64 - w
              s"(((${rd(b, ci)} << $s) >> $s) == 0L ? 0L : " +
                s"(((${rd(a, ci)} << $s) >> $s) % ((${rd(b, ci)} << $s) >> $s)) & $m)"
            case Op.REV => s"Long.reverse(${rd(a, ci)}) >>> ${64 - w}"
            case other  => throw new IllegalStateException(s"bad opcode $other")
          if isIntN(id) then s"(int) ($longExpr)" else longExpr
      val store = if spill.contains(id) then s" f$id = v$id;" else ""
      s"      ${jt(id)} v$id = $expr;$store"
    end emitOp

    // The commit target and its next value share a width by construction, so a register's
    // next value is read in the register's own storage type
    def rdNext(i: Int, ci: Int): String =
      val n = nl.regNextIds(i)
      if isIntN(nl.regIds(i)) then rdI(n, ci) else rd(n, ci)
    def rdStaged(i: Int): String =
      if isIntN(nl.regIds(i)) then s"(int) nxt[$i]" else s"nxt[$i]"

    // Kernel-owned state: registers and spilled comb values live in width-typed fields; the
    // shared signal array is only made coherent at kernel entry (load registers — pokes land
    // there) and exit (store registers and spills — peeks, wait bounds, and text-output
    // arguments read there). The sync cost is per bulk call, amortized over its cycles.
    val syncInSlices = nl.regIds.toVector.grouped(400).toVector
    val syncOutSlices = (nl.regIds.toVector ++ spill.toVector.sorted).grouped(400).toVector

    val simpleName = className.split('.').last
    val pkg = className.stripSuffix("." + simpleName)
    val sb = new StringBuilder
    sb ++= s"package $pkg;\n\n"
    sb ++= s"public final class $simpleName implements dfhdl.sim.SimKernel {\n"
    for (table, i) <- nl.romTables.zipWithIndex do
      sb ++= s"  private static final long[] ROM$i = { ${table.map(hexL).mkString(", ")} };\n"
    if chiSize > 0 then
      // static state is per-kernel-safe: every compile() call generates a distinct class in its
      // own class loader, so one kernel instance never shares CHI with another
      sb ++= s"  private static final int[] CHI = new int[$chiSize];\n"
      sb ++= "  private static final jdk.incubator.vector.VectorSpecies<Integer> VS =\n" +
        "      jdk.incubator.vector.IntVector.SPECIES_256;\n"
    for id <- (nl.regIds.toVector ++ spill.toVector).sorted if !chiSlot.contains(id) do
      sb ++= s"  private ${jt(id)} f$id;\n"
    if !single && hasStaged then
      sb ++= s"  private final long[] nxt = new long[${nl.regIds.length}];\n"
    val nxtParam = if hasStaged then "long[] nxt" else ""
    val nxtArg = if hasStaged then "nxt" else ""

    def emitSyncIn(indent: String): Unit =
      for k <- syncInSlices.indices do sb ++= s"${indent}syncIn$k(sig);\n"
    def emitSyncOut(indent: String): Unit =
      for k <- syncOutSlices.indices do sb ++= s"${indent}syncOut$k(sig);\n"

    // one chain's commit: overlapping 8-lane moves, every load issued before any store
    def emitVecChain(path: Vector[Int]): Unit =
      val base = chiSlot(nl.regIds(path.head))
      val moves = path.length - 1
      val starts =
        val s = (0 to moves - 8 by 8).toVector
        if s.last == moves - 8 then s else s :+ (moves - 8)
      for (st, t) <- starts.zipWithIndex do
        sb ++= s"    var c${base}_$t = " +
          s"jdk.incubator.vector.IntVector.fromArray(VS, CHI, ${base + 1 + st});\n"
      for (st, t) <- starts.zipWithIndex do
        sb ++= s"    c${base}_$t.intoArray(CHI, ${base + st});\n"

    def emitCombCalls(indent: String): Unit =
      if single then
        for id <- comb do
          sb ++= emitOp(id, 0)
          sb += '\n'
      else for ci <- chunks.indices do sb ++= s"${indent}chunk$ci();\n"

    def emitCommitCalls(indent: String): Unit =
      for gi <- gatherSlices.indices do sb ++= s"${indent}gather$gi(nxt);\n"
      for ci <- commitSlices.indices do sb ++= s"${indent}commit$ci($nxtArg);\n"

    def emitSingleCommit(indent: String): Unit =
      // two-phase commit: read every next value before any register field is overwritten,
      // otherwise register-to-register chains (shift registers) cascade within one cycle
      for i <- regIdx do
        if !selfMove(i) then sb ++= s"$indent${jt(nl.regIds(i))} nxt$i = ${rdNext(i, 0)};\n"
      for (r, i) <- nl.regIds.zipWithIndex do
        if !selfMove(i) then sb ++= s"${indent}f$r = nxt$i;\n"

    // watched run: exit after any cycle whose settled watch value is nonzero; the watch node
    // is fixed at generation time (the runtime argument is only cross-checked)
    val watchCheck =
      if watchNode < 0 then "false"
      else if isIntN(watchNode) then s"${rdI(watchNode, -1)} != 0"
      else s"${rd(watchNode, -1)} != 0L"

    def emitRunMethod(watch: Boolean): Unit =
      sb ++= (
        if watch then "\n  public long runWatch(long[] sig, long cycles, int watch) {\n"
        else "\n  public void run(long[] sig, long cycles) {\n"
      )
      if watch then
        sb ++= s"    if (watch != $watchNode) " +
          "throw new IllegalArgumentException(\"unexpected watch node \" + watch);\n"
      emitSyncIn("    ")
      if !single && hasStaged then sb ++= "    final long[] nxt = this.nxt;\n"
      if watch then sb ++= "    long done = cycles;\n"
      sb ++= "    for (long cyc = 0L; cyc < cycles; cyc++) {\n"
      emitCombCalls("      ")
      if single then emitSingleCommit("      ")
      else emitCommitCalls("      ")
      if watch then sb ++= s"      if ($watchCheck) { done = cyc + 1L; break; }\n"
      sb ++= "    }\n"
      emitSyncOut("    ")
      if watch then sb ++= "    return done;\n"
      sb ++= "  }\n"
    end emitRunMethod

    emitRunMethod(watch = false)
    emitRunMethod(watch = true)
    // comb-only sweep (no register commit) for settle-on-peek
    sb ++= "\n  public void settle(long[] sig) {\n"
    emitSyncIn("    ")
    emitCombCalls("    ")
    emitSyncOut("    ")
    sb ++= "  }\n"
    // one tracked-commit cycle for the scheduler's quiescence probe
    sb ++= "\n  public boolean stepDirty(long[] sig) {\n"
    emitSyncIn("    ")
    if single then
      emitCombCalls("    ")
      sb ++= "    boolean dirty = false;\n"
      for i <- regIdx do
        if !selfMove(i) then sb ++= s"    ${jt(nl.regIds(i))} nxt$i = ${rdNext(i, 0)};\n"
      for (r, i) <- nl.regIds.zipWithIndex do
        if !selfMove(i) then
          if nl.regTracked(i) then sb ++= s"    if (f$r != nxt$i) dirty = true;\n"
          sb ++= s"    f$r = nxt$i;\n"
    else
      if hasStaged then sb ++= "    final long[] nxt = this.nxt;\n"
      emitCombCalls("    ")
      for gi <- gatherSlices.indices do sb ++= s"    gather$gi(nxt);\n"
      sb ++= "    boolean dirty = false;\n"
      for di <- dirtySlices.indices do
        sb ++= s"    dirty = commitDirty$di($nxtArg) | dirty;\n"
    end if
    emitSyncOut("    ")
    sb ++= "    return dirty;\n  }\n"
    if !single then
      for (chunk, ci) <- chunks.zipWithIndex do
        sb ++= s"\n  private void chunk$ci() {\n"
        for id <- chunk do
          sb ++= emitOp(id, ci)
          sb += '\n'
        sb ++= "  }\n"
      for (slice, gi) <- gatherSlices.zipWithIndex do
        sb ++= s"\n  private void gather$gi(long[] nxt) {\n"
        for i <- slice do sb ++= s"    nxt[$i] = ${rd(nl.regNextIds(i), -1)};\n"
        sb ++= "  }\n"
      for (slice, ci) <- commitSlices.zipWithIndex do
        sb ++= s"\n  private void commit$ci($nxtParam) {\n"
        for i <- slice do
          vecStart.get(i) match
            case Some(path) => emitVecChain(path)
            case None       =>
              val v = if stagedSet(i) then rdStaged(i) else rdNext(i, -1)
              sb ++= s"    ${regLhs(nl.regIds(i))} = $v;\n"
        sb ++= "  }\n"
      for (slice, di) <- dirtySlices.zipWithIndex do
        sb ++= s"\n  private boolean commitDirty$di($nxtParam) {\n"
        sb ++= "    boolean dirty = false;\n"
        for i <- slice do
          val r = nl.regIds(i)
          val v = if stagedSet(i) then rdStaged(i) else rdNext(i, -1)
          if nl.regTracked(i) then
            sb ++= s"    ${jt(r)} x$i = $v;\n"
            sb ++= s"    if (${regLhs(r)} != x$i) dirty = true;\n"
            sb ++= s"    ${regLhs(r)} = x$i;\n"
          else sb ++= s"    ${regLhs(r)} = $v;\n"
        sb ++= "    return dirty;\n  }\n"
    end if
    for (slice, k) <- syncInSlices.zipWithIndex do
      sb ++= s"\n  private void syncIn$k(long[] sig) {\n"
      for r <- slice do
        val v = if isIntN(r) then s"(int) sig[$r]" else s"sig[$r]"
        sb ++= s"    ${regLhs(r)} = $v;\n"
      sb ++= "  }\n"
    for (slice, k) <- syncOutSlices.zipWithIndex do
      sb ++= s"\n  private void syncOut$k(long[] sig) {\n"
      for id <- slice do
        val v = if isIntN(id) then s"Integer.toUnsignedLong(${regRefI(id)})" else s"f$id"
        sb ++= s"    sig[$id] = $v;\n"
      sb ++= "  }\n"
    sb ++= "}\n"
    sb.result()
  end generateJava

  private def compileJava(className: String, source: String, useVector: Boolean): Class[?] =
    val compiler = ToolProvider.getSystemJavaCompiler
    require(compiler ne null, "JDK javac not available (running on a JRE?)")
    val diags = new DiagnosticCollector[JavaFileObject]
    val stdFm = compiler.getStandardFileManager(diags, null, null)
    val outputs = mutable.Map.empty[String, ByteArrayOutputStream]
    val fm = new ForwardingJavaFileManager[JavaFileManager](stdFm):
      override def getJavaFileForOutput(
          location: JavaFileManager.Location,
          name: String,
          kind: JavaFileObject.Kind,
          sibling: FileObject
      ): JavaFileObject =
        val bos = new ByteArrayOutputStream
        outputs(name) = bos
        new SimpleJavaFileObject(
          URI.create("mem:///" + name.replace('.', '/') + ".class"),
          JavaFileObject.Kind.CLASS
        ):
          override def openOutputStream(): OutputStream = bos
    val srcObj = new SimpleJavaFileObject(
      URI.create("string:///" + className.replace('.', '/') + ".java"),
      JavaFileObject.Kind.SOURCE
    ):
      override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = source
    // The generated class references the SimKernel interface — point javac at its classes dir
    val selfCp = java.nio.file.Paths
      .get(classOf[SimKernel].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val cp = selfCp + java.io.File.pathSeparator + System.getProperty("java.class.path")
    val opts =
      if useVector then
        java.util.List.of("-classpath", cp, "--add-modules", "jdk.incubator.vector")
      else java.util.List.of("-classpath", cp)
    val task =
      compiler.getTask(
        null,
        fm,
        diags,
        opts,
        null,
        java.util.List.of(srcObj)
      )
    if !task.call().booleanValue() then
      val msgs = diags.getDiagnostics.asScala.mkString("\n")
      throw new RuntimeException(s"javac failed:\n$msgs\n----- source -----\n$source")
    val loader = new ClassLoader(classOf[SimKernel].getClassLoader):
      override protected def findClass(name: String): Class[?] =
        outputs.get(name) match
          case Some(bos) =>
            val bytes = bos.toByteArray
            defineClass(name, bytes, 0, bytes.length)
          case None => throw new ClassNotFoundException(name)
    loader.loadClass(className)
  end compileJava
end Codegen
