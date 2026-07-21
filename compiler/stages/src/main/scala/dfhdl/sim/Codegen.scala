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
  * combinational values in method locals, state reads/writes through the signal array, constants
  * folded into the instruction stream, and bodies split into sub-methods (with cross-chunk values
  * spilled to the signal array). HotSpot never JIT-compiles a method over `-XX:HugeMethodLimit`
  * (8000 bytecodes) — one oversized method leaves the whole per-cycle path in the JVM bytecode
  * interpreter and collapses large-netlist throughput — so op chunks are cut by an estimated
  * bytecode cost with a wide safety margin, and the register commit is likewise split into slice
  * methods: next values that are themselves registers stage through a scratch array (two-phase
  * commit across method boundaries), while comb/const next values commit directly (comb slots are
  * never written during the commit).
  */
object Codegen:
  private var counter = 0

  def compile(
      nl: Netlist,
      maxOpsPerMethod: Int = 250,
      dumpSource: Boolean = false,
      observed: Set[Int] = Set.empty
  ): SimKernel =
    val className = synchronized {
      counter += 1
      s"dfhdl.sim.gen.Kernel$counter"
    }
    val source = generateJava(nl, className, maxOpsPerMethod, observed)
    if dumpSource then println(source)
    val cls = compileJava(className, source)
    cls.getDeclaredConstructor().newInstance().asInstanceOf[SimKernel]

  private def generateJava(
      nl: Netlist,
      className: String,
      maxOpsPerMethod: Int,
      observed: Set[Int]
  ): String =
    val comb = nl.combNodeIds
    def isCombNode(id: Int): Boolean =
      nl.opcodes(id) != Op.REG && nl.opcodes(id) != Op.CONST

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
      14 + nl.nodeInputs(id).map(rdCost).sum + extra + store
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
    val single =
      chunks.sizeIs <= 1 && comb.iterator.map(opCost).sum + nl.regIds.length * 22 <= methodBudget
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

    def hexL(v: Long): String = "0x%XL".format(v)
    // Read a node's value from within chunk `ci` (-1 = outside all chunks)
    def rd(id: Int, ci: Int): String = nl.opcodes(id) match
      case Op.CONST => hexL(nl.initVals(id))
      case Op.REG   => s"sig[$id]"
      case _        => if single || (ci >= 0 && chunkOf(id) == ci) then s"v$id" else s"sig[$id]"

    def emitOp(id: Int, ci: Int): String =
      val w = nl.widths(id)
      val m = hexL(nl.maskOf(id))
      val a = nl.inA(id)
      val b = nl.inB(id)
      val c = nl.inC(id)
      val expr = nl.opcodes(id) match
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
      val store = if spill.contains(id) then s" sig[$id] = v$id;" else ""
      s"      long v$id = $expr;$store"
    end emitOp

    // register commit slicing (chunked mode): a register whose next value is another register
    // stages through the `nxt` scratch array (its source slot is overwritten during the commit);
    // a comb/const next commits directly
    val regIdx = nl.regIds.indices.toVector
    val stagedSet = regIdx.filter(i => nl.opcodes(nl.regNextIds(i)) == Op.REG).toSet
    val gatherSlices = regIdx.filter(stagedSet).grouped(400).toVector
    val commitSlices = regIdx.grouped(350).toVector
    val dirtySlices = regIdx.grouped(150).toVector
    val hasRegs = regIdx.nonEmpty

    val simpleName = className.split('.').last
    val pkg = className.stripSuffix("." + simpleName)
    val sb = new StringBuilder
    sb ++= s"package $pkg;\n\n"
    sb ++= s"public final class $simpleName implements dfhdl.sim.SimKernel {\n"
    for (table, i) <- nl.romTables.zipWithIndex do
      sb ++= s"  private static final long[] ROM$i = { ${table.map(hexL).mkString(", ")} };\n"
    if !single && hasRegs then
      sb ++= s"  private final long[] nxt = new long[${nl.regIds.length}];\n"

    def emitCombCalls(indent: String): Unit =
      if single then
        for id <- comb do
          sb ++= emitOp(id, 0)
          sb += '\n'
      else for ci <- chunks.indices do sb ++= s"${indent}chunk$ci(sig);\n"

    def emitCommitCalls(indent: String): Unit =
      for gi <- gatherSlices.indices do sb ++= s"${indent}gather$gi(sig, nxt);\n"
      for ci <- commitSlices.indices do sb ++= s"${indent}commit$ci(sig, nxt);\n"

    def emitRunMethod(watch: Boolean): Unit =
      sb ++= (
        if watch then "\n  public long runWatch(long[] sig, long cycles, int watch) {\n"
        else "\n  public void run(long[] sig, long cycles) {\n"
      )
      if !single && hasRegs then sb ++= "    final long[] nxt = this.nxt;\n"
      sb ++= "    for (long cyc = 0L; cyc < cycles; cyc++) {\n"
      emitCombCalls("      ")
      if single then
        // two-phase commit: read every next value before any register slot is overwritten,
        // otherwise register-to-register chains (shift registers) cascade within one cycle
        for (n, i) <- nl.regNextIds.zipWithIndex do
          sb ++= s"      long nxt$i = ${rd(n, 0)};\n"
        for (r, i) <- nl.regIds.zipWithIndex do
          sb ++= s"      sig[$r] = nxt$i;\n"
      else emitCommitCalls("      ")
      // watched run: exit after any cycle whose settled watch value is nonzero (the watch node
      // is spilled to the signal array via the observed set)
      if watch then sb ++= "      if (sig[watch] != 0L) return cyc + 1L;\n"
      sb ++= "    }\n"
      if watch then sb ++= "    return cycles;\n"
      sb ++= "  }\n"
    end emitRunMethod

    emitRunMethod(watch = false)
    emitRunMethod(watch = true)
    // comb-only sweep (no register commit) for settle-on-peek
    sb ++= "\n  public void settle(long[] sig) {\n"
    emitCombCalls("    ")
    sb ++= "  }\n"
    // one tracked-commit cycle for the scheduler's quiescence probe
    sb ++= "\n  public boolean stepDirty(long[] sig) {\n"
    if single then
      emitCombCalls("    ")
      sb ++= "    boolean dirty = false;\n"
      for (n, i) <- nl.regNextIds.zipWithIndex do
        sb ++= s"    long nxt$i = ${rd(n, 0)};\n"
      for (r, i) <- nl.regIds.zipWithIndex do
        if nl.regTracked(i) then sb ++= s"    if (sig[$r] != nxt$i) dirty = true;\n"
        sb ++= s"    sig[$r] = nxt$i;\n"
      sb ++= "    return dirty;\n  }\n"
    else
      if hasRegs then sb ++= "    final long[] nxt = this.nxt;\n"
      emitCombCalls("    ")
      for gi <- gatherSlices.indices do sb ++= s"    gather$gi(sig, nxt);\n"
      sb ++= "    boolean dirty = false;\n"
      for di <- dirtySlices.indices do
        sb ++= s"    dirty = commitDirty$di(sig, nxt) | dirty;\n"
      sb ++= "    return dirty;\n  }\n"
    end if
    if !single then
      for (chunk, ci) <- chunks.zipWithIndex do
        sb ++= s"\n  private static void chunk$ci(long[] sig) {\n"
        for id <- chunk do
          sb ++= emitOp(id, ci)
          sb += '\n'
        sb ++= "  }\n"
      for (slice, gi) <- gatherSlices.zipWithIndex do
        sb ++= s"\n  private static void gather$gi(long[] sig, long[] nxt) {\n"
        for i <- slice do sb ++= s"    nxt[$i] = ${rd(nl.regNextIds(i), -1)};\n"
        sb ++= "  }\n"
      for (slice, ci) <- commitSlices.zipWithIndex do
        sb ++= s"\n  private static void commit$ci(long[] sig, long[] nxt) {\n"
        for i <- slice do
          val v = if stagedSet(i) then s"nxt[$i]" else rd(nl.regNextIds(i), -1)
          sb ++= s"    sig[${nl.regIds(i)}] = $v;\n"
        sb ++= "  }\n"
      for (slice, di) <- dirtySlices.zipWithIndex do
        sb ++= s"\n  private static boolean commitDirty$di(long[] sig, long[] nxt) {\n"
        sb ++= "    boolean dirty = false;\n"
        for i <- slice do
          val r = nl.regIds(i)
          val v = if stagedSet(i) then s"nxt[$i]" else rd(nl.regNextIds(i), -1)
          if nl.regTracked(i) then
            sb ++= s"    long x$i = $v;\n"
            sb ++= s"    if (sig[$r] != x$i) dirty = true;\n"
            sb ++= s"    sig[$r] = x$i;\n"
          else sb ++= s"    sig[$r] = $v;\n"
        sb ++= "    return dirty;\n  }\n"
    end if
    sb ++= "}\n"
    sb.result()
  end generateJava

  private def compileJava(className: String, source: String): Class[?] =
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
    val task =
      compiler.getTask(
        null,
        fm,
        diags,
        java.util.List.of("-classpath", cp),
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
