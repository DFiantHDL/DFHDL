package dfhdl.sim

import scala.annotation.switch

/** Interpreter tier: pre-scheduled op-array evaluation.
  *
  * The netlist is flattened into a compact `Array[Int]` instruction stream (5 ints per op: opcode,
  * dst, a, b, c) evaluated in a tight loop with a table-switch dispatch — no objects, no
  * megamorphic calls, no allocation on the hot path. Registers commit after the sweep.
  */
object Interpreter:
  def compile(nl: Netlist): SimKernel =
    val comb = nl.combNodeIds
    val code = new Array[Int](comb.length * 5)
    var j = 0
    for id <- comb do
      code(j) = nl.opcodes(id)
      code(j + 1) = id
      code(j + 2) = nl.inA(id)
      code(j + 3) = nl.inB(id)
      code(j + 4) = nl.inC(id)
      j += 5
    val masks = Array.tabulate(nl.nodeCount)(nl.maskOf)
    val nodeWidths = nl.widths.toArray
    val roms = nl.romTables.toArray
    val regOut = nl.regIds.toArray
    val regNext = nl.regNextIds.toArray
    val regTracked = nl.regTracked.toArray
    require(regNext.forall(_ >= 0), "register without a next value")
    val memDepth = nl.memInit.iterator.map(_.length).toArray
    val memWrites = nl.memWrites.toArray
    val combDepth = nl.combDepth.toArray
    // ASTORE array id by node id (its three input slots are all nodes, no room for the immediate)
    val storeArrOf = Array.fill(nl.nodeCount)(-1)
    nl.storeArrId.foreach((id, a) => storeArrOf(id) = a)
    new Kernel(code, masks, nodeWidths, roms, regOut, regNext, regTracked, memDepth, memWrites,
      combDepth, storeArrOf)
  end compile

  private final class Kernel(
      code: Array[Int],
      masks: Array[Long],
      nodeWidths: Array[Int],
      roms: Array[Array[Long]],
      regOut: Array[Int],
      regNext: Array[Int],
      regTracked: Array[Boolean],
      memDepth: Array[Int],
      memWrites: Array[MemWrite],
      combDepth: Array[Int],
      storeArrOf: Array[Int]
  ) extends SimKernel:
    // sweep-local combinational scratch arrays (cleared each sweep by ANEW; never committed)
    private val combArr: Array[Array[Long]] =
      Array.tabulate(combDepth.length)(k => new Array[Long](combDepth(k)))
    // commit is two-phase: read all next values before any register slot is overwritten,
    // otherwise register-to-register chains (shift registers) cascade within one cycle
    private val commitTmp = new Array[Long](regOut.length)
    // kernel-owned memory backing store: one long per word (masked at write). Reads (Op.MEMRD)
    // observe the pre-commit contents; write ports apply after the sweep, like registers.
    private val mem: Array[Array[Long]] = Array.tabulate(memDepth.length)(k => new Array[Long](memDepth(k)))
    // memory-less designs skip the commit entirely: a final-false branch the JIT folds out, so the
    // per-cycle path is identical to before the memory node existed (no added call or loop)
    private val hasMem = memWrites.length > 0
    override def initMem(mems: Array[Array[Long]]): Unit =
      var k = 0
      while k < mem.length do
        System.arraycopy(mems(k), 0, mem(k), 0, mem(k).length)
        k += 1

    /** Apply the memory write ports of the just-swept cycle in registration order (last-write-wins
      * per bit). Reads the settled comb values, so it runs in the commit phase alongside registers.
      */
    private def commitMem(sig: Array[Long]): Unit =
      val mem = this.mem
      val writes = this.memWrites
      var k = 0
      while k < writes.length do
        val w = writes(k)
        if sig(w.we) != 0L then
          val arr = mem(w.mid)
          val idx = sig(w.addr).toInt
          if idx >= 0 && idx < arr.length then
            arr(idx) = (arr(idx) & ~w.mask) | ((sig(w.data) << w.pos) & w.mask)
        k += 1
    end commitMem

    /** As [[commitMem]], but reports whether any write actually changed a word (the scheduler's
      * quiescence probe: a live memory write is design activity, not a fixpoint).
      */
    private def commitMemDirty(sig: Array[Long]): Boolean =
      val mem = this.mem
      val writes = this.memWrites
      var dirty = false
      var k = 0
      while k < writes.length do
        val w = writes(k)
        if sig(w.we) != 0L then
          val arr = mem(w.mid)
          val idx = sig(w.addr).toInt
          if idx >= 0 && idx < arr.length then
            val nv = (arr(idx) & ~w.mask) | ((sig(w.data) << w.pos) & w.mask)
            if nv != arr(idx) then dirty = true
            arr(idx) = nv
        k += 1
      dirty
    end commitMemDirty
    def run(sig: Array[Long], cycles: Long): Unit =
      val regOut = this.regOut
      val regNext = this.regNext
      val regCount = regOut.length
      var cyc = 0L
      while cyc < cycles do
        settle(sig)
        var r = 0
        while r < regCount do
          commitTmp(r) = sig(regNext(r))
          r += 1
        r = 0
        while r < regCount do
          sig(regOut(r)) = commitTmp(r)
          r += 1
        if hasMem then commitMem(sig)
        cyc += 1
      end while
    end run

    def runWatch(sig: Array[Long], cycles: Long, watch: Int): Long =
      val regOut = this.regOut
      val regNext = this.regNext
      val regCount = regOut.length
      var cyc = 0L
      var fired = false
      while cyc < cycles && !fired do
        settle(sig)
        var r = 0
        while r < regCount do
          commitTmp(r) = sig(regNext(r))
          r += 1
        r = 0
        while r < regCount do
          sig(regOut(r)) = commitTmp(r)
          r += 1
        if hasMem then commitMem(sig)
        cyc += 1
        if sig(watch) != 0L then fired = true
      end while
      cyc
    end runWatch

    def stepDirty(sig: Array[Long]): Boolean =
      settle(sig)
      val regOut = this.regOut
      val regNext = this.regNext
      val regCount = regOut.length
      var r = 0
      while r < regCount do
        commitTmp(r) = sig(regNext(r))
        r += 1
      var dirty = false
      r = 0
      while r < regCount do
        val out = regOut(r)
        if regTracked(r) && sig(out) != commitTmp(r) then dirty = true
        sig(out) = commitTmp(r)
        r += 1
      if hasMem && commitMemDirty(sig) then dirty = true
      dirty
    end stepDirty

    def settle(sig: Array[Long]): Unit =
      val code = this.code
      val masks = this.masks
      val nodeWidths = this.nodeWidths
      val roms = this.roms
      val mem = this.mem
      val combArr = this.combArr
      val storeArrOf = this.storeArrOf
      val codeLen = code.length
      var i = 0
      while i < codeLen do
        val dst = code(i + 1)
        val a = code(i + 2)
        val b = code(i + 3)
        (code(i): @switch) match
          case Op.MOV    => sig(dst) = sig(a)
          case Op.NOT    => sig(dst) = ~sig(a) & masks(dst)
          case Op.RESIZE => sig(dst) = sig(a) & masks(dst)
          case Op.REV    => sig(dst) = java.lang.Long.reverse(sig(a)) >>> (64 - nodeWidths(dst))
          case Op.SHL    => sig(dst) = (sig(a) << b) & masks(dst)
          case Op.SHR    => sig(dst) = sig(a) >>> b
          case Op.ROTR   =>
            val x = sig(a)
            sig(dst) = ((x >>> b) | (x << (nodeWidths(dst) - b))) & masks(dst)
          case Op.ROM => // pow2 table, masked address
            val table = roms(b)
            sig(dst) = table((sig(a) & (table.length - 1)).toInt)
          case Op.ADD  => sig(dst) = (sig(a) + sig(b)) & masks(dst)
          case Op.SUB  => sig(dst) = (sig(a) - sig(b)) & masks(dst)
          case Op.MUL  => sig(dst) = (sig(a) * sig(b)) & masks(dst)
          case Op.UDIV => // x/0 = 0
            sig(dst) = if sig(b) == 0L then 0L else java.lang.Long.divideUnsigned(sig(a), sig(b))
          case Op.SDIV => // x/0 = 0, operands sign-extended from their width
            val s = 64 - nodeWidths(dst)
            val bv = (sig(b) << s) >> s
            sig(dst) = if bv == 0L then 0L else (((sig(a) << s) >> s) / bv) & masks(dst)
          case Op.UREM => // x%0 = 0
            sig(dst) = if sig(b) == 0L then 0L
            else java.lang.Long.remainderUnsigned(sig(a), sig(b))
          case Op.SREM => // x%0 = 0, operands sign-extended from their width
            val s = 64 - nodeWidths(dst)
            val bv = (sig(b) << s) >> s
            sig(dst) = if bv == 0L then 0L else (((sig(a) << s) >> s) % bv) & masks(dst)
          case Op.AND => sig(dst) = sig(a) & sig(b)
          case Op.OR  => sig(dst) = sig(a) | sig(b)
          case Op.XOR => sig(dst) = sig(a) ^ sig(b)
          case Op.EQ  => sig(dst) = if sig(a) == sig(b) then 1L else 0L
          case Op.NEQ => sig(dst) = if sig(a) != sig(b) then 1L else 0L
          case Op.ULT =>
            sig(dst) = if java.lang.Long.compareUnsigned(sig(a), sig(b)) < 0 then 1L else 0L
          case Op.SLT =>
            val s = 64 - nodeWidths(a)
            sig(dst) = if ((sig(a) << s) >> s) < ((sig(b) << s) >> s) then 1L else 0L
          case Op.SHLV =>
            val n = sig(b)
            sig(dst) = if n >= 64L then 0L else (sig(a) << n) & masks(dst)
          case Op.SHRV =>
            val n = sig(b)
            sig(dst) = if n >= 64L then 0L else sig(a) >>> n
          case Op.SRAV => // sign-extend from the operand width, then arithmetic shift
            val s = 64 - nodeWidths(dst)
            val n = math.min(sig(b), 63L)
            sig(dst) = (((sig(a) << s) >> s) >> n) & masks(dst)
          case Op.MUX   => sig(dst) = if sig(a) != 0L then sig(b) else sig(code(i + 4))
          case Op.MEMRD => // async read: b is the memory id, a the address (out-of-range reads 0)
            val arr = mem(b)
            val idx = sig(a).toInt
            sig(dst) = if idx >= 0 && idx < arr.length then arr(idx) else 0L
          case Op.ANEW => // clear the comb array (b = array id) at the start of its sweep
            java.util.Arrays.fill(combArr(b), 0L)
            sig(dst) = 0L
          case Op.ALOAD => // arr[a]; b = array id, ordering via the version input
            val arr = combArr(b)
            val idx = sig(a).toInt
            sig(dst) = if idx >= 0 && idx < arr.length then arr(idx) else 0L
          case Op.ASTORE => // arr[a] = data(b); array id from the side table
            val arr = combArr(storeArrOf(dst))
            val idx = sig(a).toInt
            if idx >= 0 && idx < arr.length then arr(idx) = sig(b)
            sig(dst) = 0L
          case _ => throw new IllegalStateException(s"bad opcode ${code(i)}")
        end match
        i += 5
      end while
    end settle
  end Kernel
end Interpreter
