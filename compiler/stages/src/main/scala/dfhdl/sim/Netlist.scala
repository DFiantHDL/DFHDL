package dfhdl.sim

import scala.collection.mutable

/** Opcodes for the mockup netlist. Values are stable literals so the interpreter tier can dispatch
  * on them with a table switch, grouped contiguously by arity so the number of node arguments falls
  * out of two range checks (see [[Netlist.nodeInputs]]).
  */
object Op:
  // sources — no node inputs, no per-cycle evaluation
  inline val REG = 0
  inline val CONST = 1
  // unary ops — the single node input is in `inA` (`inB` holds an immediate / ROM table id)
  inline val MOV = 2 // patchable identity, for forward references (e.g. port connections)
  inline val NOT = 3
  inline val RESIZE = 4 // zero-extend or truncate to the destination width
  inline val REV = 5 // bit reversal within the destination width
  inline val SHL = 6 // shift left by an immediate amount
  inline val SHR = 7 // logical shift right by an immediate amount
  inline val ROTR = 8 // rotate right by an immediate amount
  inline val ROM = 9 // constant-table read (`inB` is the table id)
  // binary ops — node inputs in `inA`/`inB`
  inline val ADD = 10
  inline val SUB = 11
  inline val MUL = 12 // multiplication (low bits — correct for both signednesses)
  inline val UDIV = 13 // unsigned division (division by zero yields 0)
  inline val SDIV = 14 // signed division, operands sign-extended from their width
  inline val UREM = 15 // unsigned remainder (division by zero yields 0)
  inline val SREM = 16 // signed remainder, operands sign-extended from their width
  inline val AND = 17
  inline val OR = 18
  inline val XOR = 19
  inline val EQ = 20
  inline val NEQ = 21
  inline val ULT = 22 // unsigned less-than
  inline val SLT = 23 // signed less-than (operands sign-extended from their width)
  inline val SHLV = 24 // shift left by a dynamic (node) amount
  inline val SHRV = 25 // logical shift right by a dynamic (node) amount
  inline val SRAV = 26 // arithmetic shift right by a dynamic (node) amount
  // ternary ops — node inputs in `inA`/`inB`/`inC`
  inline val MUX = 27
  // memory read — async: `inA` is the address node, `inB` holds the memory id (an immediate). The
  // one node input is the address, so this op is handled outside the arity grouping below.
  inline val MEMRD = 28
  // combinational scratch array (a cell-wise combinational vector), sweep-local. Ops are version-
  // threaded so the scheduler orders writes before same-sweep reads. All handled outside the arity
  // grouping below.
  inline val ANEW = 29 // clear the array to zero; `inB` = array id. Yields the version-0 token.
  inline val ALOAD = 30 // `arr[inA]`; `inB` = array id, `inC` = version (ordering input)
  inline val ASTORE = 31 // `arr[inA] = inB`; `inC` = version. Yields a new version token.
  // arity-group boundaries
  inline val unaryFirst = MOV
  inline val unaryLast = ROM
  inline val binaryFirst = ADD
  inline val binaryLast = SRAV
end Op

/** One synchronous memory write port: `if (we) mem[addr] = (mem[addr] & ~mask) | ((data << pos) &
  * mask)`. Ports for a memory are applied in registration order after the combinational sweep (so a
  * byte-enable write is a set of ports over one word, last-write-wins per bit); reads observe the
  * pre-commit contents (read-first, matching a non-blocking `<=` RAM).
  */
private[sim] final case class MemWrite(mid: Int, addr: Int, data: Int, we: Int, pos: Int, mask: Long)

/** A tiny pre-scheduled netlist — the SimGraph precursor the DFacsimile lowering targets. Nodes are
  * identified by their index. Combinational evaluation order is computed by a topological schedule
  * over the data dependencies (construction order is the tie-breaker), so forward references
  * through [[Op.MOV]] placeholders (patched after construction, e.g. hierarchy port connections)
  * are legal. Registers follow two-phase semantics: all combinational nodes evaluate, then all
  * registers commit their `next` value simultaneously.
  */
final class Netlist:
  private[sim] val opcodes = mutable.ArrayBuffer.empty[Int]
  private[sim] val inA = mutable.ArrayBuffer.empty[Int]
  private[sim] val inB =
    mutable.ArrayBuffer.empty[Int] // node id, shift amount, or ROM table id
  private[sim] val inC = mutable.ArrayBuffer.empty[Int]
  private[sim] val widths = mutable.ArrayBuffer.empty[Int]
  private[sim] val initVals =
    mutable.ArrayBuffer.empty[Long] // CONST value / REG reset value
  private[sim] val romTables = mutable.ArrayBuffer.empty[Array[Long]]
  // memories: backing words (masked to the word width) and their ordered synchronous write ports.
  // A memory read is an [[Op.MEMRD]] node (async, O(1)); reads and writes address it by memory id.
  private[sim] val memInit = mutable.ArrayBuffer.empty[Array[Long]]
  private[sim] val memWordW = mutable.ArrayBuffer.empty[Int]
  private[sim] val memWrites = mutable.ArrayBuffer.empty[MemWrite]
  // combinational scratch arrays (cell-wise combinational vectors): depth per array, and the array
  // id of each ASTORE node (its writes have no room for the id in the three input slots)
  private[sim] val combDepth = mutable.ArrayBuffer.empty[Int]
  private[sim] val storeArrId = mutable.HashMap.empty[Int, Int]
  private[sim] val regIds = mutable.ArrayBuffer.empty[Int]
  private[sim] val regNextIds = mutable.ArrayBuffer.empty[Int]
  // change tracking classification: untracked registers (e.g. wait counters) are excluded from
  // the kernels' `stepDirty` dirtiness report, so pure time-keeping does not defeat the
  // scheduler's quiescence detection
  private[sim] val regTracked = mutable.ArrayBuffer.empty[Boolean]
  private val constCache = mutable.HashMap.empty[(Int, Long), Int]

  def nodeCount: Int = opcodes.length
  def widthOf(id: Int): Int = widths(id)
  def maskOf(id: Int): Long = maskFor(widths(id))
  private def maskFor(w: Int): Long = SimOps.maskFor(w)

  def isConst(id: Int): Boolean = opcodes(id) == Op.CONST
  def constValOf(id: Int): Long =
    require(isConst(id), s"node $id is not a constant")
    initVals(id)

  private def newNode(op: Int, w: Int, a: Int = -1, b: Int = -1, c: Int = -1, init: Long = 0L)
      : Int =
    require(w >= 1 && w <= 64, s"unsupported width $w")
    opcodes += op; inA += a; inB += b; inC += c; widths += w; initVals += init
    opcodes.length - 1

  /** Evaluatable node (all node inputs constant) fold at build time through the same [[SimOps]]
    * semantics the interpreter kernel implements; MOV (patched later) and ROM (via [[rom]]) are
    * handled at their creation sites. Constant folding is what lets process-control guards prune
    * statically (e.g. a loop re-entry guard over a just-reset iterator).
    */
  private def evalNode(op: Int, w: Int, a: Int = -1, b: Int = -1, c: Int = -1): Int =
    inline def cv(id: Int): Long = initVals(id)
    val foldable = op match
      case Op.MOV => false
      case _      =>
        if op >= Op.binaryFirst then
          if op <= Op.binaryLast then isConst(a) && isConst(b)
          else isConst(a) && isConst(b) && isConst(c) // MUX (sel-const pruning is done in `mux`)
        else isConst(a) // unary; the immediate rides in `b`
    if foldable then
      val bArg = if op >= Op.binaryFirst then cv(b) else b.toLong
      val cArg = if op == Op.MUX then cv(c) else -1L
      const(w, SimOps.eval(op, w, widths(a), cv(a), bArg, cArg))
    else newNode(op, w, a, b, c)
  end evalNode

  def const(w: Int, v: Long): Int =
    val key = (w, v & maskFor(w))
    constCache.getOrElseUpdate(key, newNode(Op.CONST, w, init = key._2))

  def reg(w: Int, init: Long): Int =
    val id = newNode(Op.REG, w, init = init & maskFor(w))
    regIds += id
    regNextIds += -1
    regTracked += true
    id

  /** Overrides a register's time-zero value after creation (a post-hoc fold, e.g. the FSM
    * reset-site fold appending dispatch constants to the initial state).
    */
  def setRegInit(regId: Int, init: Long): Unit =
    require(opcodes(regId) == Op.REG, s"node $regId is not a register")
    initVals(regId) = init & maskFor(widths(regId))

  /** Exclude a register from `stepDirty` change tracking (scheduler-owned time-keeping cells). */
  def markUntracked(regId: Int): Unit =
    val idx = regIds.indexOf(regId)
    require(idx >= 0, s"node $regId is not a register")
    regTracked(idx) = false

  def setNext(regId: Int, nextId: Int): Unit =
    val idx = regIds.indexOf(regId)
    require(idx >= 0, s"node $regId is not a register")
    require(regNextIds(idx) == -1, s"register $regId already has a next value")
    require(widths(regId) == widths(nextId), "width mismatch on register next")
    regNextIds(idx) = nextId

  private def bin(op: Int, a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on binary op")
    evalNode(op, widths(a), a, b)

  def add(a: Int, b: Int): Int = bin(Op.ADD, a, b)
  def sub(a: Int, b: Int): Int = bin(Op.SUB, a, b)
  def xor(a: Int, b: Int): Int = bin(Op.XOR, a, b)
  def and(a: Int, b: Int): Int = bin(Op.AND, a, b)
  def or(a: Int, b: Int): Int = bin(Op.OR, a, b)
  def mul(a: Int, b: Int): Int = bin(Op.MUL, a, b)
  def udiv(a: Int, b: Int): Int = bin(Op.UDIV, a, b)
  def sdiv(a: Int, b: Int): Int = bin(Op.SDIV, a, b)
  def urem(a: Int, b: Int): Int = bin(Op.UREM, a, b)
  def srem(a: Int, b: Int): Int = bin(Op.SREM, a, b)
  def not(a: Int): Int = evalNode(Op.NOT, widths(a), a)

  /** Bit reversal of `a` within width `w` (>= the width of `a`; bits above `a`'s width read 0). */
  def rev(a: Int, w: Int): Int =
    require(w >= widths(a), "reversal width below the operand width")
    evalNode(Op.REV, w, a)

  def rotr(a: Int, n: Int): Int =
    require(n > 0 && n < widths(a), "rotate amount out of range")
    evalNode(Op.ROTR, widths(a), a, n)

  def shl(a: Int, n: Int): Int =
    require(n >= 0 && n < widths(a), "shift amount out of range")
    evalNode(Op.SHL, widths(a), a, n)

  def shr(a: Int, n: Int): Int =
    require(n >= 0 && n < widths(a), "shift amount out of range")
    evalNode(Op.SHR, widths(a), a, n)

  def mux(sel: Int, t: Int, f: Int): Int =
    require(widths(t) == widths(f), "width mismatch on mux branches")
    // a constant selector prunes to the chosen input (which need not be constant itself)
    if isConst(sel) then (if initVals(sel) != 0L then t else f)
    else evalNode(Op.MUX, widths(t), sel, t, f)

  def eq(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    evalNode(Op.EQ, 1, a, b)

  def neq(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    evalNode(Op.NEQ, 1, a, b)

  def ult(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    evalNode(Op.ULT, 1, a, b)

  def slt(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    evalNode(Op.SLT, 1, a, b)

  def shlv(a: Int, amount: Int): Int = evalNode(Op.SHLV, widths(a), a, amount)
  def shrv(a: Int, amount: Int): Int = evalNode(Op.SHRV, widths(a), a, amount)
  def srav(a: Int, amount: Int): Int = evalNode(Op.SRAV, widths(a), a, amount)

  /** Placeholder for a value not yet known (forward reference); patch with [[patchMov]]. */
  def mov(w: Int): Int = newNode(Op.MOV, w)

  /** A combinational snapshot of a register read: text-output actions fire after their cycle
    * commits, so register operands pass through a MOV whose swept value survives the commit.
    * Non-register nodes are already sweep-computed and pass through unchanged.
    */
  def snap(a: Int): Int =
    if opcodes(a) == Op.REG then newNode(Op.MOV, widths(a), a) else a

  def patchMov(movId: Int, srcId: Int): Unit =
    require(opcodes(movId) == Op.MOV && inA(movId) == -1, s"node $movId is not an unpatched MOV")
    require(widths(movId) == widths(srcId), "width mismatch on MOV patch")
    inA(movId) = srcId

  def resize(a: Int, w: Int): Int =
    if w == widths(a) then a else evalNode(Op.RESIZE, w, a)

  def rom(table: Array[Long], w: Int, addr: Int): Int =
    // pow2 table size so the address can be masked instead of bounds-checked
    require(
      table.nonEmpty && (table.length & (table.length - 1)) == 0,
      "ROM table length must be a power of 2"
    )
    val masked = table.map(_ & maskFor(w))
    if isConst(addr) then const(w, masked((initVals(addr) & (masked.length - 1)).toInt))
    else
      romTables += masked
      newNode(Op.ROM, w, addr, romTables.length - 1)

  def memCount: Int = memInit.length

  /** A new memory of `depth` words, each `wordW` (<= 64) bits, preloaded from `init` (one Long per
    * word, LSB-aligned). Returns the memory id used by [[memRead]]/[[memWrite]].
    */
  def newMem(depth: Int, wordW: Int, init: Array[Long]): Int =
    require(wordW >= 1 && wordW <= 64, s"unsupported memory word width $wordW")
    require(init.length == depth, s"memory init length ${init.length} != depth $depth")
    val m = maskFor(wordW)
    memInit += Array.tabulate(depth)(i => init(i) & m)
    memWordW += wordW
    memInit.length - 1

  /** Async read node: `mem[addr]` (out-of-range addresses read 0). */
  def memRead(mid: Int, addr: Int): Int =
    require(mid >= 0 && mid < memCount, s"no memory $mid")
    newNode(Op.MEMRD, memWordW(mid), a = addr, b = mid)

  /** Register a synchronous write port for `mem[addr]`: the `data` node's low `pos+`-shifted bits
    * replace the `mask` bits of the word when `we` is nonzero.
    */
  def memWrite(mid: Int, addr: Int, data: Int, we: Int, pos: Int, mask: Long): Unit =
    require(mid >= 0 && mid < memCount, s"no memory $mid")
    memWrites += MemWrite(mid, addr, data, we, pos, mask & maskFor(memWordW(mid)))

  def combCount: Int = combDepth.length

  /** A new sweep-local combinational scratch array of `depth` cells; returns its id. */
  def newCombArray(depth: Int): Int =
    combDepth += depth
    combDepth.length - 1

  /** Clear-and-version-zero node for a comb array (its evaluation zeroes the backing array). */
  def combNew(arrId: Int): Int =
    require(arrId >= 0 && arrId < combCount, s"no comb array $arrId")
    newNode(Op.ANEW, 1, b = arrId)

  /** `arr[addr]` read (`version` orders it after the writes it must observe). */
  def combLoad(arrId: Int, cellW: Int, addr: Int, version: Int): Int =
    require(arrId >= 0 && arrId < combCount, s"no comb array $arrId")
    newNode(Op.ALOAD, cellW, a = addr, b = arrId, c = version)

  /** `arr[addr] = data` write on top of `version`; yields the new version token. */
  def combStore(arrId: Int, addr: Int, data: Int, version: Int): Int =
    require(arrId >= 0 && arrId < combCount, s"no comb array $arrId")
    val id = newNode(Op.ASTORE, 1, a = addr, b = data, c = version)
    storeArrId(id) = arrId
    id

  /** Signal array with constants and register reset values pre-loaded. */
  def initialSig: Array[Long] =
    val sig = new Array[Long](nodeCount)
    var i = 0
    while i < nodeCount do
      val op = opcodes(i)
      if op == Op.CONST || op == Op.REG then sig(i) = initVals(i)
      i += 1
    sig

  /** Node ids that require per-cycle evaluation, topologically scheduled over the data dependencies
    * (id order as the tie-breaker; registers and constants are sources). Fails on combinational
    * cycles and unpatched MOV nodes.
    */
  def combNodeIds: Vector[Int] =
    val order = Vector.newBuilder[Int]
    // 0 = unvisited, 1 = in progress (on stack), 2 = done
    val state = new Array[Byte](nodeCount)
    def isComb(id: Int): Boolean = opcodes(id) != Op.REG && opcodes(id) != Op.CONST
    def visit(id: Int): Unit =
      if isComb(id) && state(id) != 2 then
        require(state(id) != 1, s"combinational cycle through node $id")
        if opcodes(id) == Op.MOV then require(inA(id) != -1, s"unpatched MOV node $id")
        state(id) = 1
        nodeInputs(id).foreach(visit)
        state(id) = 2
        order += id
    (0 until nodeCount).foreach(visit)
    order.result()
  end combNodeIds

  /** Node-id inputs of a combinational node (excludes immediates and ROM table ids). The opcode
    * grouping makes the arity a pair of range checks.
    */
  private[sim] def nodeInputs(id: Int): List[Int] =
    val op = opcodes(id)
    if op == Op.MEMRD then List(inA(id)) // address is the only node input (inB is the memory id)
    else if op == Op.ANEW then Nil // array clear; inB is the array id (immediate)
    else if op == Op.ALOAD then List(inA(id), inC(id)) // addr, version (inB is the array id)
    else if op == Op.ASTORE then List(inA(id), inB(id), inC(id)) // addr, data, version
    else if op >= Op.binaryFirst then
      if op <= Op.binaryLast then List(inA(id), inB(id))
      else List(inA(id), inB(id), inC(id)) // MUX
    else if op >= Op.unaryFirst then List(inA(id))
    else Nil // sources (REG/CONST)
end Netlist
