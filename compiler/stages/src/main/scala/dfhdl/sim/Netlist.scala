package dfhdl.sim

import scala.collection.mutable

/** Opcodes for the mockup netlist. Values are stable literals so the interpreter tier can dispatch
  * on them with a table switch.
  */
object Op:
  inline val REG = 0
  inline val CONST = 1
  inline val ADD = 2
  inline val XOR = 3
  inline val AND = 4
  inline val OR = 5
  inline val NOT = 6
  inline val ROTR = 7
  inline val SHL = 8
  inline val SHR = 9
  inline val MUX = 10
  inline val ROM = 11
  inline val RESIZE = 12 // zero-extend or truncate to the destination width
  inline val EQ = 13
  inline val NEQ = 14
  inline val MOV = 15 // patchable identity, for forward references (e.g. port connections)
  inline val SUB = 16
  inline val SHLV = 17 // shift left by a dynamic (node) amount
  inline val SHRV = 18 // logical shift right by a dynamic (node) amount
  inline val SRAV = 19 // arithmetic shift right by a dynamic (node) amount
  inline val ULT = 20 // unsigned less-than
  inline val SLT = 21 // signed less-than (operands sign-extended from their width)
end Op

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
  private[sim] val regIds = mutable.ArrayBuffer.empty[Int]
  private[sim] val regNextIds = mutable.ArrayBuffer.empty[Int]

  def nodeCount: Int = opcodes.length
  def widthOf(id: Int): Int = widths(id)
  def maskOf(id: Int): Long = maskFor(widths(id))
  private def maskFor(w: Int): Long = if w == 64 then -1L else (1L << w) - 1

  private def newNode(op: Int, w: Int, a: Int = -1, b: Int = -1, c: Int = -1, init: Long = 0L)
      : Int =
    require(w >= 1 && w <= 64, s"unsupported width $w")
    opcodes += op; inA += a; inB += b; inC += c; widths += w; initVals += init
    opcodes.length - 1

  def const(w: Int, v: Long): Int = newNode(Op.CONST, w, init = v & maskFor(w))

  def reg(w: Int, init: Long): Int =
    val id = newNode(Op.REG, w, init = init & maskFor(w))
    regIds += id
    regNextIds += -1
    id

  def setNext(regId: Int, nextId: Int): Unit =
    val idx = regIds.indexOf(regId)
    require(idx >= 0, s"node $regId is not a register")
    require(regNextIds(idx) == -1, s"register $regId already has a next value")
    require(widths(regId) == widths(nextId), "width mismatch on register next")
    regNextIds(idx) = nextId

  private def bin(op: Int, a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on binary op")
    newNode(op, widths(a), a, b)

  def add(a: Int, b: Int): Int = bin(Op.ADD, a, b)
  def sub(a: Int, b: Int): Int = bin(Op.SUB, a, b)
  def xor(a: Int, b: Int): Int = bin(Op.XOR, a, b)
  def and(a: Int, b: Int): Int = bin(Op.AND, a, b)
  def or(a: Int, b: Int): Int = bin(Op.OR, a, b)
  def not(a: Int): Int = newNode(Op.NOT, widths(a), a)

  def rotr(a: Int, n: Int): Int =
    require(n > 0 && n < widths(a), "rotate amount out of range")
    newNode(Op.ROTR, widths(a), a, n)

  def shl(a: Int, n: Int): Int =
    require(n >= 0 && n < widths(a), "shift amount out of range")
    newNode(Op.SHL, widths(a), a, n)

  def shr(a: Int, n: Int): Int =
    require(n >= 0 && n < widths(a), "shift amount out of range")
    newNode(Op.SHR, widths(a), a, n)

  def mux(sel: Int, t: Int, f: Int): Int =
    require(widths(t) == widths(f), "width mismatch on mux branches")
    newNode(Op.MUX, widths(t), sel, t, f)

  def eq(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    newNode(Op.EQ, 1, a, b)

  def neq(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    newNode(Op.NEQ, 1, a, b)

  def ult(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    newNode(Op.ULT, 1, a, b)

  def slt(a: Int, b: Int): Int =
    require(widths(a) == widths(b), "width mismatch on comparison")
    newNode(Op.SLT, 1, a, b)

  def shlv(a: Int, amount: Int): Int = newNode(Op.SHLV, widths(a), a, amount)
  def shrv(a: Int, amount: Int): Int = newNode(Op.SHRV, widths(a), a, amount)
  def srav(a: Int, amount: Int): Int = newNode(Op.SRAV, widths(a), a, amount)

  /** Placeholder for a value not yet known (forward reference); patch with [[patchMov]]. */
  def mov(w: Int): Int = newNode(Op.MOV, w)

  def patchMov(movId: Int, srcId: Int): Unit =
    require(opcodes(movId) == Op.MOV && inA(movId) == -1, s"node $movId is not an unpatched MOV")
    require(widths(movId) == widths(srcId), "width mismatch on MOV patch")
    inA(movId) = srcId

  def resize(a: Int, w: Int): Int =
    if w == widths(a) then a else newNode(Op.RESIZE, w, a)

  def rom(table: Array[Long], w: Int, addr: Int): Int =
    // pow2 table size so the address can be masked instead of bounds-checked
    require(
      table.nonEmpty && (table.length & (table.length - 1)) == 0,
      "ROM table length must be a power of 2"
    )
    romTables += table.map(_ & maskFor(w))
    newNode(Op.ROM, w, addr, romTables.length - 1)

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

  /** Node-id inputs of a combinational node (excludes immediates and ROM table ids). */
  private[sim] def nodeInputs(id: Int): List[Int] = opcodes(id) match
    case Op.ADD | Op.SUB | Op.XOR | Op.AND | Op.OR | Op.EQ | Op.NEQ | Op.ULT | Op.SLT |
        Op.SHLV | Op.SHRV | Op.SRAV =>
      List(inA(id), inB(id))
    case Op.NOT | Op.ROTR | Op.SHL | Op.SHR | Op.ROM | Op.RESIZE | Op.MOV => List(inA(id))
    case Op.MUX => List(inA(id), inB(id), inC(id))
    case _      => Nil
end Netlist
