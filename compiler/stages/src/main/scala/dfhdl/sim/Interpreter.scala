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
    require(regNext.forall(_ >= 0), "register without a next value")
    new Kernel(code, masks, nodeWidths, roms, regOut, regNext)
  end compile

  private final class Kernel(
      code: Array[Int],
      masks: Array[Long],
      nodeWidths: Array[Int],
      roms: Array[Array[Long]],
      regOut: Array[Int],
      regNext: Array[Int]
  ) extends SimKernel:
    // commit is two-phase: read all next values before any register slot is overwritten,
    // otherwise register-to-register chains (shift registers) cascade within one cycle
    private val commitTmp = new Array[Long](regOut.length)
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
        cyc += 1
      end while
    end run

    def settle(sig: Array[Long]): Unit =
      val code = this.code
      val masks = this.masks
      val nodeWidths = this.nodeWidths
      val roms = this.roms
      val codeLen = code.length
      var i = 0
      while i < codeLen do
        val dst = code(i + 1)
        val a = code(i + 2)
        val b = code(i + 3)
        (code(i): @switch) match
          case 2 => sig(dst) = (sig(a) + sig(b)) & masks(dst) // ADD
          case 3 => sig(dst) = sig(a) ^ sig(b) // XOR
          case 4 => sig(dst) = sig(a) & sig(b) // AND
          case 5 => sig(dst) = sig(a) | sig(b) // OR
          case 6 => sig(dst) = ~sig(a) & masks(dst) // NOT
          case 7 => // ROTR
            val x = sig(a)
            sig(dst) = ((x >>> b) | (x << (nodeWidths(dst) - b))) & masks(dst)
          case 8  => sig(dst) = (sig(a) << b) & masks(dst) // SHL
          case 9  => sig(dst) = sig(a) >>> b // SHR
          case 10 => sig(dst) = if sig(a) != 0L then sig(b) else sig(code(i + 4)) // MUX
          case 11 => // ROM (pow2 table, masked address)
            val table = roms(b)
            sig(dst) = table((sig(a) & (table.length - 1)).toInt)
          case 12 => sig(dst) = sig(a) & masks(dst) // RESIZE
          case 13 => sig(dst) = if sig(a) == sig(b) then 1L else 0L // EQ
          case 14 => sig(dst) = if sig(a) != sig(b) then 1L else 0L // NEQ
          case 15 => sig(dst) = sig(a) // MOV
          case 16 => sig(dst) = (sig(a) - sig(b)) & masks(dst) // SUB
          case 17 => // SHLV
            val n = sig(b)
            sig(dst) = if n >= 64L then 0L else (sig(a) << n) & masks(dst)
          case 18 => // SHRV
            val n = sig(b)
            sig(dst) = if n >= 64L then 0L else sig(a) >>> n
          case 19 => // SRAV (sign-extend from the operand width, then arithmetic shift)
            val s = 64 - nodeWidths(dst)
            val n = math.min(sig(b), 63L)
            sig(dst) = (((sig(a) << s) >> s) >> n) & masks(dst)
          case 20 => // ULT
            sig(dst) = if java.lang.Long.compareUnsigned(sig(a), sig(b)) < 0 then 1L else 0L
          case 21 => // SLT
            val s = 64 - nodeWidths(a)
            sig(dst) = if ((sig(a) << s) >> s) < ((sig(b) << s) >> s) then 1L else 0L
          case _ => throw new IllegalStateException(s"bad opcode ${code(i)}")
        end match
        i += 5
      end while
    end settle
  end Kernel
end Interpreter
