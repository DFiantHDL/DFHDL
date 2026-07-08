package dfhdl.sim
import dfhdl.*

/** Combinational wide-op DUT: exercises the lane-decomposition paths on >64-bit values -
  * carry/borrow chains (`+`/`+^`/`-`), lane-wise logic, lexicographic compares (signed and
  * unsigned), dynamic shifts through the barrel network (logical and arithmetic), concatenation,
  * lane-crossing slice/bit selection, and sign extension.
  */
class WideOpsDut extends RTDesign:
  val a = UInt(100) <> IN
  val b = UInt(100) <> IN
  val sa = SInt(72) <> IN
  val sb = SInt(72) <> IN
  val shamt = UInt(7) <> IN
  val sum = UInt(100) <> OUT
  val carrySum = UInt(101) <> OUT
  val diff = UInt(100) <> OUT
  val bxor = Bits(100) <> OUT
  val band = Bits(100) <> OUT
  val bnot = Bits(100) <> OUT
  val ltu = Bit <> OUT
  val lts = Bit <> OUT
  val equ = Bit <> OUT
  val shlDynOut = UInt(100) <> OUT
  val shrDynOut = UInt(100) <> OUT
  val sshrDynOut = SInt(72) <> OUT
  val catOut = Bits(172) <> OUT
  val sliceOut = Bits(33) <> OUT
  val bitSel = Bit <> OUT
  val sextOut = SInt(100) <> OUT
  sum := a + b
  carrySum := a +^ b
  diff := a - b
  bxor := a.bits ^ b.bits
  band := a.bits & b.bits
  bnot := ~a.bits
  ltu := a < b
  lts := sa < sb
  equ := a == b
  shlDynOut := a << shamt
  shrDynOut := a >> shamt
  sshrDynOut := sa >> shamt
  catOut := a.bits ++ sa.bits
  sliceOut := a.bits(80, 48)
  bitSel := a.bits(64)
  sextOut := sa.resize(100)
end WideOpsDut

/** Wide register: carry propagation across lanes through committed REG state. */
class WideAcc extends RTDesign:
  val add = UInt(128) <> IN
  val acc = UInt(128) <> OUT.REG init 0
  acc.din := acc + add

/** Partial-assignment DUT: RMW range/bit updates on a wide Bits variable, with the range crossing a
  * lane boundary.
  */
class WidePartialDut extends RTDesign:
  val base = Bits(96) <> IN
  val fld = Bits(20) <> IN
  val b0 = Bit <> IN
  val out = Bits(96) <> OUT
  val v = Bits(96) <> VAR
  v := base
  v(70, 51) := fld
  v(95) := b0
  out := v
end WidePartialDut

/** Narrow arithmetic coverage the wide DUT cannot host: multiplication, unsigned/signed division
  * and remainder (single-lane kernel ops), and bits repetition.
  */
class ArithMiscDut extends RTDesign:
  val x = UInt(32) <> IN
  val y = UInt(32) <> IN
  val sx = SInt(32) <> IN
  val sy = SInt(32) <> IN
  val prod = UInt(32) <> OUT
  val quot = UInt(32) <> OUT
  val rem = UInt(32) <> OUT
  val squot = SInt(32) <> OUT
  val srem = SInt(32) <> OUT
  val rep = Bits(96) <> OUT
  prod := x * y
  quot := x / y
  rem := x % y
  squot := sx / sy
  srem := sx % sy
  rep := x.bits.repeat(3)
end ArithMiscDut
