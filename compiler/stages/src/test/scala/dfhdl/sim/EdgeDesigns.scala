package dfhdl.sim
import dfhdl.*

/** Width-parameterized unsigned edge DUT: overflow/underflow behavior of the carry/borrow lowering
  * at and around the 64-bit lane size (W = 64 full-lane mask, W = 65 one-bit top lane, W = 128
  * multi-lane ripple).
  */
class UIntEdgeDut(val W: Int <> CONST) extends RTDesign:
  val a = UInt(W) <> IN
  val b = UInt(W) <> IN
  val sum = UInt(W) <> OUT
  val carrySum = UInt(W + 1) <> OUT
  val diff = UInt(W) <> OUT
  val ltu = Bit <> OUT
  val equ = Bit <> OUT
  sum := a + b
  carrySum := a +^ b
  diff := a - b
  ltu := a < b
  equ := a == b
end UIntEdgeDut

/** Width-parameterized signed edge DUT: two's-complement wraparound at the type extremes (MAX+1 ->
  * MIN, MIN-1 -> MAX, -MIN -> MIN) and signed comparison across lane boundaries.
  */
class SIntEdgeDut(val W: Int <> CONST) extends RTDesign:
  val a = SInt(W) <> IN
  val b = SInt(W) <> IN
  val sum = SInt(W) <> OUT
  val diff = SInt(W) <> OUT
  val negOut = SInt(W) <> OUT
  val lts = Bit <> OUT
  sum := a + b
  diff := a - b
  negOut := -a
  lts := a < b
end SIntEdgeDut

/** Fixed 64-bit DUT for the scalar-kernel edge paths on a full lane: multiplication overflow, the
  * signed-division overflow case (MIN / -1), unsigned division with the MSB set, dynamic shifts at
  * and above the lane size, and full-mask inversion.
  */
class Lane64EdgeDut extends RTDesign:
  val a = UInt(64) <> IN
  val b = UInt(64) <> IN
  val sa = SInt(64) <> IN
  val sb = SInt(64) <> IN
  val shamt = UInt(6) <> IN // DFHDL caps dynamic shift amounts at clog2(width) bits
  val prod = UInt(64) <> OUT
  val sprod = SInt(64) <> OUT
  val quot = UInt(64) <> OUT
  val squot = SInt(64) <> OUT
  val srem = SInt(64) <> OUT
  val shlOut = UInt(64) <> OUT
  val shrOut = UInt(64) <> OUT
  val sshrOut = SInt(64) <> OUT
  val notOut = Bits(64) <> OUT
  prod := a * b
  sprod := sa * sb
  quot := a / b
  squot := sa / sb
  srem := sa % sb
  shlOut := a << shamt
  shrOut := a >> shamt
  sshrOut := sa >> shamt
  notOut := ~a.bits
end Lane64EdgeDut
