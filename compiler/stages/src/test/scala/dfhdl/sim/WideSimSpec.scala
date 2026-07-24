package dfhdl.sim
import dfhdl.*

/** Wide-value (>64-bit) engine coverage on both kernel tiers. Expected values are computed with the
  * same DFHDL constant arithmetic on the host side, so every assertion is a bit-accurate constant
  * comparison.
  */
class WideSimSpec extends SimSpec:
  // 100-bit operands (25 nibbles) with active bits in both lanes
  private val aV = h"A5A5DEADBEEF0123456789ABC".uint
  private val bV = h"0123456789ABCDEF012345678".uint
  // 72-bit signed operands (18 nibbles): saV negative, sbV positive
  private val saV = h"FFFFFFFFFFFFFF85F1".sint
  private val sbV = h"0123456789ABCDEF01".sint

  bothTiers("wide combinational ops"): tier =>
    (new WideOpsDut).simulation { dut =>
      dut.a.poke(aV)
      dut.b.poke(bV)
      dut.sa.poke(saV)
      dut.sb.poke(sbV)
      assertEquals(dut.sum.peek, aV + bV)
      assertEquals(dut.carrySum.peek, aV +^ bV)
      assertEquals(dut.diff.peek, aV - bV)
      assertEquals(dut.bxor.peek, aV.bits ^ bV.bits)
      assertEquals(dut.band.peek, aV.bits & bV.bits)
      assertEquals(dut.bnot.peek, ~aV.bits)
      assertEquals(dut.ltu.peek, 0) // aV > bV
      assertEquals(dut.lts.peek, 1) // negative < positive
      assertEquals(dut.equ.peek, 0)
      assertEquals(dut.catOut.peek, aV.bits ++ saV.bits)
      assertEquals(dut.sliceOut.peek, aV.bits(80, 48))
      assertEquals(dut.bitSel.peek, aV.bits(64))
      assertEquals(dut.sextOut.peek, saV.resize(100))
      // swapped operands flip the comparisons
      dut.a.poke(bV)
      dut.b.poke(aV)
      assertEquals(dut.ltu.peek, 1)
      dut.b.poke(bV)
      assertEquals(dut.equ.peek, 1)
      dut.sa.poke(sbV)
      dut.sb.poke(saV)
      assertEquals(dut.lts.peek, 0)
    }.withTier(tier).run()

  bothTiers("wide dynamic shifts (barrel network)"): tier =>
    (new WideOpsDut).simulation { dut =>
      dut.a.poke(aV)
      dut.b.poke(bV)
      dut.sa.poke(saV)
      dut.sb.poke(sbV)
      for amt <- List(0, 1, 37, 63, 64, 99) do
        dut.shamt.poke(amt)
        assertEquals(dut.shlDynOut.peek, aV << amt, s"shl by $amt")
        assertEquals(dut.shrDynOut.peek, aV >> amt, s"shr by $amt")
      for amt <- List(0, 5, 37, 71) do
        dut.shamt.poke(amt)
        assertEquals(dut.sshrDynOut.peek, saV >> amt, s"arithmetic shr by $amt")
      // amounts at/above the width: logical shifts flush to zero, arithmetic fills the sign
      dut.shamt.poke(127)
      assertEquals(dut.shlDynOut.peek, 0)
      assertEquals(dut.shrDynOut.peek, 0)
      assertEquals(dut.sshrDynOut.peek, -1)
    }.withTier(tier).run()

  bothTiers("wide register accumulation across lanes"): tier =>
    (new WideAcc).simulation { dut =>
      val big = h"FFFFFFFFFFFFFFFF".uint.resize(128) // max single-lane value
      dut.add.poke(big)
      assertEquals(dut.acc.peek, 0)
      simCtx.step()
      assertEquals(dut.acc.peek, big)
      simCtx.step() // second accumulation carries into the upper lane
      assertEquals(dut.acc.peek, big + big)
      simCtx.step(1000)
      assertEquals(dut.acc.peek, big * d"128'1002")
    }.withTier(tier).run()

  bothTiers("wide partial (RMW) assignments"): tier =>
    (new WidePartialDut).simulation { dut =>
      val baseV = h"123456789ABCDEF012345678" // 96 bits
      val fldV = h"ABCDE" // 20 bits
      dut.base.poke(baseV)
      dut.fld.poke(fldV)
      dut.b0.poke(1)
      assertEquals(dut.out.peek, b"1" ++ baseV(94, 71) ++ fldV ++ baseV(50, 0))
      dut.b0.poke(0)
      assertEquals(dut.out.peek, b"0" ++ baseV(94, 71) ++ fldV ++ baseV(50, 0))
    }.withTier(tier).run()

  bothTiers("multiplication, division, remainder, repetition"): tier =>
    (new ArithMiscDut).simulation { dut =>
      val xV = d"32'3000000000"
      val yV = d"32'7919"
      dut.x.poke(xV)
      dut.y.poke(yV)
      assertEquals(dut.prod.peek, xV * yV)
      assertEquals(dut.quot.peek, xV / yV)
      assertEquals(dut.rem.peek, xV % yV)
      assertEquals(dut.rep.peek, xV.bits.repeat(3))
      val sxV = sd"32'-2000000000"
      val syV = sd"32'777"
      dut.sx.poke(sxV)
      dut.sy.poke(syV)
      assertEquals(dut.squot.peek, sxV / syV)
      assertEquals(dut.srem.peek, sxV % syV)
    }.withTier(tier).run()

  bothTiers("bit reductions (single-arg |/&/^)"): tier =>
    (new ReduceDut).simulation { dut =>
      // narrow (single-lane): zeros, a single set bit above bit 0, all ones, mixed parity
      for xV <- List(b"0000", b"0100", b"1111", b"1011") do
        dut.x.poke(xV)
        assertEquals(dut.xOr.peek, xV.|, s"or-reduce of $xV")
        assertEquals(dut.xAnd.peek, xV.&, s"and-reduce of $xV")
        assertEquals(dut.xXor.peek, xV.^, s"xor-reduce of $xV")
      // wide (multi-lane): zeros, one bit in the top lane only, all ones, mixed lanes
      val wZeros: Bits[100] <> CONST = all(0)
      val wOnes: Bits[100] <> CONST = all(1)
      val wTop: Bits[100] <> CONST = h"1000000000000000000000000"
      val wMix: Bits[100] <> CONST = h"A5A5DEADBEEF0123456789ABC"
      for wV <- List(wZeros, wTop, wOnes, wMix) do
        dut.w.poke(wV)
        assertEquals(dut.wOr.peek, wV.|, s"or-reduce of $wV")
        assertEquals(dut.wAnd.peek, wV.&, s"and-reduce of $wV")
        assertEquals(dut.wXor.peek, wV.^, s"xor-reduce of $wV")
    }.withTier(tier).run()
end WideSimSpec
