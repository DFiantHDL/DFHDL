package dfhdl.sim
import dfhdl.*

/** Overflow/underflow edge coverage at and around the 64-bit lane size, on both kernel tiers. Every
  * expected wrap value is an explicit literal (not host-side arithmetic), so the tests pin the
  * wraparound semantics themselves: unsigned add/sub wrap modulo 2^W, signed arithmetic wraps
  * two's-complement (MAX+1 -> MIN, MIN-1 -> MAX, -MIN -> MIN), multiplication keeps the low W bits,
  * and MIN / -1 wraps to MIN.
  */
class EdgeSimSpec extends SimSpec:
  // width 64: one full lane — the all-ones mask and Java-Long-boundary paths
  private val max64 = h"FFFFFFFFFFFFFFFF".uint
  private val one64 = d"64'1"
  // width 65: a 1-bit top lane — carries/borrows must cross into and out of it
  private val max65 = h"65'1FFFFFFFFFFFFFFFF".uint
  private val lane65 = h"65'10000000000000000".uint // 2^64
  private val lowMax65 = h"65'0FFFFFFFFFFFFFFFF".uint // 2^64 - 1
  private val one65 = d"65'1"
  // width 128: two full lanes — ripple across the lane boundary
  private val max128 = h"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF".uint
  private val lane128 = h"00000000000000010000000000000000".uint // 2^64
  private val lowMax128 = h"0000000000000000FFFFFFFFFFFFFFFF".uint // 2^64 - 1
  private val one128 = d"128'1"

  bothTiers("unsigned overflow/underflow, width 64 (full-lane mask)"): tier =>
    (new UIntEdgeDut(64)).simulation { dut =>
      dut.a.poke(max64)
      dut.b.poke(one64)
      assertEquals(dut.sum.peek, d"64'0") // MAX + 1 wraps to 0
      assertEquals(dut.carrySum.peek, h"65'10000000000000000".uint) // carry-out kept by +^
      assertEquals(dut.diff.peek, h"FFFFFFFFFFFFFFFE".uint)
      dut.b.poke(max64)
      assertEquals(dut.sum.peek, h"FFFFFFFFFFFFFFFE".uint) // MAX + MAX
      assertEquals(dut.carrySum.peek, h"65'1FFFFFFFFFFFFFFFE".uint)
      assertEquals(dut.equ.peek, 1)
      dut.a.poke(d"64'0")
      dut.b.poke(one64)
      assertEquals(dut.diff.peek, max64) // 0 - 1 underflows to all-ones
      // comparison must be UNSIGNED: 2^63 > 1 (a signed compare would flip this)
      dut.a.poke(h"8000000000000000".uint)
      assertEquals(dut.ltu.peek, 0)
      dut.a.poke(one64)
      dut.b.poke(h"8000000000000000".uint)
      assertEquals(dut.ltu.peek, 1)
    }.withTier(tier).run()

  bothTiers("unsigned overflow/underflow, width 65 (1-bit top lane)"): tier =>
    (new UIntEdgeDut(65)).simulation { dut =>
      dut.a.poke(max65)
      dut.b.poke(one65)
      assertEquals(dut.sum.peek, d"65'0") // carry out of the 1-bit top lane
      assertEquals(dut.carrySum.peek, h"66'20000000000000000".uint)
      // carry INTO the top lane: (2^64 - 1) + 1 = 2^64
      dut.a.poke(lowMax65)
      assertEquals(dut.sum.peek, lane65)
      // borrow OUT of the top lane: 2^64 - 1
      dut.a.poke(lane65)
      assertEquals(dut.diff.peek, lowMax65)
      dut.a.poke(d"65'0")
      assertEquals(dut.diff.peek, max65) // 0 - 1 underflows to all-ones
      // lane priority: top lane dominates even when the low lane says otherwise
      dut.a.poke(lane65)
      dut.b.poke(lowMax65)
      assertEquals(dut.ltu.peek, 0)
      dut.a.poke(lowMax65)
      dut.b.poke(lane65)
      assertEquals(dut.ltu.peek, 1)
    }.withTier(tier).run()

  bothTiers("unsigned overflow/underflow, width 128 (multi-lane ripple)"): tier =>
    (new UIntEdgeDut(128)).simulation { dut =>
      dut.a.poke(max128)
      dut.b.poke(one128)
      assertEquals(dut.sum.peek, d"128'0") // ripple through both lanes
      assertEquals(dut.carrySum.peek, h"129'100000000000000000000000000000000".uint)
      dut.a.poke(lowMax128)
      assertEquals(dut.sum.peek, lane128) // carry crosses the lane boundary
      dut.a.poke(lane128)
      assertEquals(dut.diff.peek, lowMax128) // borrow crosses the lane boundary
      dut.a.poke(d"128'0")
      assertEquals(dut.diff.peek, max128) // 0 - 1 underflows to all-ones
      dut.a.poke(max128)
      dut.b.poke(max128)
      assertEquals(dut.sum.peek, h"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE".uint)
    }.withTier(tier).run()

  bothTiers("signed wraparound, width 64"): tier =>
    (new SIntEdgeDut(64)).simulation { dut =>
      val minS = h"8000000000000000".sint
      val maxS = h"7FFFFFFFFFFFFFFF".sint
      dut.a.poke(maxS)
      dut.b.poke(sd"64'1")
      assertEquals(dut.sum.peek, minS) // MAX + 1 -> MIN
      dut.a.poke(minS)
      assertEquals(dut.diff.peek, maxS) // MIN - 1 -> MAX
      assertEquals(dut.negOut.peek, minS) // -MIN -> MIN
      dut.b.poke(minS)
      assertEquals(dut.sum.peek, sd"64'0") // MIN + MIN wraps to 0
      dut.b.poke(maxS)
      assertEquals(dut.lts.peek, 1) // MIN < MAX
      dut.a.poke(maxS)
      dut.b.poke(minS)
      assertEquals(dut.lts.peek, 0)
      dut.a.poke(sd"64'-1")
      dut.b.poke(sd"64'1")
      assertEquals(dut.sum.peek, sd"64'0")
    }.withTier(tier).run()

  bothTiers("signed wraparound, width 65"): tier =>
    (new SIntEdgeDut(65)).simulation { dut =>
      val minS = h"65'10000000000000000".sint // -2^64
      val maxS = h"65'0FFFFFFFFFFFFFFFF".sint // 2^64 - 1
      dut.a.poke(maxS)
      dut.b.poke(sd"65'1")
      assertEquals(dut.sum.peek, minS)
      dut.a.poke(minS)
      assertEquals(dut.diff.peek, maxS)
      assertEquals(dut.negOut.peek, minS)
      dut.a.poke(sd"65'-1") // all-ones: carry must ripple through the 1-bit top lane
      assertEquals(dut.sum.peek, sd"65'0")
      dut.a.poke(minS)
      dut.b.poke(maxS)
      assertEquals(dut.lts.peek, 1) // the 1-bit top lane compares SIGNED
      dut.a.poke(maxS)
      dut.b.poke(minS)
      assertEquals(dut.lts.peek, 0)
    }.withTier(tier).run()

  bothTiers("signed wraparound, width 128"): tier =>
    (new SIntEdgeDut(128)).simulation { dut =>
      val minS = h"80000000000000000000000000000000".sint
      val maxS = h"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF".sint
      dut.a.poke(maxS)
      dut.b.poke(sd"128'1")
      assertEquals(dut.sum.peek, minS)
      dut.a.poke(minS)
      assertEquals(dut.diff.peek, maxS)
      assertEquals(dut.negOut.peek, minS)
      // -2^64 (all-ones top lane, zero low lane) vs 0: only the top lane decides, signed
      dut.a.poke(h"FFFFFFFFFFFFFFFF0000000000000000".sint)
      dut.b.poke(sd"128'0")
      assertEquals(dut.lts.peek, 1)
      // same top lanes, decision falls to the low lane (unsigned there)
      dut.a.poke(h"FFFFFFFFFFFFFFFF0000000000000001".sint)
      dut.b.poke(h"FFFFFFFFFFFFFFFF0000000000000000".sint)
      assertEquals(dut.lts.peek, 0)
    }.withTier(tier).run()

  bothTiers("64-bit scalar kernel edges: mul/div overflow, shifts at the lane size"): tier =>
    (new Lane64EdgeDut).simulation { dut =>
      val minS = h"8000000000000000".sint
      val maxS = h"7FFFFFFFFFFFFFFF".sint
      // (2^64 - 1)^2 mod 2^64 = 1
      dut.a.poke(max64)
      dut.b.poke(max64)
      assertEquals(dut.prod.peek, one64)
      assertEquals(dut.quot.peek, one64) // MAX / MAX, must be UNSIGNED division
      assertEquals(dut.notOut.peek, h"0000000000000000")
      // unsigned division with the MSB set: (2^64 - 1) / 2^63 = 1 (signed would give 0)
      dut.b.poke(h"8000000000000000".uint)
      assertEquals(dut.quot.peek, one64)
      // signed division overflow: MIN / -1 wraps to MIN (Java Long semantics, no exception)
      dut.sa.poke(minS)
      dut.sb.poke(sd"64'-1")
      assertEquals(dut.squot.peek, minS)
      assertEquals(dut.srem.peek, sd"64'0")
      assertEquals(dut.sprod.peek, minS) // MIN * -1 wraps to MIN
      dut.sa.poke(maxS)
      assertEquals(dut.squot.peek, h"8000000000000001".sint) // MAX / -1 = -MAX
      // MIN * MIN = 2^126 mod 2^64 = 0
      dut.sa.poke(minS)
      dut.sb.poke(minS)
      assertEquals(dut.sprod.peek, sd"64'0")
      // dynamic shifts at the maximum expressible amount (DFHDL caps amounts at clog2(width),
      // so 63 is the edge for a 64-bit value; over-width amounts are covered at width 100)
      dut.a.poke(one64)
      dut.shamt.poke(63)
      assertEquals(dut.shlOut.peek, h"8000000000000000".uint)
      dut.a.poke(h"8000000000000000".uint)
      assertEquals(dut.shrOut.peek, one64)
      // arithmetic shift fills the sign all the way down
      dut.sa.poke(minS)
      dut.shamt.poke(1)
      assertEquals(dut.sshrOut.peek, h"C000000000000000".sint)
      dut.shamt.poke(63)
      assertEquals(dut.sshrOut.peek, sd"64'-1")
    }.withTier(tier).run()
end EdgeSimSpec
