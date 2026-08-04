package dfhdl.sim
import dfhdl.*

/** `.din` read (pending next-cycle value) engine coverage on both kernel tiers: position
  * sensitivity, read-modify-write, partial reads, a read after a conditional assignment, and reads
  * inside an RT process.
  */
class RegDINSimSpec extends SimSpec:

  bothTiers("din reads are position-sensitive and chain through read-modify-write"): tier =>
    (new RegDINDut).simulation { dut =>
      val xs = Vector(d"8'3", d"8'7", d"8'0", d"8'20")
      var r: UInt[8] <> CONST = d"8'0"
      for t <- xs.indices do
        dut.i.poke(xs(t))
        assertEquals(dut.r.peek, r, s"r at t=$t")
        // read before any assignment: the register itself
        assertEquals(dut.pre.peek, r, s"pre at t=$t")
        // read after both assignments: the pending next-cycle value
        assertEquals(dut.post.peek, r + 1 + xs(t), s"post at t=$t")
        r = r + 1 + xs(t)
        simCtx.step()
      end for
      assertEquals(dut.r.peek, r)
    }.withTier(tier).run()

  bothTiers("partial din reads slice the pending value"): tier =>
    (new RegDINPartialDut).simulation { dut =>
      val xs = Vector(h"13", h"c8", h"25")
      var r: Bits[8] <> CONST = h"00"
      for t <- xs.indices do
        dut.i.poke(xs(t))
        val pend = r | xs(t)
        assertEquals(dut.lo.peek, pend(3, 0), s"lo at t=$t")
        assertEquals(dut.hi.peek, pend(7, 4), s"hi at t=$t")
        r = pend
        simCtx.step()
      end for
    }.withTier(tier).run()

  bothTiers("a din read after a conditional assignment follows the taken branch"): tier =>
    (new RegDINCondDut).simulation { dut =>
      val ens = Vector(1, 0, 1, 1, 0)
      var r: UInt[8] <> CONST = d"8'0"
      for t <- ens.indices do
        dut.en.poke(if ens(t) == 1 then 1 else 0)
        val pend = if ens(t) == 1 then r + 10 else r
        assertEquals(dut.pend.peek, pend, s"pend at t=$t")
        r = pend
        simCtx.step()
      end for
    }.withTier(tier).run()

  // reads inside an RT process are covered by the staged oracle in `RTProcessSimSpec`, where the
  // FSM the backends synthesize is the reference
end RegDINSimSpec
