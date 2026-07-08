package dfhdl.sim
import dfhdl.*

/** `.reg` (History State alias) engine coverage on both kernel tiers: per-step delay chains,
  * time-zero inits, chained aliases, bare `.reg` on a REG dcl, versioned sampling of a
  * multiply-assigned wire, and wide history registers.
  */
class RegAliasSimSpec extends SimSpec:
  bothTiers("reg alias delays, inits, chaining, versioned sampling"): tier =>
    (new RegAliasDut).simulation { dut =>
      // time zero: every history register shows its init
      assertEquals(dut.d1.peek, 55)
      assertEquals(dut.d2.peek, 0)
      assertEquals(dut.d3.peek, 0)
      assertEquals(dut.accD.peek, 0)
      val xs = Vector(d"8'10", d"8'20", d"8'30", d"8'40", d"8'50", d"8'60")
      var sum = d"8'0"
      var prevSum = d"8'0"
      for t <- xs.indices do
        val x = xs(t)
        dut.i.poke(x)
        simCtx.step()
        prevSum = sum
        sum = sum + x
        assertEquals(dut.d1.peek, xs(t), s"d1 at t=$t")
        assertEquals(dut.d2.peek, if t >= 1 then xs(t - 1) else d"8'0", s"d2 at t=$t")
        assertEquals(dut.d3.peek, if t >= 2 then xs(t - 2) else d"8'0", s"d3 at t=$t")
        // versioned wire sampling: verA sees i, verB sees i + 1 (both 1-cycle delayed)
        assertEquals(dut.verA.peek, xs(t), s"verA at t=$t")
        assertEquals(dut.verB.peek, xs(t) + 1, s"verB at t=$t")
        // reg-of-reg: acc accumulates, accD trails it by one cycle
        assertEquals(dut.acc.peek, sum, s"acc at t=$t")
        assertEquals(dut.accD.peek, prevSum, s"accD at t=$t")
      end for
    }.withTier(tier).run()

  bothTiers("wide reg alias (lane-split history chain)"): tier =>
    (new WideRegAliasDut).simulation { dut =>
      val a = h"A5A5DEADBEEF0123456789ABC".uint
      val b = h"0123456789ABCDEF012345678".uint
      dut.i.poke(a)
      assertEquals(dut.d2.peek, 0)
      simCtx.step()
      assertEquals(dut.d2.peek, 0)
      dut.i.poke(b)
      simCtx.step()
      assertEquals(dut.d2.peek, a)
      simCtx.step()
      assertEquals(dut.d2.peek, b)
    }.withTier(tier).run()

  test("`.reg` of a mutable wire inside a conditional block is rejected (loudly)"):
    intercept[UnsupportedOperationException] {
      (new CondWireRegDut).simulation { dut => simCtx.step() }.run()
    }
end RegAliasSimSpec
