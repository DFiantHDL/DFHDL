package dfhdl.sim
import dfhdl.*

/** Free-running counter: `count` equals the elapsed cycle count (up to 2^32). */
class FreeCounter extends RTDesign:
  val count = UInt(32) <> OUT.REG init 0
  count.din := count + 1

/** [[SimulationRun]] lifecycle coverage: run status/reasons, cycle-limit pausing mid-`step`,
  * continuation budgets, post-run inspection, imperative (block-less) driving, background
  * pause/continue/finish, and failure surfacing.
  */
class SimulationRunSpec extends SimSpec:
  bothTiers("foreground run finishes and remains inspectable"): tier =>
    val run = (new FreeCounter).simulation { dut =>
      simCtx.step(100)
      assertEquals(dut.count.peek, 100)
    }.withTier(tier).run()
    assertEquals(run.getRunStatus, RunStatus.Finished(FinishedReason.MainDone))
    assertEquals(run.cycles, 100L)
    run.inspect { dut => assertEquals(dut.count.peek, 100) }
    intercept[IllegalStateException](run.continue())

  bothTiers("cycle limit pauses mid-step; continue grants fresh budgets"): tier =>
    val run = (new FreeCounter).simulation { dut =>
      simCtx.step(100) // a single step call, interrupted twice by budget exhaustion
    }.withTier(tier).run(limit = 10)
    assertEquals(run.getRunStatus, RunStatus.Paused(PausedReason.Limit))
    assertEquals(run.cycles, 10L)
    run.inspect { dut => assertEquals(dut.count.peek, 10) }
    assertEquals(run.continue(limit = 50), RunStatus.Paused(PausedReason.Limit))
    assertEquals(run.cycles, 60L)
    run.inspect { dut => assertEquals(dut.count.peek, 60) }
    assertEquals(run.continue(), RunStatus.Finished(FinishedReason.MainDone))
    assertEquals(run.cycles, 100L)

  bothTiers("block-less run: imperative continue/inspect driving"): tier =>
    val run = (new FreeCounter).simulation.withTier(tier).run()
    assertEquals(run.getRunStatus, RunStatus.Paused(PausedReason.Limit))
    assertEquals(run.cycles, 0L)
    run.continue(5)
    run.inspect { dut => assertEquals(dut.count.peek, 5) }
    run.continue(7)
    run.inspect { dut => assertEquals(dut.count.peek, 12) }
    // an unbounded continue has no block to hand control back to
    intercept[IllegalArgumentException](run.continue())
    // and stepping from inspect is rejected — the clock belongs to continue
    intercept[IllegalStateException](run.inspect { dut => simCtx.step() })

  bothTiers("background run: pause, inspect consistently, resume, pause again"):
    tier =>
      val run = (new FreeCounter).simulation { dut =>
        simCtx.step(Long.MaxValue) // effectively endless — only pause() can interrupt
      }.withTier(tier).runBackground()
      assertEquals(run.pause(), RunStatus.Paused(PausedReason.User))
      val c1 = run.cycles
      run.inspect { dut => assertEquals(dut.count.peek, d"32'$c1") }
      assertEquals(run.continue(), RunStatus.Running)
      assertEquals(run.pause(), RunStatus.Paused(PausedReason.User))
      val c2 = run.cycles
      assert(c2 >= c1, s"cycles went backwards: $c2 < $c1")
      run.inspect { dut => assertEquals(dut.count.peek, d"32'$c2") }
      // left paused: the parked daemon worker costs nothing

  bothTiers("background run finishes to completion"): tier =>
    val run = (new FreeCounter).simulation { dut =>
      simCtx.step(300_000)
    }.withTier(tier).runBackground()
    assertEquals(run.finish(), RunStatus.Finished(FinishedReason.MainDone))
    assertEquals(run.cycles, 300_000L)
    run.inspect { dut => assertEquals(dut.count.peek, d"32'300000") }

  test("background host-block failure surfaces on finish"):
    val run = (new FreeCounter).simulation { dut =>
      simCtx.step(10)
      throw new RuntimeException("boom")
    }.runBackground()
    intercept[RuntimeException](run.finish())
    assertEquals(run.getRunStatus, RunStatus.Finished(FinishedReason.HostError))

  test("foreground host-block failure still fails fast"):
    intercept[RuntimeException] {
      (new FreeCounter).simulation { dut =>
        simCtx.step(10)
        throw new RuntimeException("boom")
      }.run()
    }
end SimulationRunSpec
