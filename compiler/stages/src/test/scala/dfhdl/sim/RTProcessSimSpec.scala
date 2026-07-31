package dfhdl.sim
import dfhdl.*
import dfhdl.core.DFCG
import dfhdl.compiler.stages.dropRTProcess

/** RT process (FSM) engine coverage on both kernel tiers.
  *
  * Two verification layers:
  *   - typed-frontend cycle traces on the public [[Simulation]] surface (`dsn.simulation`, typed
  *     peek/poke, DFHDL-constant reference models) for the shapes whose counts the cycle-semantics
  *     rules pin down directly (wait composition equivalence, guard sampling, park loops,
  *     prologue/bootstrap)
  *   - the staged oracle: the same design lowered by the actual FSM stages (`dropRTProcess`) and
  *     simulated in lockstep — every fusion/fallback corner must be cycle-identical to the FSM the
  *     backends would synthesize. The oracle run applies the stage lowering through the in-package
  *     `Simulation` dbTransform behind a second design instance's typed surface, so both sides stay
  *     on typed member-object peek/poke.
  */
class RTProcessSimSpec extends SimSpec:

  /** Runs a design and its stage-lowered FSM oracle in lockstep, comparing the watched members, the
    * accumulated text output, and the run status every cycle under the same pokes. `watch` entries
    * are member selectors (the failure clue names come from the members themselves); `pokes` runs
    * on both duts through the typed surface. A terminal status (e.g. a `finish` statement) must
    * land on the same cycle on both sides and ends the lockstep.
    */
  private def lockstep[D <: dfhdl.core.Design](
      mkDsn: => D,
      tier: SimTier,
      cycles: Int,
      watch: List[D => dfhdl.core.DFValAny] = Nil,
      pokes: Int => DFCG ?=> SimCtx ?=> D => Unit = _ => _ => ()
  ): Unit =
    val rawRun = mkDsn.simulation.withTier(tier).run()
    val oracle = new Simulation(mkDsn, None, tier, 0L, dbTransform = _.dropRTProcess).run()
    val rawText = new StringBuilder
    val oracleText = new StringBuilder
    rawRun.raw.textSink = s =>
      rawText ++= s; ()
    oracle.raw.textSink = s =>
      oracleText ++= s; ()
    var t = 0
    var done = false
    while t < cycles && !done do
      rawRun.inspect { dut => pokes(t)(dut) }
      oracle.inspect { dut => pokes(t)(dut) }
      for sel <- watch do
        val expected = rawRun.inspect { dut => sel(dut).peek }
        oracle.inspect { dut =>
          val member = sel(dut)
          val name = simCtx.memberPath(member.asIR)
          assertEquals(member.peek, expected, s"'$name' at cycle $t (raw vs staged oracle)")
        }
      val rawStatus = rawRun.continue(1)
      val oracleStatus = oracle.continue(1)
      assertEquals(
        oracleText.result(),
        rawText.result(),
        s"text output through cycle $t (staged oracle vs raw)"
      )
      assertEquals(oracleStatus, rawStatus, s"run status at cycle $t (staged oracle vs raw)")
      rawStatus match
        case RunStatus.Finished(_) =>
          assertEquals(oracle.cycles, rawRun.cycles, "finish cycle (staged oracle vs raw)")
          done = true
        case _ => ()
      t += 1
    end while
  end lockstep

  // ---- direct cycle-trace semantics (typed frontend) ----------------------------------------

  bothTiers("toggle: const prologue folds to time zero and re-executes on wrap-around"): tier =>
    (new ToggleProc).simulation { dut =>
      var expected: Bit = 1
      for t <- 0 until 10 do
        assertEquals(dut.x.peek, expected, s"x at cycle $t")
        expected = ~expected
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("wait-composition equivalence: flat = loop of waits = nested loops"): tier =>
    val flat = (new WaitEqFlat).simulation.withTier(tier).run()
    val loop = (new WaitEqLoop).simulation.withTier(tier).run()
    val nested = (new WaitEqNested).simulation.withTier(tier).run()
    for t <- 0 until 60 do
      val expected = flat.inspect { dut => dut.tick.peek }
      loop.inspect { dut => assertEquals(dut.tick.peek, expected, s"loop vs flat at cycle $t") }
      nested.inspect { dut =>
        assertEquals(dut.tick.peek, expected, s"nested vs flat at cycle $t")
      }
      flat.continue(1); loop.continue(1); nested.continue(1)

  bothTiers("wait-composition period: 1 lead + 12 wait cycles per pass"): tier =>
    (new WaitEqFlat).simulation { dut =>
      var expected: Bit = 0
      for t <- 0 until 55 do
        if t > 0 && t % 13 == 0 then expected = ~expected
        assertEquals(dut.tick.peek, expected, s"tick at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("guard sampling: one cycle per streamed element, exit fused with the last"): tier =>
    (new StreamerProc).simulation { dut =>
      // pass period: 1 (lead) + 4 (loop) + 1 (done wait) = 6 cycles; `y` holds its last element
      // through the lead cycle of the next pass (nothing resets it)
      val ySteady = Array(3, 0, 1, 2, 3, 3)
      val doneExp = Array[Bit](0, 0, 0, 0, 0, 1)
      for t <- 0 until 24 do
        val yExp = if t == 0 then 0 else ySteady(t % 6)
        assertEquals(dut.y.peek, yExp, s"y at cycle $t")
        assertEquals(dut.done.peek, doneExp(t % 6), s"done at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("empty (park) loops: iterations plus the guard-false detection cycle"): tier =>
    (new EmptyLoopsProc).simulation { dut =>
      // pass period: 1 (lead) + 3 * (2 iterations + 1 detect) = 10 cycles
      var expected: Bit = 0
      for t <- 0 until 40 do
        if t > 0 && t % 10 == 0 then expected = ~expected
        assertEquals(dut.tick.peek, expected, s"tick at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("loop-first process: one cycle per iteration, zero bootstrap cycles"): tier =>
    (new LoopFirstProc).simulation { dut =>
      // the loop control fuses at the wrap-around and the reset fold provides y = 0 and the
      // iterator at time zero: a single-state FSM streaming y = t mod 4 from the first cycle
      for t <- 0 until 20 do
        assertEquals(dut.y.peek, t % 4, s"y at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("while(false) parks exactly one cycle per loop"): tier =>
    (new WhileFalseProc).simulation { dut =>
      // pass period: 1 (lead) + 3 * 1 = 4 cycles; cnt increments once per pass
      for t <- 0 until 20 do
        assertEquals(dut.cnt.peek, t / 4, s"cnt at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("dynamic cycle wait parks exactly n cycles (live compare)"): tier =>
    (new DynWaitProc).simulation { dut =>
      dut.n.poke(3)
      var expected: Bit = 0
      for t <- 0 until 18 do
        if t > 0 && t % 3 == 0 then expected = ~expected
        assertEquals(dut.tick.peek, expected, s"tick at cycle $t (n=3)")
        simCtx.step()
    }.withTier(tier).run()
    (new DynWaitProc).simulation { dut =>
      dut.n.poke(1)
      var expected: Bit = 0
      for t <- 0 until 8 do
        assertEquals(dut.tick.peek, expected, s"tick at cycle $t (n=1)")
        expected = ~expected
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("condition wait samples once per cycle; while-with-empty-body is equivalent"): tier =>
    val a = (new CondWaitProc).simulation.withTier(tier).run()
    val b = (new WhileVsWaitProc).simulation.withTier(tier).run()
    def pokeGo(v: Bit): Unit =
      a.inspect { dut => dut.go.poke(v) }
      b.inspect { dut => dut.go.poke(v) }
    pokeGo(0)
    for t <- 0 until 30 do
      if t == 5 then pokeGo(1)
      if t == 7 then pokeGo(0)
      if t == 15 then pokeGo(1)
      val expected = a.inspect { dut => dut.busy.peek }
      b.inspect { dut =>
        assertEquals(dut.busy.peek, expected, s"while vs waitUntil at cycle $t")
      }
      a.continue(1); b.continue(1)
    // explicit: go raised at cycle 5 -> busy high exactly at cycles 6,7,8
    (new CondWaitProc).simulation { dut =>
      dut.go.poke(0)
      val busyExp = Array[Bit](0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0)
      for t <- 0 until 12 do
        if t == 5 then dut.go.poke(1)
        if t == 6 then dut.go.poke(0)
        assertEquals(dut.busy.peek, busyExp(t), s"busy at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("run-once sequence halts at the endless wait; the scheduler skips ahead"): tier =>
    (new RunOnceProc).simulation { dut =>
      assertEquals(dut.out.peek, 5)
      simCtx.step()
      assertEquals(dut.out.peek, 5)
      simCtx.step()
      assertEquals(dut.out.peek, 42)
      simCtx.step(1000000)
      assertEquals(dut.out.peek, 42)
      // scheduler observability (internal, in-package): the halted FSM is skipped, not evaluated
      val skipped = simCtx.raw.skippedCycles
      assert(skipped > 900000, s"skipped only $skipped cycles")
    }.withTier(tier).run()

  bothTiers("a very long wait is skipped on the event timeline, cycle-exactly"): tier =>
    (new BigWaitProc).simulation { dut =>
      simCtx.step(1000000)
      assertEquals(dut.tick.peek, 1)
      simCtx.step(1000000)
      assertEquals(dut.tick.peek, 0)
      val skipped = simCtx.raw.skippedCycles
      assert(skipped > 1900000, s"skipped only $skipped cycles")
    }.withTier(tier).run()

  bothTiers("skip-ahead is transparent: bulk stepping equals cycle-by-cycle stepping"): tier =>
    val one = (new TwoProcs).simulation.withTier(tier).run()
    val bulk = (new TwoProcs).simulation.withTier(tier).run()
    for _ <- 0 until 60 do one.continue(1)
    bulk.continue(60)
    val (aExp, bExp, bothExp) = one.inspect { dut => (dut.a.peek, dut.b.peek, dut.both.peek) }
    bulk.inspect { dut =>
      assertEquals(dut.a.peek, aExp, "a after 60")
      assertEquals(dut.b.peek, bExp, "b after 60")
      assertEquals(dut.both.peek, bothExp, "both after 60")
    }
    val one2 = (new DynWaitProc).simulation.withTier(tier).run()
    val bulk2 = (new DynWaitProc).simulation.withTier(tier).run()
    one2.inspect { dut => dut.n.poke(200) }
    bulk2.inspect { dut => dut.n.poke(200) }
    for _ <- 0 until 1000 do one2.continue(1)
    bulk2.continue(1000)
    val tickExp = one2.inspect { dut => dut.tick.peek }
    bulk2.inspect { dut => assertEquals(dut.tick.peek, tickExp, "tick after 1000") }

  bothTiers("bootstrap semantics: non-convertible prologue costs one cycle; trailing-share " +
    "keeps the trailing value observable"): tier =>
    // NoBootProc: period 2, x constant 5 from time zero
    (new NoBootProc).simulation { dut =>
      for t <- 0 until 8 do
        assertEquals(dut.x.peek, 5, s"NoBoot x at cycle $t")
        simCtx.step()
    }.withTier(tier).run()
    // BootProc: boot(1) + wait(2) = period 3; x samples `a` in the boot cycle
    (new BootProc).simulation { dut =>
      dut.a.poke(9)
      assertEquals(dut.x.peek, 0) // time zero: decl init, prologue not folded
      simCtx.step()
      assertEquals(dut.x.peek, 9)
      simCtx.step(6)
      dut.a.poke(13)
      simCtx.step(3) // next boot cycle re-samples
      assertEquals(dut.x.peek, 13)
    }.withTier(tier).run()
    // ShareGateProc: boot kept; the trailing x:=7 is observable for exactly one cycle per pass
    (new ShareGateProc).simulation { dut =>
      // period 4: boot, W, W, W(exit: x:=7); x=7 visible only in the boot cycle of the next pass
      for t <- 0 until 12 do
        assertEquals(dut.x.peek, if t > 0 && t % 4 == 0 then 7 else 0, s"ShareGate x at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("two processes with co-prime periods"): tier =>
    (new TwoProcs).simulation { dut =>
      var aExp: Bit = 0
      var bExp: Bit = 0
      for t <- 0 until 24 do
        if t > 0 && t % 2 == 0 then aExp = ~aExp
        if t > 0 && t % 3 == 0 then bExp = ~bExp
        assertEquals(dut.a.peek, aExp, s"a at cycle $t")
        assertEquals(dut.b.peek, bExp, s"b at cycle $t")
        assertEquals(dut.both.peek, aExp ^ bExp, s"both at cycle $t")
        simCtx.step()
    }.withTier(tier).run()

  bothTiers("dirty-guard while keeps a control state per iteration boundary"): tier =>
    (new DirtyGuardProc).simulation { dut =>
      dut.sel.poke(0)
      // t0: lead wait; odd cycles: control state; even cycles (from t2): in-loop wait.
      // acc increments once per 2-cycle iteration: acc = k first visible at t = 2k + 1.
      for t <- 0 until 22 do
        val expected = if t < 3 then 0 else math.min((t - 1) / 2, 10)
        assertEquals(dut.acc.peek, expected, s"acc at cycle $t")
        simCtx.step()
      // t21 was the control cycle seeing acc = 10: guard false, trailing reset on the exit edge
      assertEquals(dut.acc.peek, 0, "acc after the loop exit")
    }.withTier(tier).run()

  // ---- text output (M2): prints, reports, assertions, finish, starvation --------------------

  bothTiers("process prints fire on their transition cycles with typed rendering"): tier =>
    val run = (new PrintFlowProc).simulation.withTier(tier).run()
    val out = new StringBuilder
    run.raw.textSink = s =>
      out ++= s; ()
    // period 6: println on the lead wait's expiry cycle, report on the single-cycle wait
    val expectedAt = Map(
      2 -> "tick x=1 b=abc f=false st=Idle\n",
      3 -> "INFO: mid x=8 [PrintFlowProc @ cycle 3]\n",
      8 -> "tick x=1 b=abc f=true st=Busy\n",
      9 -> "INFO: mid x=8 [PrintFlowProc @ cycle 9]\n"
    )
    for t <- 1 to 12 do
      out.clear()
      run.continue(1)
      assertEquals(out.result(), expectedAt.getOrElse(t, ""), s"output of cycle $t")

  bothTiers("debug prints the design path, position, and name = value lines"): tier =>
    val run = (new DebugProc).simulation.withTier(tier).run()
    val out = new StringBuilder
    run.raw.textSink = s =>
      out ++= s; ()
    run.continue(2)
    val text = out.result()
    assert(text.startsWith("Debug at DebugProc\n"), text)
    assert(text.contains("RTProcessDesigns.scala"), text)
    assert(text.endsWith("a = 3\n"), text)
    out.clear()
    run.continue(2)
    assert(out.result().endsWith("a = 4\n"), out.result())

  bothTiers("finish() ends the run on its fused transition cycle"): tier =>
    // block-less: the continue that hits the finish reports the terminal status
    val run = (new FinishProc).simulation.withTier(tier).run()
    assertEquals(run.continue(10), RunStatus.Finished(FinishedReason.Finish))
    assertEquals(run.cycles, 3L)
    // host-block: the block unwinds mid-`step` and the run reports the same terminal status
    val blockRun = (new FinishProc).simulation { dut => simCtx.step(100) }.withTier(tier).run()
    assertEquals(blockRun.getRunStatus, RunStatus.Finished(FinishedReason.Finish))
    assertEquals(blockRun.cycles, 3L)

  bothTiers("assertion severity policy: continue (default), pause, finish, and fatal"): tier =>
    // default policy: errors print and count, the run keeps going (HDL simulator behavior)
    val cont = (new CountAssertProc()).simulation.withTier(tier).run()
    val out = new StringBuilder
    cont.raw.textSink = s =>
      out ++= s; ()
    assertEquals(cont.continue(9), RunStatus.Paused(PausedReason.Limit))
    assertEquals(
      out.result(),
      "ERROR: cnt reached 3 [CountAssertProc @ cycle 6]\n" +
        "ERROR: cnt reached 4 [CountAssertProc @ cycle 8]\n"
    )
    assertEquals(cont.raw.errorCount, 2L)
    // pause-on-error: the run pauses at each failing cycle and resumes on continue
    val pausing = (new CountAssertProc()).simulation.withTier(tier)
      .withSeverityPolicy(error = SeverityAction.Pause).run()
    pausing.raw.textSink = _ => ()
    assertEquals(pausing.continue(20), RunStatus.Paused(PausedReason.Error))
    assertEquals(pausing.cycles, 6L)
    assertEquals(pausing.continue(20), RunStatus.Paused(PausedReason.Error))
    assertEquals(pausing.cycles, 8L)
    // finish-on-error: terminal
    val finishing = (new CountAssertProc()).simulation.withTier(tier)
      .withSeverityPolicy(error = SeverityAction.Finish).run()
    finishing.raw.textSink = _ => ()
    assertEquals(finishing.continue(20), RunStatus.Finished(FinishedReason.Error))
    assertEquals(finishing.cycles, 6L)
    // fatal always finishes, policy-independent
    val fatal = (new CountAssertProc(Severity.Fatal)).simulation.withTier(tier).run()
    fatal.raw.textSink = _ => ()
    assertEquals(fatal.continue(20), RunStatus.Finished(FinishedReason.Fatal))
    assertEquals(fatal.cycles, 6L)

  bothTiers("a concurrent (design-body) assertion is checked every cycle"): tier =>
    val run = (new BodyAssertDesign).simulation.withTier(tier)
      .withSeverityPolicy(warning = SeverityAction.Pause).run()
    val out = new StringBuilder
    run.raw.textSink = s =>
      out ++= s; ()
    assertEquals(run.continue(20), RunStatus.Paused(PausedReason.Warning))
    assertEquals(run.cycles, 6L)
    assertEquals(out.result(), "WARNING: cnt reached 5 [BodyAssertDesign @ cycle 6]\n")
    // the pause sits on the committed cycle boundary: the fire read cnt = 5, the commit made 6
    run.inspect { dut => assertEquals(dut.cnt.peek, 6) }
    // the condition keeps failing, so the very next cycle pauses again
    assertEquals(run.continue(20), RunStatus.Paused(PausedReason.Warning))
    assertEquals(run.cycles, 7L)

  bothTiers("event starvation finishes a block-less run of a closed design"): tier =>
    // RunOnceProc halts at an endless wait with no pokeable inputs: nothing can ever happen
    val run = (new RunOnceProc).simulation.withTier(tier).run()
    assertEquals(run.continue(100), RunStatus.Finished(FinishedReason.MainDone))
    assertEquals(run.cycles, 100L)
    run.inspect { dut => assertEquals(dut.out.peek, 42) }
    // an open design (pokeable inputs) never starves — the budget just runs out
    val open = (new CondWaitProc).simulation.withTier(tier).run()
    open.inspect { dut => dut.go.poke(0) }
    assertEquals(open.continue(100), RunStatus.Paused(PausedReason.Limit))
    assertEquals(open.cycles, 100L)

  // ---- staged-oracle lockstep (fusion/fallback fidelity vs the FSM lowering stages) ----------

  bothTiers("oracle: toggle"): tier =>
    lockstep(new ToggleProc, tier, 20, watch = List(_.x))

  bothTiers("oracle: wait-composition loop and nest"): tier =>
    lockstep(new WaitEqLoop, tier, 60, watch = List(_.tick))
    lockstep(new WaitEqNested, tier, 60, watch = List(_.tick))

  bothTiers("oracle: streamer guard sampling"): tier =>
    lockstep(new StreamerProc, tier, 30, watch = List(_.y, _.done))

  bothTiers("oracle: loop-first process fuses at the wrap-around with zero bootstrap"): tier =>
    lockstep(new LoopFirstProc, tier, 30, watch = List(_.y))

  bothTiers("oracle: statically skipped leading loop keeps the one-time reset bootstrap"): tier =>
    lockstep(new SkipLoopFirstProc, tier, 20, watch = List(_.tick))

  bothTiers("oracle: empty (park) loops"): tier =>
    lockstep(new EmptyLoopsProc, tier, 40, watch = List(_.tick))

  bothTiers("oracle: dynamic cycle wait with a live-changing count"): tier =>
    lockstep(
      new DynWaitProc,
      tier,
      40,
      watch = List(_.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.n.poke(4)
          else if t == 9 then dut.n.poke(2) // change mid-wait: live compare semantics
          else if t == 20 then dut.n.poke(7)
    )

  bothTiers("oracle: condition wait"): tier =>
    lockstep(
      new CondWaitProc,
      tier,
      30,
      watch = List(_.busy),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 4 then dut.go.poke(1)
          else if t == 6 then dut.go.poke(0)
          else if t == 16 then dut.go.poke(1)
    )

  bothTiers("oracle: step FSM with self-transitions and relative/explicit jumps"): tier =>
    val xSeq = Array[Bit](0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1)
    lockstep(
      new SimpleFSMProc,
      tier,
      xSeq.length,
      watch = List(_.y),
      pokes = t => dut => dut.x.poke(xSeq(t))
    )

  bothTiers("oracle: nested first steps fuse into a shared entry cycle"): tier =>
    lockstep(
      new NestedStepsProc,
      tier,
      20,
      watch = List(_.a, _.b, _.c)
    )

  bothTiers("oracle: match-based step dispatch"): tier =>
    val selSeq = Array(0, 0, 1, 2, 0, 3, 0, 0, 2, 1, 0, 2)
    lockstep(
      new MatchDispatchProc,
      tier,
      selSeq.length,
      watch = List(_.y),
      pokes = t => dut => dut.sel.poke(selSeq(t))
    )

  bothTiers("oracle: dirty-guard while keeps a control state"): tier =>
    lockstep(
      new DirtyGuardProc,
      tier,
      30,
      watch = List(_.acc),
      pokes = t =>
        dut =>
          if t == 0 then dut.sel.poke(0)
          else if t == 9 then dut.sel.poke(1)
          else if t == 15 then dut.sel.poke(0)
    )

  bothTiers("oracle: dynamic-nest re-entry keeps the outer control state"): tier =>
    lockstep(
      new DynNestProc,
      tier,
      40,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.n.poke(2)
          else if t == 20 then dut.n.poke(0) // zero-iteration inner passes
    )

  bothTiers("oracle: single dynamic loop fuses; a zero count skips with zero cycles"): tier =>
    lockstep(
      new DynLoopProc,
      tier,
      30,
      watch = List(_.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.n.poke(0)
          else if t == 10 then dut.n.poke(2)
    )

  bothTiers("oracle: bootstrap and trailing-share shapes"): tier =>
    lockstep(
      new BootProc,
      tier,
      20,
      watch = List(_.x),
      pokes = t =>
        dut =>
          if t == 0 then dut.a.poke(9)
          else if t == 8 then dut.a.poke(13)
    )
    lockstep(new NoBootProc, tier, 12, watch = List(_.x))
    lockstep(new ShareGateProc, tier, 20, watch = List(_.x))

  bothTiers("oracle: two processes in one design"): tier =>
    lockstep(
      new TwoProcs,
      tier,
      30,
      watch = List(_.a, _.b, _.both)
    )

  bothTiers("oracle: prints, reports, and debug fire on identical cycles with identical text"):
    tier =>
      lockstep(new PrintFlowProc, tier, 30, watch = List(_.x))
      lockstep(new DebugProc, tier, 20, watch = List(_.a))

  bothTiers("oracle: finish() ends both runs on the same cycle"): tier =>
    lockstep(new FinishProc, tier, 10)

  bothTiers("oracle: failing assertions report on identical cycles"): tier =>
    lockstep(new CountAssertProc(), tier, 12, watch = List(_.cnt))

  bothTiers("oracle: FALL_THROUGH while park loop, zero-cycle skip vs iteration"): tier =>
    lockstep(
      new FallThroughWhileProc,
      tier,
      40,
      watch = List(_.cnt, _.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 5 then dut.go.poke(1)
          else if t == 9 then dut.go.poke(0)
          else if t == 20 then dut.go.poke(1)
          else if t == 23 then dut.go.poke(0)
    )

  bothTiers("oracle: FALL_THROUGH loop with a waiting body costs one cycle per iteration"): tier =>
    lockstep(
      new FallThroughWaitLoopProc,
      tier,
      40,
      watch = List(_.cnt, _.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 5 then dut.go.poke(1)
          else if t == 9 then dut.go.poke(0)
          else if t == 20 then dut.go.poke(1)
          else if t == 23 then dut.go.poke(0)
    )

  bothTiers("oracle: a fused FALL_THROUGH loop skipped into the forever wrap-around"): tier =>
    lockstep(
      new FallThroughWrapLoopProc,
      tier,
      40,
      watch = List(_.cnt, _.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 4 then dut.go.poke(1)
          else if t == 10 then dut.go.poke(0)
          else if t == 18 then dut.go.poke(1)
    )

  bothTiers("oracle: chained fused FALL_THROUGH loops skip within one cycle"): tier =>
    lockstep(
      new FallThroughChainLoopProc,
      tier,
      40,
      watch = List(_.cnt, _.tick),
      pokes = t =>
        dut =>
          if t == 0 then
            dut.a.poke(0)
            dut.b.poke(0)
          else if t == 4 then dut.a.poke(1)
          else if t == 8 then
            dut.a.poke(0)
            dut.b.poke(1)
          else if t == 12 then dut.b.poke(0)
          else if t == 20 then
            dut.a.poke(1)
            dut.b.poke(1)
          else if t == 26 then
            dut.a.poke(0)
            dut.b.poke(0)
    )

  bothTiers("oracle: a fused step's fallThrough skips its payload and its cycle"): tier =>
    lockstep(
      new FallThroughFusedStepProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 4 then dut.x.poke(1)
          else if t == 12 then dut.x.poke(0)
    )

  bothTiers("oracle: FALL_THROUGH empty-body while loop"): tier =>
    lockstep(
      new FallThroughEmptyWhileProc,
      tier,
      30,
      watch = List(_.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 5 then dut.go.poke(1)
          else if t == 8 then dut.go.poke(0)
    )

  bothTiers("oracle: FALL_THROUGH conditional waits, zero-cycle skip vs park"): tier =>
    lockstep(
      new FallThroughCondWaitProc,
      tier,
      40,
      watch = List(_.cnt, _.tick),
      pokes = t =>
        dut =>
          if t == 0 then dut.go.poke(0)
          else if t == 4 then dut.go.poke(1)
          else if t == 9 then dut.go.poke(0)
          else if t == 18 then dut.go.poke(1)
          else if t == 25 then dut.go.poke(0)
    )

  bothTiers("oracle: FALL_THROUGH wait decides on the register value just written"): tier =>
    lockstep(
      new FallThroughRegWaitProc,
      tier,
      30,
      watch = List(_.y, _.armed),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 5 then dut.x.poke(1)
          else if t == 12 then dut.x.poke(0)
    )

  bothTiers("oracle: onEntry/onExit hooks fire on non-self step transitions only"): tier =>
    lockstep(
      new HookFSMProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 3 then dut.x.poke(1)
          else if t == 8 then dut.x.poke(0)
          else if t == 12 then dut.x.poke(1)
          else if t == 16 then dut.x.poke(0)
    )

  bothTiers("oracle: fallThrough steps advance in the same cycle"): tier =>
    lockstep(
      new FallThroughStepProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 5 then dut.x.poke(1)
          else if t == 12 then dut.x.poke(0)
    )

  bothTiers("oracle: a fallThrough cascade follows the step's own exit goto"): tier =>
    lockstep(
      new FallThroughOutOfOrderProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 4 then dut.x.poke(1)
          else if t == 11 then dut.x.poke(0)
          else if t == 18 then dut.x.poke(1)
    )

  bothTiers("oracle: FirstStep jumps past the bootstrap step"): tier =>
    lockstep(
      new FirstStepOverBootProc,
      tier,
      24,
      watch = List(_.y, _.z),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 6 then dut.x.poke(1)
          else if t == 15 then dut.x.poke(0)
    )

  bothTiers("oracle: a convertible first-step onEntry folds into the time-zero state"): tier =>
    lockstep(
      new FirstStepEntryProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 3 then dut.x.poke(1)
          else if t == 10 then dut.x.poke(0)
          else if t == 14 then dut.x.poke(1)
    )

  bothTiers("oracle: a non-convertible first-step onEntry keeps the bootstrap state"): tier =>
    lockstep(
      new FirstStepEntryBootProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 4 then dut.x.poke(1)
          else if t == 11 then dut.x.poke(0)
          else if t == 15 then dut.x.poke(1)
    )

  bothTiers("oracle: prologue and first-step onEntry re-run in order at the wrap-around"): tier =>
    lockstep(
      new PrologueEntryProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 7 then dut.x.poke(1)
          else if t == 13 then dut.x.poke(0)
    )

  bothTiers("oracle: a leading wait leaves the first step's onEntry on its edge"): tier =>
    lockstep(
      new WaitThenEntryProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 4 then dut.x.poke(1)
          else if t == 12 then dut.x.poke(0)
    )

  bothTiers("oracle: entering a nested step fires the parent's onExit and its onEntry"): tier =>
    lockstep(new NestedHookProc, tier, 30, watch = List(_.y, _.z))

  bothTiers("oracle: a hook-carrying first step is not fused away"): tier =>
    lockstep(new FirstStepEntryNestedProc, tier, 20, watch = List(_.y))

  bothTiers("oracle: onExit lands after the wrap-around's re-executed prologue"): tier =>
    lockstep(new ExitOrderProc, tier, 20, watch = List(_.y, _.z))

  bothTiers("oracle: a fallThrough cascade stops at the step it left"): tier =>
    lockstep(
      new FallThroughCycleProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 4 then dut.x.poke(1)
          else if t == 9 then dut.x.poke(0)
    )

  bothTiers("oracle: a fallThrough cascade past the last step re-runs the prologue"): tier =>
    lockstep(
      new PrologueFallThroughProc,
      tier,
      30,
      watch = List(_.y),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(0)
          else if t == 6 then dut.x.poke(1)
          else if t == 14 then dut.x.poke(0)
    )

  bothTiers("oracle: `.din` reads inside a process see only the current state's writes"): tier =>
    lockstep(new RegDINProcDut, tier, 20, watch = List(_.r, _.seen))

  bothTiers("oracle: a FALL_THROUGH for-loop decides on the reset iterator, not the stale one"):
    tier =>
      lockstep(
        new FallThroughForLoopProc,
        tier,
        40,
        watch = List(_.y, _.pass),
        pokes = t =>
          dut =>
            if t == 0 then dut.n.poke(2)
            else if t == 12 then dut.n.poke(0)
            else if t == 18 then dut.n.poke(3)
      )

  bothTiers("oracle: a register-guarded fall-through loop at the forever wrap-around"): tier =>
    lockstep(
      new FallThroughWrapRegLoopProc,
      tier,
      40,
      watch = List(_.x, _.i, _.pass),
      pokes = t =>
        dut =>
          if t == 0 then dut.n.poke(2)
          else if t == 10 then dut.n.poke(0)
          else if t == 16 then dut.n.poke(1)
    )

  bothTiers("oracle: a fallThrough reads the register its own onEntry just assigned"): tier =>
    lockstep(
      new FallThroughOnEntryRegProc,
      tier,
      30,
      watch = List(_.y, _.armed),
      pokes = t =>
        dut =>
          if t == 0 then dut.x.poke(1)
          else if t == 5 then dut.x.poke(0)
          else if t == 14 then dut.x.poke(1)
    )
end RTProcessSimSpec
