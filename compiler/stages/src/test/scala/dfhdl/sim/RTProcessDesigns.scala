package dfhdl.sim
import dfhdl.*

/** Period-2 toggle: two single-cycle waits with a convertible (const) prologue. The prologue folds
  * into time-zero state and re-executes on every forever wrap-around.
  */
class ToggleProc extends RTDesign:
  val x = Bit <> OUT.REG init 0
  process:
    x.din := 1
    1.cy.wait
    x.din := 0
    1.cy.wait

/** The wait-composition equivalence trio (cycle-semantics Rule 3): a flat wait, a loop of waits,
  * and nested loops of waits with the same total must be cycle-identical. A leading single-cycle
  * wait keeps the loops off the process-leading position (the reset-entry state).
  */
class WaitEqFlat extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    12.cy.wait
    tick.din := !tick

class WaitEqLoop extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    for (i <- 0 until 3)
      4.cy.wait
    tick.din := !tick

class WaitEqNested extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    for (i <- 0 until 3)
      for (j <- 0 until 2)
        2.cy.wait
    tick.din := !tick

/** Guard sampling: each iteration streams one value with exactly one cycle; the loop exit fuses
  * with the final iteration, so `done` rises in the cycle right after the last element.
  */
class StreamerProc extends RTDesign:
  val y = SInt(16) <> OUT.REG init 0
  val done = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    for (i <- 0 until 4)
      y.din := i
      1.cy.wait
    done.din := 1
    1.cy.wait
    done.din := 0

/** The canonical loop-first process (the ToED end-to-end shape): the loop control fuses at the
  * forever wrap-around (its self-goto resolves through the re-initialized iterator) and the reset
  * bootstrap folds into time-zero state, so each iteration costs exactly one cycle and the process
  * starts with zero bootstrap cycles (a single-state FSM).
  */
class LoopFirstProc extends RTDesign:
  val y = SInt(16) <> OUT.REG init 0
  process:
    for (i <- 0 until 4)
      y.din := i
      1.cy.wait

/** A leading loop statically skipped at reset (empty range): the reset fold lands past the loop's
  * own wait, which the FSM lowering cannot make its entry state, so a one-time reset bootstrap
  * state is kept (one cycle at process start only).
  */
class SkipLoopFirstProc extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    for (i <- 0 until 0)
      1.cy.wait
    2.cy.wait
    tick.din := !tick

/** Empty (park) loops: each pass of a control-free loop parks per iteration plus the final
  * guard-false detection cycle.
  */
class EmptyLoopsProc extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    for (i <- 0 until 3)
      for (j <- 0 until 2) {}
    tick.din := !tick

/** `while` with a constant-false guard parks exactly one cycle per loop (enter, sample, skip). */
class WhileFalseProc extends RTDesign:
  val F: Boolean <> CONST = false
  val cnt = UInt(8) <> OUT.REG init 0
  process:
    1.cy.wait
    while (F) {}
    while (F) {}
    while (F) {}
    cnt.din := cnt + 1

/** Dynamic cycle wait: the count is sampled live, like the equivalent FSM counter compare. */
class DynWaitProc extends RTDesign:
  val n = UInt(8) <> IN
  val tick = Bit <> OUT.REG init 0
  process:
    n.cy.wait
    tick.din := !tick

/** Condition wait: samples once per cycle (at least one cycle), then a timed busy pulse. */
class CondWaitProc extends RTDesign:
  val go = Bit <> IN
  val busy = Bit <> OUT.REG init 0
  process:
    waitUntil(go)
    busy.din := 1
    3.cy.wait
    busy.din := 0

/** A while loop with a control-free body is a per-cycle guard sample - waitUntil equivalence. */
class WhileVsWaitProc extends RTDesign:
  val go = Bit <> IN
  val busy = Bit <> OUT.REG init 0
  process:
    while (!go) {}
    busy.din := 1
    3.cy.wait
    busy.din := 0

/** Run-once sequence ending in an endless wait: the FSM halts in a terminal state. */
class RunOnceProc extends RTDesign:
  val out = UInt(8) <> OUT.REG init 0
  process:
    out.din := 5
    2.cy.wait
    out.din := 42
    wait

/** A very long wait - the scheduler's skip-ahead target. */
class BigWaitProc extends RTDesign:
  val tick = Bit <> OUT.REG init 0
  process:
    1000000.cy.wait
    tick.din := !tick

/** The user guide's step FSM: pure-dispatch steps with if-dispatch, self-transitions, and
  * explicit/relative jumps.
  */
class SimpleFSMProc extends RTDesign:
  val x = Bit <> IN
  val y = Bit <> OUT.REG init 0
  process:
    def S0: Step =
      y.din := 0
      if (x) NextStep else S0
    def S1: Step =
      y.din := 1
      if (x) S2 else FirstStep
    def S2: Step =
      y.din := 0
      if (x) ThisStep else FirstStep

/** Nested first steps fuse into a shared entry cycle (the innermost pure step is the park). */
class NestedStepsProc extends RTDesign:
  val a = UInt(4) <> OUT.REG init 0
  val b = UInt(4) <> OUT.REG init 0
  val c = UInt(4) <> OUT.REG init 0
  process:
    def S1: Step =
      a.din := a + 1
      def S2: Step =
        b.din := b + 1
        def S3: Step =
          c.din := c + 1
          NextStep
        NextStep
      NextStep

/** A loop whose guard reads a register assigned conditionally in the body tail: the loop-back
  * cannot forward the guard value and keeps a control state (one extra cycle per iteration).
  */
class DirtyGuardProc extends RTDesign:
  val sel = Bit <> IN
  val acc = UInt(8) <> OUT.REG init 0
  process:
    1.cy.wait
    while (acc < 10)
      1.cy.wait
      if (sel) acc.din := acc + 3
      else acc.din := acc + 1
    acc.din := 0

/** A dynamic-count inner loop nested in a static outer loop: the re-entry dispatch cycle cannot
  * fold, so a control state is kept (the outer control, per the FSM lowering's victim order).
  */
class DynNestProc extends RTDesign:
  val n = Int <> IN
  val y = Bit <> OUT.REG init 0
  process:
    def S0: Step = NextStep
    for (i <- 0 until 2)
      for (j <- 0 until n)
        2.cy.wait
      y.din := !y

/** A single dynamic loop still fuses: a zero-count run skips the loop with zero cycles. */
class DynLoopProc extends RTDesign:
  val n = Int <> IN
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    for (i <- 0 until n)
      1.cy.wait
    tick.din := !tick

/** Match-based step dispatch (kept in the step's own parked cycle). */
class MatchDispatchProc extends RTDesign:
  val sel = UInt(2) <> IN
  val y = UInt(2) <> OUT.REG init 0
  process:
    def SA: Step =
      y.din := 1
      sel match
        case 0 => SA
        case _ => NextStep
    def SB: Step =
      y.din := 2
      NextStep

/** A non-convertible prologue (non-constant right-hand side) keeps the bootstrap state. */
class BootProc extends RTDesign:
  val a = UInt(8) <> IN
  val x = UInt(8) <> OUT.REG init 0
  process:
    x.din := a
    2.cy.wait

/** The convertible twin of [[BootProc]]: a constant prologue costs no bootstrap cycle. */
class NoBootProc extends RTDesign:
  val x = UInt(8) <> OUT.REG init 0
  process:
    x.din := 5
    2.cy.wait

/** The trailing-share gate: a trailing statement assigns a prologue-assigned register, so the
  * wrap-around re-initialization must not shadow it - the bootstrap state is kept and the trailing
  * value stays observable for one cycle.
  */
class ShareGateProc extends RTDesign:
  val x = UInt(8) <> OUT.REG init 0
  process:
    x.din := 0
    3.cy.wait
    x.din := 7

/** Two independent processes with co-prime periods, combined combinationally. */
class TwoProcs extends RTDesign:
  val a = Bit <> OUT.REG init 0
  val b = Bit <> OUT.REG init 0
  val both = Bit <> OUT
  process:
    2.cy.wait
    a.din := !a
  process:
    3.cy.wait
    b.din := !b
  both := a ^ b

enum TxState extends Encoded:
  case Idle, Busy

/** Prints and reports at distinct FSM transition points: message arguments (decimal, hex bits,
  * boolean, enum entry) render from the fired transition cycle's settled values. Period 6: wait(2) -
  * println - wait(1) - report - wait(3) - wrap (the constant prologue folds).
  */
class PrintFlowProc extends RTDesign:
  val x = UInt(8) <> OUT.REG init 0
  val b = Bits(12) <> OUT.REG init h"abc"
  val f = Boolean <> OUT.REG init false
  val st = TxState <> OUT.REG init TxState.Idle
  process:
    x.din := 1
    2.cy.wait
    println(s"tick x=$x b=$b f=$f st=$st")
    x.din := x + 7
    f.din := true
    st.din := TxState.Busy
    1.cy.wait
    report(s"mid x=$x")
    3.cy.wait
end PrintFlowProc

/** A per-pass assertion that fails once the counter crosses its threshold (severity is a parameter
  * for the policy tests): boot(1) + wait(1) per pass, checked on the wait cycle. The first failure
  * fires at cycle 6 (cnt = 3) and every second cycle after.
  */
class CountAssertProc(severity: Severity = Severity.Error) extends RTDesign:
  val cnt = UInt(8) <> OUT.REG init 0
  process:
    cnt.din := cnt + 1
    1.cy.wait
    assert(cnt < 3, s"cnt reached $cnt", severity)

/** A concurrent (design-body) assertion: checked on every cycle under its own condition only,
  * alongside body register logic.
  */
class BodyAssertDesign extends RTDesign:
  val cnt = UInt(8) <> OUT.REG init 0
  cnt.din := cnt + 1
  assert(cnt < 5, s"cnt reached $cnt", Severity.Warning)

/** Three constant-false `while` parks and then `finish()`, fused into the third park's exit path
  * (the run ends during cycle 3, one cycle per skipped loop).
  */
class FinishProc extends RTDesign:
  val F: Boolean <> CONST = false
  process:
    while (F) {}
    while (F) {}
    while (F) {}
    finish()

/** Debug output inside a process: the design path, the source position, and `name = value` lines.
  * Period 2: wait(2) - debug - increment - wrap.
  */
class DebugProc extends RTDesign:
  val a = UInt(8) <> OUT.REG init 3
  process:
    2.cy.wait
    debug(a)
    a.din := a + 1

/** A `FALL_THROUGH` while loop with a park body (no wait): when `go` is false on entry the loop is
  * skipped with zero cycles (the fall-through cascade to the following wait); while `go` holds,
  * each cycle counts one iteration. A wait follows the loop so the fall-through target is a clean
  * park.
  */
class FallThroughWhileProc extends RTDesign:
  val go = Bit <> IN
  val cnt = UInt(8) <> OUT.REG init 0
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    FALL_THROUGH:
      while (go)
        cnt.din := cnt + 1
    1.cy.wait
    tick.din := !tick

/** A step FSM with `onEntry` and `onExit` hooks (the user-guide example): `onEntry` runs on
  * entering a step from a different step, `onExit` on leaving it; neither fires on a
  * self-transition.
  */
class HookFSMProc extends RTDesign:
  val x = Bit <> IN
  val y = Bit <> OUT.REG init 0
  process:
    def S0: Step =
      y.din := 0
      if (x) S1 else S0
    def S1: Step =
      def onEntry =
        y.din := 1
      if (x) S2 else S0
    def S2: Step =
      def onExit =
        y.din := 0
      if (x) ThisStep else S0
end HookFSMProc

/** A step FSM with `fallThrough` hooks: when a step's fall-through condition holds on entry the FSM
  * advances to the next step in the same cycle (a conditional zero-cycle chain), running each
  * traversed step's `onEntry`.
  */
class FallThroughStepProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      y.din := 1
      NextStep
    def S1: Step =
      def onEntry =
        y.din := y + 1
      def fallThrough = x
      NextStep
    def S2: Step =
      def onEntry =
        y.din := y + 2
      def fallThrough = !x
      NextStep
    def S3: Step =
      y.din := y + 4
      FirstStep
end FallThroughStepProc

/** A `FALL_THROUGH` while loop with an empty body: exercises the empty-body branch of the loop
  * lowering. `go` false on entry falls through with zero cycles to the following wait; `go` true
  * parks one cycle per sample until it drops (a `waitUntil(!go)` with a zero-cycle skip).
  */
class FallThroughEmptyWhileProc extends RTDesign:
  val go = Bit <> IN
  val tick = Bit <> OUT.REG init 0
  process:
    1.cy.wait
    FALL_THROUGH:
      while (go) {}
    1.cy.wait
    tick.din := !tick

/** The first step's `onEntry` is process-prologue content: being initial-convertible it costs no
  * cycle at process start (it lands in the generated `initial` block, superseding the declaration
  * init of `y`), and it re-runs on every forever wrap-around back into the first step.
  */
class FirstStepEntryProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      def onEntry =
        y.din := 1
      if (x) NextStep else ThisStep
    def S1: Step =
      y.din := y + 1
      NextStep

/** A first-step `onEntry` that is *not* initial-convertible keeps the synthetic bootstrap state: it
  * fires on the bootstrap -> first-step edge, costing one cycle at process start and one on every
  * wrap-around (which returns to the bootstrap state).
  */
class FirstStepEntryBootProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      def onEntry =
        y.din := y + 1
      if (x) NextStep else ThisStep
    def S1: Step =
      y.din := y + 2
      NextStep

/** Leading statements and the first step's `onEntry` together form the prologue: both fold into the
  * time-zero state, and both re-execute (leading statements first, then `onEntry`) at the
  * wrap-around. An explicit jump to the first step runs the `onEntry` but not the leading
  * statements, which is what `y` distinguishes here.
  */
class PrologueEntryProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  val z = UInt(8) <> OUT.REG init 0
  process:
    y.din := 3
    def S0: Step =
      def onEntry =
        z.din := 7
      NextStep
    def S1: Step =
      y.din := y + 1
      z.din := z + 1
      if (x) FirstStep else NextStep
    def S2: Step =
      y.din := y + 2
      NextStep
end PrologueEntryProc

/** With a leading wait the process's first state is that wait, so the first step's `onEntry` is not
  * folded into the time-zero state: it fires on the wait -> step edge like any other transition.
  */
class WaitThenEntryProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    1.cy.wait
    def S0: Step =
      def onEntry =
        y.din := 1
      if (x) NextStep else ThisStep
    def S1: Step =
      y.din := y + 1
      NextStep

/** Hooks on a nested step: entering it from its parent is a real FSM edge, so the parent's `onExit`
  * and the nested step's `onEntry` both fire there. Neither step fuses (a hook-carrying step is
  * never a first-step-fusion candidate), so each keeps its own state.
  */
class NestedHookProc extends RTDesign:
  val y = UInt(8) <> OUT.REG init 0
  val z = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      def onExit =
        y.din := y + 9
      def Inner: Step =
        def onEntry =
          z.din := z + 5
        1.cy.wait
        NextStep
      NextStep
    def S1: Step =
      y.din := y + 1
      NextStep
end NestedHookProc

/** A first step whose `onEntry` is folded into the time-zero state while its own first action is a
  * nested step: the hook keeps it out of first-step fusion, so it stays a real state of its own.
  */
class FirstStepEntryNestedProc extends RTDesign:
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      def onEntry =
        y.din := 1
      def Inner: Step =
        y.din := y + 8
        NextStep
      NextStep
    def S1: Step =
      y.din := y + 1
      NextStep

/** Every step carries a `fallThrough`, so a cascade can travel the whole ring: it stops only when
  * it comes back around to the step the transition left, which it still enters (`onEntry` and state
  * write) before stopping.
  */
class FallThroughCycleProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      def onEntry =
        y.din := 1
      def fallThrough = x
      NextStep
    def S1: Step =
      def onEntry =
        y.din := 2
      def fallThrough = x
      NextStep
    def S2: Step =
      def onEntry =
        y.din := 4
      def fallThrough = x
      NextStep
end FallThroughCycleProc

/** A fall-through cascade past the last step is a wrap-around: the leading statements re-execute
  * before the first step's `onEntry`, while the skipped step's own body does not run at all.
  */
class PrologueFallThroughProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  val z = UInt(8) <> OUT.REG init 0
  process:
    y.din := 3
    def S0: Step =
      def onEntry =
        z.din := 7
      NextStep
    def S1: Step =
      y.din := y + 1
      NextStep
    def S2: Step =
      def fallThrough = x
      z.din := z + 1
      NextStep
end PrologueFallThroughProc

/** A transition's hooks are planted at its goto site, which sits *after* the wrap-around's
  * re-executed leading statements: `y` ends the wrap cycle holding the `onExit` value, not the
  * prologue one.
  */
class ExitOrderProc extends RTDesign:
  val y = UInt(8) <> OUT.REG init 0
  val z = UInt(8) <> OUT.REG init 0
  process:
    y.din := 3
    def S0: Step =
      z.din := z + 1
      NextStep
    def S1: Step =
      def onExit =
        y.din := 9
      NextStep

/** A `fallThrough` step whose exit goto names a step that is not the next one declared. The
  * execution order S0 -> S1 -> S3 -> S2 -> S0 differs from the declaration order, so a cascade out
  * of S1 must follow S1's own `S3` goto and run S3's `onEntry`, not the `onEntry` of the S2 that
  * merely follows it in the state list.
  */
class FallThroughOutOfOrderProc extends RTDesign:
  val x = Bit <> IN
  val y = UInt(8) <> OUT.REG init 0
  process:
    def S0: Step =
      y.din := 1
      NextStep
    def S1: Step =
      def onEntry =
        y.din := y + 2
      def fallThrough = x
      S3
    def S2: Step =
      def onEntry =
        y.din := y + 4
      def fallThrough = !x
      FirstStep
    def S3: Step =
      def onEntry =
        y.din := y + 8
      S2
end FallThroughOutOfOrderProc
