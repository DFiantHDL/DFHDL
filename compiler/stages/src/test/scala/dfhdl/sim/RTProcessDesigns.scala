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
