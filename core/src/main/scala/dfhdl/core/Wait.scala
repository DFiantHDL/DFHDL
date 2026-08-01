package dfhdl.core

import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import ir.DFVal.Func.Op as FuncOp

opaque type Wait <: Unit = Unit
object Wait:
  def apply(trigger: DFValOf[DFBoolOrBit] | Duration, fallThrough: Boolean = false)(using
      DFC
  ): Unit =
    val wait: ir.Wait = ir.Wait(
      trigger.asIR.refTW[ir.Wait],
      dfc.owner.ref,
      dfc.getMeta,
      if (fallThrough) dfc.tags.tag(ir.FallThroughTag) else dfc.tags
    ).addMember
  opaque type Cycles <: DFValOf[DFUInt[Int]] = DFValOf[DFUInt[Int]]
  object Cycles:
    def apply(value: Int | Long)(using DFC): Cycles =
      val bigInt = value match
        case i: Int  => BigInt(i)
        case l: Long => BigInt(l)
      DFVal.Const(DFUInt(bigInt.bitsWidth(signed = false)), Some(bigInt), named = true)
    def apply(value: DFValOf[DFUInt[Int]])(using DFC): Cycles = value
  type Duration = DFConstOf[DFTime] | Cycles

  protected type CYInRT = AssertGiven[
    DomainType.RT,
    "`.cy` unit wait is only allowed under register-transfer (RT) domains."
  ]
  // `wait` blocks on time, so it needs a scope that can do so: `HasWait`, which only `Process` and
  // `Procedural` have. A design or domain body (`Concurrent`) does not, and neither does an
  // `initial` block (a `Sequence`, deliberately NOT a `TimedSequence`) nor a function body. This
  // holds under RT too: RT sequential code, `wait(1.cy)` included, lives inside a process.
  //
  // This makes "no `wait` inside an `initial` block" a COMPILE error. The elaboration check in
  // `DB.initialCheck` stays as the backstop, since a helper `def` can still launder the scope
  // evidence past a type-level guard.
  protected type InWaitScope = AssertGiven[
    DFC.Scope.HasWait,
    "`wait` statements are only allowed inside a process or a procedural (task) method body.\nThey are not allowed in a design or domain body, in an `initial` block, or in a function method body."
  ]
  // Since Java's wait belongs to the Object class, we need to be able to override it
  // with our own wait method, so we need to extend this in the Container trait, instead
  // of relying on export like the rest of the core API.
  trait ContainerOps:
    // An endless wait: modeled as a wait on an anonymous constant `false` trigger, which never
    // resumes (`ir.Wait(X)` resumes when X becomes true). A bare `wait` statement resolves here
    // (Java's `Object.wait()` requires explicit parentheses, so there is no ambiguity).
    final def wait(using DFC, InWaitScope): Unit = trydf {
      Wait(DFVal.Const(DFBool, Some(false)))
    }
    final def wait(lhs: DFConstOf[DFTime])(using DFC, InWaitScope): Unit = trydf { Wait(lhs) }
    final def wait(lhs: Cycles)(using DFC, CYInRT, InWaitScope): Unit = trydf { Wait(lhs) }
    inline def java_wait(): Unit = this.wait()
    inline def java_wait(timeoutMillis: Long): Unit = this.wait(timeoutMillis)
    inline def java_wait(timeoutMillis: Long, nanos: Int): Unit = this.wait(timeoutMillis, nanos)
    export TextOut.Ops.assert
  // `ir.Wait(X)` means "block until X becomes true" (resume when X is true). So a `waitUntil`
  // stores the trigger as-is, and a `waitWhile` stores its negation. `fallThrough` marks the wait
  // as costing no cycle when its condition already holds on entry.
  private def untilImpl(trigger: DFValOf[DFBoolOrBit], fallThrough: Boolean)(using DFC): Wait =
    Wait(trigger, fallThrough)
  private def whileImpl(cond: DFValOf[DFBoolOrBit], fallThrough: Boolean)(using DFC): Wait =
    cond.asIR match
      case ir.DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
        throw new IllegalArgumentException(
          "`waitWhile` does not support rising/falling edges. Use `waitUntil` instead."
        )
      case _ =>
        import DFBoolOrBit.Val.Ops.not
        Wait(cond.not, fallThrough)
  // The compiler plugin replaces `waitUntil(FALL_THROUGH(cond))` and `waitWhile(FALL_THROUGH(cond))`
  // with a call to this entry point, so the mark reaches the `Wait` member at construction and never
  // touches the condition value itself. The wait-scope evidence that the original call required has
  // already been checked by then, so it is not repeated here.
  def plugin(cond: DFValOf[DFBoolOrBit], isUntil: Boolean, fallThrough: Boolean)(using DFC): Wait =
    trydf { if (isUntil) untilImpl(cond, fallThrough) else whileImpl(cond, fallThrough) }
  object Ops:
    extension (lhs: Int | Long)
      def cy(using DFCG): Cycles = trydf {
        val pos = lhs match
          case long: Long => long > 0
          case int: Int   => int > 0
        if (!pos)
          throw new IllegalArgumentException("`cy` can only be used with positive values.")
        Cycles(lhs)
      }
    extension (lhs: DFValOf[DFUInt[Int]]) def cy(using DFCG): Cycles = Cycles(lhs)

    // A `FALL_THROUGH` mark on the condition (`waitUntil(FALL_THROUGH(cond))`) is consumed by the
    // compiler plugin, which routes the call to `Wait.plugin` instead of here.
    def waitWhile(cond: DFValOf[DFBoolOrBit])(using DFC, InWaitScope): Wait =
      trydf { whileImpl(cond, fallThrough = false) }
    def waitUntil(trigger: DFValOf[DFBoolOrBit])(using DFC, InWaitScope): Wait =
      trydf { untilImpl(trigger, fallThrough = false) }
  end Ops
end Wait
