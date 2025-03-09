package dfhdl.core

import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import ir.DFVal.Func.Op as FuncOp

opaque type Wait <: Unit = Unit
object Wait:
  def apply(trigger: DFValOf[DFBool] | DFConstOf[Duration])(using DFC): Unit =
    val wait: ir.Wait = ir.Wait(
      trigger.asIR.refTW[ir.Wait],
      dfc.owner.ref,
      dfc.getMeta,
      dfc.tags
    ).addMember
  object Ops:
    inline def __java_waitErr(): Unit =
      compiletime.error(
        "Did you mean to call DFHDL's `wait`? If so, use `<time>.wait` instead (e.g., `5.ns.wait`).\nDid you mean to call Java's `wait`? if so, use `this.wait` instead."
      )
    inline def __java_waitErr(arg: Long): Unit = __java_waitErr()
    inline def __java_waitErr(arg: Long, arg2: Int): Unit = __java_waitErr()

    extension (lhs: DFConstOf[Duration]) def wait(using DFC): Wait = trydf { Wait(lhs) }
    def waitWhile(cond: DFValOf[DFBool])(using DFC): Wait =
      trydf {
        cond.asIR match
          case ir.DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _) =>
            throw new IllegalArgumentException(
              "`waitWhile` does not support rising/falling edges. Use `waitUntil` instead."
            )
          case _ =>
            Wait(cond)
      }
    def waitUntil(trigger: DFValOf[DFBool])(using DFC): Wait = trydf {
      trigger.asIR match
        // special case for rising/falling edges, the trigger remains as is inside the wait
        case ir.DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _) =>
          Wait(trigger)
        case _ =>
          import DFBoolOrBit.Val.Ops.unary_!
          Wait(!trigger)
    }
  end Ops
end Wait
