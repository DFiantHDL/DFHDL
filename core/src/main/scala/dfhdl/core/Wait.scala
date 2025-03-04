package dfhdl.core

import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import ir.DFVal.Func.Op as FuncOp

//sealed class Wait private (val irValue: ir.Wait | DFError) extends DFMember[ir.Wait]
object Wait:
  def apply(trigger: DFValOf[DFBool] | DFConstOf[Duration])(using DFC): Unit =
    val wait: ir.Wait = ir.Wait(
      trigger.asIR.refTW[ir.Wait],
      dfc.owner.ref,
      dfc.getMeta,
      dfc.tags
    ).addMember
  object Ops:
    extension (lhs: DFConstOf[Duration]) def wait(using DFC): Unit = trydf { Wait(lhs) }
    def waitWhile(cond: DFValOf[DFBool])(using DFC): Unit =
      trydf {
        cond.asIR match
          case ir.DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _) =>
            throw new IllegalArgumentException(
              "`waitWhile` does not support rising/falling edges. Use `waitUntil` instead."
            )
          case _ =>
            Wait(cond)
      }
    def waitUntil(trigger: DFValOf[DFBool])(using DFC): Unit = trydf {
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
