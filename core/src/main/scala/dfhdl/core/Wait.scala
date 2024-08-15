package dfhdl.core

import dfhdl.compiler.ir
import ir.Time
import dfhdl.internals.*
import scala.annotation.targetName

//sealed class Wait private (val irValue: ir.Wait | DFError) extends DFMember[ir.Wait]
object Wait:
//  extension (wait: ir.Wait) private def asFE: Wait = new Wait(wait)
  object Duration:
    def apply(timeOpt: Option[Time])(using DFC): Unit =
      val wait: ir.Wait = ir.Wait
        .Duration(
          timeOpt,
          dfc.owner.ref,
          dfc.getMeta,
          dfc.tags
        )
        .addMember
  object Until:
    def apply(trigger: DFValOf[DFBit])(using DFC): Unit =
      val wait: ir.Wait = ir.Wait
        .Until(
          trigger.asIR.refTW[ir.Wait],
          dfc.owner.ref,
          dfc.getMeta,
          dfc.tags
        )
        .addMember
  object Ops:
    def wait(time: Time)(using DFC): Unit = trydf { Duration(Some(time)) }
    def waitForever(using DFC): Unit = trydf { Duration(None) }
    def waitUntil(trigger: DFValOf[DFBit])(using DFC): Unit = trydf { Until(trigger) }
    def waitWhile(trigger: DFValOf[DFBit])(using DFC): Unit = trydf {
      import DFBoolOrBit.Val.Ops.unary_!
      Until(!trigger)
    }
end Wait
