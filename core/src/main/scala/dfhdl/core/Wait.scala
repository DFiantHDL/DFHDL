package dfhdl.core

import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName
import ir.DFVal.Func.Op as FuncOp

opaque type Wait <: Unit = Unit
object Wait:
  def apply(trigger: DFValOf[DFBoolOrBit] | Duration)(using DFC): Unit =
    val wait: ir.Wait = ir.Wait(
      trigger.asIR.refTW[ir.Wait],
      dfc.owner.ref,
      dfc.getMeta,
      dfc.tags
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

  object Ops:
    protected type CYInRT = AssertGiven[
      DomainType.RT,
      "`.cy` unit is only allowed under register-transfer (RT) domains."
    ]
    extension (lhs: Int | Long) def cy(using DFC, CYInRT): Cycles = Cycles(lhs)
    extension (lhs: DFValOf[DFUInt[Int]]) def cy(using DFC, CYInRT): Cycles = Cycles(lhs)
    inline def __java_waitErr(): Unit =
      compiletime.error(
        "Did you mean to call DFHDL's `wait`? If so, use `<time>.wait` instead (e.g., `5.ns.wait`).\nDid you mean to call Java's `wait`? if so, use `this.wait` instead."
      )
    inline def __java_waitErr(arg: Long): Unit = __java_waitErr()
    inline def __java_waitErr(arg: Long, arg2: Int): Unit = __java_waitErr()

    extension (lhs: Duration) def wait(using DFC): Wait = trydf { Wait(lhs) }
    def waitWhile(cond: DFValOf[DFBoolOrBit])(using DFC): Wait =
      trydf {
        cond.asIR match
          case ir.DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
            throw new IllegalArgumentException(
              "`waitWhile` does not support rising/falling edges. Use `waitUntil` instead."
            )
          case _ =>
            Wait(cond)
      }
    def waitUntil(trigger: DFValOf[DFBoolOrBit])(using DFC): Wait = trydf {
      trigger.asIR match
        // special case for rising/falling edges, the trigger remains as is inside the wait
        case ir.DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
          Wait(trigger)
        case _ =>
          import DFBoolOrBit.Val.Ops.not
          Wait(trigger.not)
    }
  end Ops
end Wait
