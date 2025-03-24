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

  // Since Java's wait belongs to the Object class, we need to be able to override it
  // with our own wait method, so we need to extend this in the Container trait, instead
  // of relying on export like the rest of the core API.
  trait ContainerOps:
    extension (lhs: Duration) final def wait(using DFC): Wait = trydf { Wait(lhs) }
    inline def java_wait(): Unit = this.wait()
    inline def java_wait(timeoutMillis: Long): Unit = this.wait(timeoutMillis)
    inline def java_wait(timeoutMillis: Long, nanos: Int): Unit = this.wait(timeoutMillis, nanos)
  object Ops:
    protected type CYInRT = AssertGiven[
      DomainType.RT,
      "`.cy` unit is only allowed under register-transfer (RT) domains."
    ]
    extension (lhs: Int | Long)
      def cy(using DFC, CYInRT): Cycles = trydf {
        val pos = lhs match
          case long: Long => long > 0
          case int: Int   => int > 0
        if (!pos)
          throw new IllegalArgumentException("`cy` can only be used with positive values.")
        Cycles(lhs)
      }
    extension (lhs: DFValOf[DFUInt[Int]]) def cy(using DFC, CYInRT): Cycles = Cycles(lhs)

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
