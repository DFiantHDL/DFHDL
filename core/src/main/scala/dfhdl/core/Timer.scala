package dfhdl.core
import dfhdl.compiler.ir
import ir.{Time, Freq, Ratio}
import ir.Timer.Func.Op as FuncOp
import dfhdl.internals.*
import scala.annotation.targetName

sealed class Timer private (val irValue: ir.Timer | DFError) extends DFMember[ir.Timer]
object Timer:
  extension (timer: ir.Timer) private def asFE: Timer = new Timer(timer)
  type Period = Time | Freq
  extension (period: Period)
    def time = period match
      case t: Time => t
      case f: Freq => f.period

  def apply()(using DFC): Timer = Periodic(None, None)
  @targetName("applyPeriod")
  def apply(period: Period)(using DFC): Timer = Periodic(None, Some(period.time))
  def apply(trigger: DFValOf[DFBit])(using DFC): Timer = Periodic(Some(trigger), None)
  def apply(trigger: DFValOf[DFBit], period: Period)(using DFC): Timer =
    Periodic(Some(trigger), Some(period.time))
  extension (bd: BigDecimal.type)
    private def apply(arg: Int | Double): BigDecimal = arg match
      case i: Int    => BigDecimal(i)
      case d: Double => BigDecimal(d)
//  trait Literal[T]:
//    def apply(arg: T): BigDecimal
//  object Literal:
//    given fromInt[T <: Int]: Literal[T] with
//      def apply(arg: T): BigDecimal = BigDecimal(arg)
//    given fromDouble[T <: Double]: Literal[T] with
//      def apply(arg: T): BigDecimal = BigDecimal(arg)
  object Ops:
    extension (timer: Timer)
      def *(ratio: Int | Double)(using DFC): Timer =
        Timer.Func(timer, FuncOp.`*`, Ratio(BigDecimal(ratio)))
      def /(ratio: Int | Double)(using DFC): Timer =
        Timer.Func(timer, FuncOp./, Ratio(BigDecimal(ratio)))
      def delay(arg: Time)(using DFC): Timer =
        Timer.Func(timer, FuncOp.Delay, arg)
      def isActive(using DFC): DFValOf[DFBool] =
        Timer.IsActive(timer)
    extension (lhs: Int | Double)
      def ps: Time = Time(BigDecimal(lhs) * 1e6)
      def ns: Time = Time(BigDecimal(lhs) * 1e3)
      def us: Time = Time(BigDecimal(lhs))
      def ms: Time = Time(BigDecimal(lhs) / 1e3)
      def sec: Time = Time(BigDecimal(lhs) / 1e6)
    extension (lhs: Int | Double)
      def Hz: Freq = Freq(BigDecimal(lhs))
      def KHz: Freq = Freq(BigDecimal(lhs) * 1e3)
      def MHz: Freq = Freq(BigDecimal(lhs) * 1e6)
      def GHz: Freq = Freq(BigDecimal(lhs) * 1e9)
  end Ops

  object Periodic:
    def apply(trigger: Option[DFValOf[DFBit]], periodOpt: Option[Time])(using DFC): Timer =
      val triggerRef: ir.Timer.TriggerRef = trigger match
        case Some(value) => value.asIR.refTW[ir.Timer]
        case None        => ir.DFRef.TwoWay.Empty
      val timer: ir.Timer = ir.Timer
        .Periodic(
          triggerRef,
          periodOpt,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      timer.asFE
  end Periodic
  object Func:
    def apply(source: Timer, op: FuncOp, arg: Time | Ratio)(using DFC): Timer =
      val timer: ir.Timer = ir.Timer
        .Func(
          source.asIR.refTW[ir.Timer],
          op,
          arg,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      timer.asFE

  object IsActive:
    def apply(timer: Timer)(using DFC): DFValOf[DFBool] =
      val dfVal: ir.DFVal = ir.Timer
        .IsActive(
          timer.asIR.refTW[ir.DFVal],
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      dfVal.asValOf[DFBool]
end Timer
