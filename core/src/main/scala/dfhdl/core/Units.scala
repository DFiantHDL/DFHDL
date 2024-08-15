package dfhdl.core
import dfhdl.compiler.ir
import scala.annotation.targetName

extension (bd: BigDecimal.type)
  private def apply(arg: Int | Double): BigDecimal = arg match
    case i: Int    => BigDecimal(i)
    case d: Double => BigDecimal(d)

opaque type Time <: ir.Time = ir.Time
object Time:
  extension (time: Time) def asIR: ir.Time = time
  extension (time: ir.Time)
    @targetName("asFETime")
    def asFE: Time = time
  object Ops:
    extension (lhs: Int | Double)
      def fs: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.fs)
      def ps: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.ps)
      def ns: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.ns)
      def us: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.us)
      def ms: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.ms)
      def secs: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.secs)
      def mins: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.mins)
      def hours: Time = ir.Time(BigDecimal(lhs), ir.TimeUnits.hours)
    private def commonUnitOp(
        lhs: Time,
        rhs: Time,
        op: (BigDecimal, BigDecimal) => BigDecimal
    ): Time = ir.Time(op(lhs.num, rhs.to(lhs.unit).num), lhs.unit)
    extension (lhs: Time)
      def unary_- : Time = ir.Time(-lhs.num, lhs.unit)
      def +(rhs: Time): Time = commonUnitOp(lhs, rhs, _ + _)
      def -(rhs: Time): Time = commonUnitOp(lhs, rhs, _ - _)
      def *(ratio: Int | Double): Time = ir.Time(lhs.num * BigDecimal(ratio), lhs.unit)
      def /(ratio: Int | Double): Time = ir.Time(lhs.num / BigDecimal(ratio), lhs.unit)
  end Ops
end Time
type Period = Time

opaque type Freq <: ir.Freq = ir.Freq
object Freq:
  extension (freq: Freq) def asIR: ir.Freq = freq
  extension (freq: ir.Freq)
    @targetName("asFEFreq")
    def asFE: Freq = freq
  object Ops:
    extension (lhs: Int | Double)
      def Hz: Freq = ir.Freq(BigDecimal(lhs), ir.FreqUnits.Hz)
      def KHz: Freq = ir.Freq(BigDecimal(lhs), ir.FreqUnits.KHz)
      def MHz: Freq = ir.Freq(BigDecimal(lhs), ir.FreqUnits.MHz)
      def GHz: Freq = ir.Freq(BigDecimal(lhs), ir.FreqUnits.GHz)

type Rate = Freq | Period
extension (rate: Rate) def asIR: ir.Rate = rate.asInstanceOf[ir.Rate]
object Rate:
  extension (rate: ir.Rate)
    @targetName("asFERate")
    def asFE: Rate = rate.asInstanceOf[Rate]
