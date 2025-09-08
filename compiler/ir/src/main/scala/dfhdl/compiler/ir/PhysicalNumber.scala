package dfhdl.compiler.ir
import upickle.default.*
import scala.annotation.targetName
import dfhdl.internals.StableEnum

extension (bd: BigDecimal.type)
  private def apply(arg: Int | Long | Double): BigDecimal = arg match
    case i: Int    => BigDecimal(i)
    case l: Long   => BigDecimal(l)
    case d: Double => BigDecimal(d)

sealed trait PhysicalNumber derives CanEqual:
  val value: BigDecimal
  val unit: PhysicalNumber.Unit
  override def toString(): String = s"${value}.${unit}"

object PhysicalNumber:
  given [T <: PhysicalNumber]: ReadWriter[T] = summon[ReadWriter[Data]].asInstanceOf[ReadWriter[T]]
  sealed trait Unit derives CanEqual
  object Unit:
    given ReadWriter[Unit] = ReadWriter.merge(
      summon[ReadWriter[LiteralNumber.Unit.type]],
      summon[ReadWriter[TimeNumber.Unit]],
      summon[ReadWriter[FreqNumber.Unit]]
    )

  object Ops:
    extension (lhs: Int | Long | Double)
      def fs: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.fs)
      def ps: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.ps)
      def ns: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.ns)
      def us: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.us)
      def ms: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.ms)
      def sec: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.sec)
      def min: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.min)
      def hr: TimeNumber = TimeNumber(BigDecimal(lhs), TimeNumber.Unit.hr)
      def Hz: FreqNumber = FreqNumber(BigDecimal(lhs), FreqNumber.Unit.Hz)
      def KHz: FreqNumber = FreqNumber(BigDecimal(lhs), FreqNumber.Unit.KHz)
      def MHz: FreqNumber = FreqNumber(BigDecimal(lhs), FreqNumber.Unit.MHz)
      def GHz: FreqNumber = FreqNumber(BigDecimal(lhs), FreqNumber.Unit.GHz)
    end extension
  end Ops
  extension (lhs: RateNumber)
    def to_ps: TimeNumber =
      lhs match
        case time: TimeNumber => TimeNumber.to_ps(time)
        case freq: FreqNumber => FreqNumber.to_ps(freq)
    end to_ps
    def to_ns: TimeNumber = TimeNumber(lhs.to_ps.value / BigDecimal(1000), TimeNumber.Unit.ns)
    def to_period: TimeNumber =
      lhs match
        case time: TimeNumber => time
        case freq: FreqNumber => FreqNumber.to_period(freq)
    end to_period
    def to_freq: FreqNumber =
      lhs match
        case time: TimeNumber => TimeNumber.to_hz(time)
        case freq: FreqNumber => freq
    end to_freq
  end extension
end PhysicalNumber

final case class LiteralNumber(value: BigDecimal) extends PhysicalNumber:
  val unit: PhysicalNumber.Unit = LiteralNumber.Unit
object LiteralNumber:
  case object Unit extends PhysicalNumber.Unit:
    given ReadWriter[Unit.type] = macroRW
  extension (lhs: LiteralNumber)
    def /(rhs: TimeNumber): FreqNumber = rhs.to_freq * lhs
    def /(rhs: FreqNumber): TimeNumber = rhs.to_period * lhs
    def /(rhs: LiteralNumber): LiteralNumber = LiteralNumber(lhs.value / rhs.value)
    def *(rhs: LiteralNumber): LiteralNumber = LiteralNumber(lhs.value * rhs.value)
    def +(rhs: LiteralNumber): LiteralNumber = LiteralNumber(lhs.value + rhs.value)
    def -(rhs: LiteralNumber): LiteralNumber = LiteralNumber(lhs.value - rhs.value)

final case class TimeNumber(value: BigDecimal, unit: TimeNumber.Unit) extends PhysicalNumber
object TimeNumber:
  enum Unit extends PhysicalNumber.Unit, StableEnum derives ReadWriter:
    case fs, ps, ns, us, ms, sec, min, hr
  extension (lhs: TimeNumber)
    private def to_psVal: BigDecimal = lhs.unit match
      case TimeNumber.Unit.fs  => lhs.value / BigDecimal(1000)
      case TimeNumber.Unit.ps  => lhs.value
      case TimeNumber.Unit.ns  => lhs.value * BigDecimal(1000)
      case TimeNumber.Unit.us  => lhs.value * BigDecimal(1000000)
      case TimeNumber.Unit.ms  => lhs.value * BigDecimal(1000000000)
      case TimeNumber.Unit.sec => lhs.value * BigDecimal(1000000000000L)
      case TimeNumber.Unit.min => lhs.value * BigDecimal(60000000000000L)
      case TimeNumber.Unit.hr  => lhs.value * BigDecimal(3600000000000000L)
    end to_psVal
    def /(rhs: Int): TimeNumber = TimeNumber(lhs.value / rhs, lhs.unit)
    def /(rhs: LiteralNumber): TimeNumber = TimeNumber(lhs.value / rhs.value, lhs.unit)
    def *(rhs: LiteralNumber): TimeNumber = TimeNumber(lhs.value * rhs.value, lhs.unit)
    def /(rhs: TimeNumber): LiteralNumber = LiteralNumber(lhs.to_psVal / rhs.to_psVal)
    def *(rhs: FreqNumber): LiteralNumber =
      LiteralNumber(lhs.to_psVal * rhs.to_hz.value * BigDecimal(1e12))
    def to_ps: TimeNumber =
      val psVal = to_psVal
      TimeNumber(psVal, TimeNumber.Unit.ps)
    end to_ps
    def to_hz: FreqNumber =
      val hzVal = BigDecimal(1e12) / to_psVal
      FreqNumber(hzVal, FreqNumber.Unit.Hz)
    end to_hz
    def normalize: TimeNumber =
      val psVal = to_psVal
      if psVal < 1000 then TimeNumber(psVal, TimeNumber.Unit.ps)
      else if psVal < 1000000 then TimeNumber(psVal / 1000, TimeNumber.Unit.ns)
      else if psVal < 1000000000 then TimeNumber(psVal / 1000000, TimeNumber.Unit.us)
      else if psVal < 1000000000000L then TimeNumber(psVal / 1000000000L, TimeNumber.Unit.ms)
      else TimeNumber(psVal / 1000000000000L, TimeNumber.Unit.sec)
  end extension
end TimeNumber

final case class FreqNumber(value: BigDecimal, unit: FreqNumber.Unit) extends PhysicalNumber
object FreqNumber:
  enum Unit extends PhysicalNumber.Unit, StableEnum derives ReadWriter:
    case Hz, KHz, MHz, GHz
  extension (lhs: FreqNumber)
    def to_hz: FreqNumber =
      val hzVal = lhs.unit match
        case FreqNumber.Unit.Hz  => lhs.value
        case FreqNumber.Unit.KHz => lhs.value * BigDecimal(1000)
        case FreqNumber.Unit.MHz => lhs.value * BigDecimal(1000000)
        case FreqNumber.Unit.GHz => lhs.value * BigDecimal(1000000000)
      FreqNumber(hzVal, FreqNumber.Unit.Hz)
    end to_hz
    def to_ps: TimeNumber =
      val psVal = BigDecimal(1e12) / to_hz.value
      TimeNumber(psVal, TimeNumber.Unit.ps)
    end to_ps
    def /(rhs: Int): FreqNumber = FreqNumber(lhs.value / rhs, lhs.unit)
    def /(rhs: FreqNumber): LiteralNumber = LiteralNumber(lhs.to_hz.value / rhs.to_hz.value)
    def /(rhs: LiteralNumber): FreqNumber = FreqNumber(lhs.value / rhs.value, lhs.unit)
    def *(rhs: LiteralNumber): FreqNumber = FreqNumber(lhs.value * rhs.value, lhs.unit)
    def *(rhs: TimeNumber): LiteralNumber =
      LiteralNumber(lhs.to_hz.value * rhs.to_ps.value * BigDecimal(1e12))
    def to_period: TimeNumber =
      val psVal = to_ps.value
      if psVal < 1000 then TimeNumber(psVal, TimeNumber.Unit.ps)
      else if psVal < 1000000 then TimeNumber(psVal / 1000, TimeNumber.Unit.ns)
      else if psVal < 1000000000 then TimeNumber(psVal / 1000000, TimeNumber.Unit.us)
      else if psVal < 1000000000000L then TimeNumber(psVal / 1000000000L, TimeNumber.Unit.ms)
      else if psVal < 1000000000000000L then TimeNumber(psVal / 1000000000000L, TimeNumber.Unit.sec)
      else TimeNumber(psVal / 60000000000000L, TimeNumber.Unit.min)
  end extension
end FreqNumber

type RateNumber = TimeNumber | FreqNumber
