package dfhdl.compiler.ir

final case class Time(num: BigDecimal, unit: TimeUnits) derives CanEqual:
  override def toString(): String = s"$num.$unit"
  def to(unit: TimeUnits): Time = Time(num * (unit / this.unit), unit)
enum TimeUnits derives CanEqual:
  case hours, mins, secs, ms, us, ns, ps, fs
object TimeUnits:
  extension (lhs: TimeUnits)
    def toUSRatio: BigDecimal =
      lhs match
        case `hours` => 360e6
        case `mins`  => 60e6
        case `secs`  => 1e6
        case `ms`    => 1e3
        case `us`    => 1
        case `ns`    => 1e-3
        case `ps`    => 1e-6
        case `fs`    => 1e-9
    def /(rhs: TimeUnits): BigDecimal = lhs.toUSRatio / rhs.toUSRatio
type Period = Time
final case class Freq(num: BigDecimal, unit: FreqUnits) derives CanEqual:
  override def toString(): String = s"$num.$unit"
enum FreqUnits derives CanEqual:
  case Hz, KHz, MHz, GHz

type Rate = Period | Freq
given CanEqual[Rate, Rate] = CanEqual.derived

final case class Ratio(value: BigDecimal) derives CanEqual

given CanEqual[Time | Ratio, Time | Ratio] = CanEqual.derived
