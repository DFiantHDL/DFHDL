package dfhdl.compiler.ir

final case class Time(usec: BigDecimal) derives CanEqual
final case class Freq(hertz: BigDecimal) derives CanEqual:
  def period: Time = Time(BigDecimal(1.0e6) / hertz)
final case class Ratio(value: BigDecimal) derives CanEqual

given CanEqual[Time | Ratio, Time | Ratio] = CanEqual.derived
