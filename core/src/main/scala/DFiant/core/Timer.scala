package DFiant.core

class Time(value: BigDecimal)
class Timer(period: Time):
  def *(arg: Int): Timer = ???
  def *(arg: Double): Timer = ???
  def /(arg: Int): Timer = ???
  def /(arg: Double): Timer = ???
  def delay(arg: Time): Timer = ???
object Timer:
  object Ops:
    extension (lhs: Int)
      def ps: Time = ???
      def ns: Time = ???
      def us: Time = ???
      def ms: Time = ???
      def units: Time = ???
    extension (lhs: Double)
      def ps: Time = ???
      def ns: Time = ???
      def us: Time = ???
      def ms: Time = ???
      def units: Time = ???
    extension (lhs: Int)
      def Hz: Timer = ???
      def KHz: Timer = ???
      def MHz: Timer = ???
      def GHz: Timer = ???

  end Ops
end Timer
