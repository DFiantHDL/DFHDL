package DFiant.core
import DFiant.internals.*
class Time(value: BigDecimal)
class Freq(value: BigDecimal)
sealed trait Timer:
  def *[T <: Int | Double](arg: Inlined[T]): Timer = ???
  def /[T <: Int | Double](arg: Inlined[T]): Timer = ???
  def isActive: DFValOf[DFBool] = ???
  def delay(arg: Time): Timer = ???

object Timer:
  def apply(freq: Freq): Timer = ???
  def apply(period: Time): Timer = ???
  final case class Ratio()
  final class Periodic(period: Time) extends Timer
  final class DerivedDelay(origin: Timer, delay: Time) extends Timer
  final class DerivedRatio(origin: Timer, ratio: Ratio) extends Timer
  final class FromDFBit(bit: DFValOf[DFBit]) extends Timer
  final class Unspecified() extends Timer
  extension (bit: DFValOf[DFBit]) def asTimer: Timer = ???
  trait Literal[T]:
    def apply(arg: T): BigDecimal
  object Literal:
    given fromInt[T <: Int]: Literal[T] with
      def apply(arg: T): BigDecimal = BigDecimal(arg)
    given fromDouble[T <: Double]: Literal[T] with
      def apply(arg: T): BigDecimal = BigDecimal(arg)
  object Ops:
    extension (lhs: Int | Double)
      def ps: Time = ???
      def ns: Time = ???
      def us: Time = ???
      def ms: Time = ???
      def units: Time = ???
    extension (lhs: Int | Double)
      def Hz: Freq = ???
      def KHz: Freq = ???
      def MHz: Freq = ???
      def GHz: Freq = ???

  end Ops
end Timer
