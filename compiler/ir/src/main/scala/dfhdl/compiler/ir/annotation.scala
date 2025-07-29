package dfhdl.compiler.ir
import dfhdl.compiler.printing.{Printer, HasCodeString}
import dfhdl.internals.*
import upickle.default.*
import dfhdl.internals.StableEnum

object annotation:
  sealed abstract class HWAnnotation extends Product, Serializable, HasCodeString derives CanEqual

  object HWAnnotation:
    given ReadWriter[HWAnnotation] = ReadWriter.merge(
      summon[ReadWriter[Unused]],
      summon[ReadWriter[Pure.type]],
      summon[ReadWriter[FlattenMode]],
      summon[ReadWriter[constraints.Constraint]]
    )

  enum Unused extends HWAnnotation derives ReadWriter:
    case Quiet, Keep, Prune
    def codeString(using Printer): String =
      this match
        case Quiet => "@hw.annotation.unused.quiet"
        case Keep  => "@hw.annotation.unused.keep"
        case Prune => "@hw.annotation.unused.prune"

  case object Pure extends HWAnnotation:
    given ReadWriter[Pure.type] = macroRW
    def codeString(using Printer): String = "@hw.annotation.pure"

  /** Flattening Mode:
    *   - transparent: $memberName
    *   - prefix: $ownerName$sep$memberName
    *   - suffix: $memberName$sep$ownerName
    */
  enum FlattenMode extends HWAnnotation, StableEnum derives ReadWriter:
    case Transparent
    case Prefix(sep: String)
    case Suffix(sep: String)
    def codeString(using Printer): String =
      "@hw.annotation.flattenMode." + (
        this match
          case Transparent => "transparent()"
          case Prefix(sep) => s"""prefix("$sep")"""
          case Suffix(sep) => s"""suffix("$sep")"""
      )
  object FlattenMode:
    val defaultPrefixUnderscore = FlattenMode.Prefix("_")
end annotation

object constraints:
  sealed abstract class Constraint extends annotation.HWAnnotation derives ReadWriter
  sealed abstract class SigConstraint extends Constraint derives ReadWriter:
    val bitIdx: ConfigN[Int]
  final case class Device(name: String, properties: Map[String, String]) extends Constraint
      derives CanEqual, ReadWriter:
    def this(name: String, properties: (String, String)*) =
      this(name, properties.toMap)
    def codeString(using Printer): String =
      val props = properties.map { case (k, v) => s""""$k" -> "$v"""" }.mkString(", ")
      s"""@device("$name"${props.emptyOr(", " + _)})"""

  private def csParam[T](name: String, value: ConfigN[T])(using printer: Printer): String =
    value match
      case None              => ""
      case cs: HasCodeString => s"$name = ${cs.codeString}"
      case str: String       => s"$name = \"$str\""
      case num: FreqNumber   => s"$name = ${printer.csDFFreqData(num)}"
      case num: TimeNumber   => s"$name = ${printer.csDFTimeData(num)}"
      case _                 => s"$name = ${value}"

  final case class IO(
      bitIdx: ConfigN[Int] = None,
      loc: ConfigN[String] = None,
      standard: ConfigN[IO.Standard] = None,
      slewRate: ConfigN[IO.SlewRate] = None,
      driveStrength: ConfigN[Int] = None,
      pullMode: ConfigN[IO.PullMode] = None
  ) extends SigConstraint derives CanEqual, ReadWriter:
    def codeString(using Printer): String =
      val params = List(
        csParam("bitIdx", bitIdx),
        csParam("loc", loc),
        csParam("standard", standard),
        csParam("slewRate", slewRate),
        csParam("driveStrength", driveStrength),
        csParam("pullMode", pullMode)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@io($params)"""
    end codeString
  end IO
  object IO:
    enum Standard extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case LVCMOS33, LVCMOS25, LVCMOS18
      def codeString(using Printer): String = "io.Standard." + this.toString
    enum SlewRate extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case SLOW, FAST
      def codeString(using Printer): String = "io.SlewRate." + this.toString
    enum PullMode extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case UP, DOWN
      def codeString(using Printer): String = "io.PullMode." + this.toString

  object Timing:
    final case class Ignore(
        bitIdx: ConfigN[Int] = None,
        maxFreqMinPeriod: ConfigN[RateNumber] = None
    ) extends SigConstraint
        derives CanEqual, ReadWriter:
      def codeString(using Printer): String =
        val params = List(
          csParam("bitIdx", bitIdx),
          csParam("maxFreqMinPeriod", maxFreqMinPeriod)
        ).filter(_.nonEmpty).mkString(", ")
        s"""@timing.ignore($params)"""

    final case class Clock(
        rate: RateNumber
    ) extends Constraint derives CanEqual, ReadWriter:
      def codeString(using Printer): String =
        s"""@timing.clock(${csParam("rate", rate)})"""
  end Timing
end constraints
