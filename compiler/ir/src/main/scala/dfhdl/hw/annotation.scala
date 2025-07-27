/** this package contains all the dfhdl hardware annotations
  */
package dfhdl.hw

import dfhdl.compiler.printing.{Printer, HasCodeString}
import scala.annotation.StaticAnnotation
import dfhdl.internals.*
import scala.annotation.Annotation
import upickle.default.*
import dfhdl.internals.StableEnum
import dfhdl.compiler.ir.ConfigN

object annotation:
  sealed abstract class HWAnnotation extends StaticAnnotation, Product, Serializable, HasCodeString
      derives CanEqual:
    val isActive: Boolean

  extension (annotList: List[Annotation])
    def getActiveHWAnnotations: List[HWAnnotation] = annotList.collect {
      case annot: HWAnnotation if annot.isActive => annot
    }
  object HWAnnotation:
    given ReadWriter[HWAnnotation] = ReadWriter.merge(
      summon[ReadWriter[unused]],
      summon[ReadWriter[pure]],
      summon[ReadWriter[flattenMode]],
      summon[ReadWriter[constraints.Constraint]]
    )

  sealed trait unused extends HWAnnotation derives ReadWriter
  object unused:
    /** `quiet` suppresses the unused warning for the tagged value.
      */
    final case class quiet(isActive: Boolean) extends unused:
      def this() = this(true)
      def codeString(using Printer): String = "@hw.annotation.unused.quiet"

    /** `keep` suppresses the unused warning, and also attempts to keep the tagged value.
      */
    final case class keep(isActive: Boolean) extends unused:
      def this() = this(true)
      def codeString(using Printer): String = "@hw.annotation.unused.keep"

    /** `prune` removes all the redundant paths until and including the tagged value.
      */
    final case class prune(isActive: Boolean) extends unused:
      def this() = this(true)
      def codeString(using Printer): String = "@hw.annotation.unused.prune"
  end unused

  final case class pure(isActive: Boolean) extends HWAnnotation derives ReadWriter:
    def this() = this(true)
    def codeString(using Printer): String = "@hw.annotation.pure"

  /** Flattening Mode:
    *   - transparent: $memberName
    *   - prefix: $ownerName$sep$memberName
    *   - suffix: $memberName$sep$ownerName
    */
  enum flattenMode extends HWAnnotation, StableEnum derives CanEqual, ReadWriter:
    case transparent()
    case prefix(sep: String)
    case suffix(sep: String)
    val isActive: Boolean = true
    def codeString(using Printer): String =
      "@hw.annotation.flattenMode." + (
        this match
          case transparent() => "transparent()"
          case prefix(sep)   => s"""prefix("$sep")"""
          case suffix(sep)   => s"""suffix("$sep")"""
      )
  object flattenMode:
    val defaultPrefixUnderscore = flattenMode.prefix("_")
end annotation

object constraints:
  sealed abstract class Constraint extends annotation.HWAnnotation:
    val isActive: Boolean = true
  sealed abstract class SigConstraint extends Constraint:
    val bitIdx: ConfigN[Int]
  object Constraint:
    given ReadWriter[Constraint] = ReadWriter.merge(
      summon[ReadWriter[io]],
      summon[ReadWriter[device]]
    )

  final case class device(name: String, properties: (String, String)*)
      extends Constraint
      derives CanEqual, ReadWriter:
    def codeString(using Printer): String =
      val props = properties.map { case (k, v) => s""""$k" -> "$v"""" }.mkString(", ")
      s"""@device("$name"${props.emptyOr(", " + _)})"""

  final case class io(
      bitIdx: ConfigN[Int] = None,
      loc: ConfigN[String] = None,
      standard: ConfigN[io.Standard] = None,
      slewRate: ConfigN[io.SlewRate] = None,
      driveStrength: ConfigN[Int] = None,
      pullMode: ConfigN[io.PullMode] = None
  ) extends SigConstraint derives CanEqual, ReadWriter:
    def codeString(using Printer): String =
      def byName[T](name: String, value: ConfigN[T]): String =
        value match
          case None              => ""
          case cs: HasCodeString => s"$name = ${cs.codeString}"
          case str: String       => s"$name = \"$str\""
          case _                 => s"$name = ${value}"
      val params = List(
        byName("bitIdx", bitIdx),
        byName("loc", loc),
        byName("standard", standard),
        byName("slewRate", slewRate),
        byName("driveStrength", driveStrength),
        byName("pullMode", pullMode)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@io($params)"""
    end codeString
  end io
  object io:
    enum Standard extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case LVCMOS33, LVCMOS25, LVCMOS18
      def codeString(using Printer): String = "io.Standard." + this.toString
    enum SlewRate extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case SLOW, FAST
      def codeString(using Printer): String = "io.SlewRate." + this.toString
    enum PullMode extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case UP, DOWN
      def codeString(using Printer): String = "io.PullMode." + this.toString

  object timing:
    final case class ignore(bitIdx: ConfigN[Int] = None)
        extends SigConstraint
        derives CanEqual, ReadWriter:
      def codeString(using Printer): String =
        val params = bitIdx match
          case None   => ""
          case bitIdx => s"bitIdx = $bitIdx"
        s"""@timing.ignore($params)"""
end constraints
