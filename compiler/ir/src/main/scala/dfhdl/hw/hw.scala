/** this package contains all the dfhdl hardware annotations
  */
package dfhdl.hw

import dfhdl.compiler.printing.Printer
import scala.annotation.StaticAnnotation
import dfhdl.internals.HasTypeName
import scala.annotation.Annotation
import upickle.default.*

sealed abstract class HWAnnotation extends StaticAnnotation, Product, Serializable derives CanEqual:
  val isActive: Boolean
  def codeString(using Printer): String

extension (annotList: List[Annotation])
  def getActiveHWAnnotations: List[HWAnnotation] = annotList.collect {
    case annot: HWAnnotation if annot.isActive => annot
  }
object HWAnnotation:
  given ReadWriter[HWAnnotation] = ReadWriter.merge(
    summon[ReadWriter[unused]],
    summon[ReadWriter[pure]],
    summon[ReadWriter[flattenMode]]
  )

sealed trait unused extends HWAnnotation derives ReadWriter
object unused:
  /** `quiet` suppresses the unused warning for the tagged value.
    */
  final case class quiet(isActive: Boolean) extends unused:
    def this() = this(true)
    def codeString(using Printer): String = "unused.quiet"

  /** `keep` suppresses the unused warning, and also attempts to keep the tagged value.
    */
  final case class keep(isActive: Boolean) extends unused:
    def this() = this(true)
    def codeString(using Printer): String = "unused.keep"

  /** `prune` removes all the redundant paths until and including the tagged value.
    */
  final case class prune(isActive: Boolean) extends unused:
    def this() = this(true)
    def codeString(using Printer): String = "unused.prune"
end unused

final case class pure(isActive: Boolean) extends HWAnnotation derives ReadWriter:
  def this() = this(true)
  def codeString(using Printer): String = "pure"

/** Flattening Mode:
  *   - transparent: $memberName
  *   - prefix: $ownerName$sep$memberName
  *   - suffix: $memberName$sep$ownerName
  */
enum flattenMode extends HWAnnotation derives CanEqual, ReadWriter:
  case transparent()
  case prefix(sep: String)
  case suffix(sep: String)
  val isActive: Boolean = true
  def codeString(using Printer): String =
    this match
      case transparent() => "flattenMode.transparent()"
      case prefix(sep)   => s"""flattenMode.prefix("$sep")"""
      case suffix(sep)   => s"""flattenMode.suffix("$sep")"""
object flattenMode:
  val defaultPrefixUnderscore = flattenMode.prefix("_")
