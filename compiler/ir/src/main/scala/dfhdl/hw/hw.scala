/** this package contains all the dfhdl hardware annotations
  */
package dfhdl.hw

export dfhdl.compiler.ir.HWAnnotation
import dfhdl.compiler.printing.Printer

sealed trait unused extends HWAnnotation
object unused:
  /** `quiet` suppresses the unused warning for the tagged value.
    */
  final case class quiet(isActive: Boolean) extends unused:
    def this() = this(true)
    override def codeString(using Printer): String = "unused.quiet"

  /** `keep` suppresses the unused warning, and also attempts to keep the tagged value.
    */
  final case class keep(isActive: Boolean) extends unused:
    def this() = this(true)
    override def codeString(using Printer): String = "unused.keep"

  /** `prune` removes all the redundant paths until and including the tagged value.
    */
  final case class prune(isActive: Boolean) extends unused:
    def this() = this(true)
    override def codeString(using Printer): String = "unused.prune"
end unused

final case class pure(isActive: Boolean) extends HWAnnotation:
  def this() = this(true)

/** Flattening Mode:
  *   - transparent: $memberName
  *   - prefix: $ownerName$sep$memberName
  *   - suffix: $memberName$sep$ownerName
  */
enum flattenMode extends HWAnnotation derives CanEqual:
  case transparent()
  case prefix(sep: String)
  case suffix(sep: String)
  val isActive: Boolean = true
  override def codeString(using Printer): String =
    this match
      case transparent() => "flattenMode.transparent()"
      case prefix(sep)   => s"""flattenMode.prefix("$sep")"""
      case suffix(sep)   => s"""flattenMode.suffix("$sep")"""
object flattenMode:
  val defaultPrefixUnderscore = flattenMode.prefix("_")
