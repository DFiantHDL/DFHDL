package DFiant.compiler
package analysis
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFiant.internals.*
import ir.*

object Ident:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using
      MemberGetSet
  ): Option[ir.DFVal] =
    val relVal = alias.relValRef.get
    if (alias.dfType == relVal.dfType) Some(relVal)
    else None

object Bind:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using
      MemberGetSet
  ): Option[ir.DFVal] =
    alias match
      case Ident(dfVal) if alias.getTagOf[Pattern.Bind.Tag.type].isDefined =>
        Some(dfVal)
      case _ => None
