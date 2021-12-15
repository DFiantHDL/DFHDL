package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*

object Ident:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using MemberGetSet): Boolean =
    alias.dfType == alias.relValRef.get.dfType

object Placeholder:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using MemberGetSet): Boolean =
    alias match
      case Ident() =>
        alias.relValRef.get match
          case _: DFIfElseBlock => false
          case _                => true
      case _ => false
