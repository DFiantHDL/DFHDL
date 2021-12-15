package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*

object Ident:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using MemberGetSet): Boolean =
    alias.dfType == alias.relValRef.get.dfType
