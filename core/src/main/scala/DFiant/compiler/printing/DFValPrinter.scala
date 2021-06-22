package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*
import DFVal.*

protected trait DFValPrinter extends AbstractPrinter:
  def csDFValConst(dfVal: Const, anonRef: Boolean): String =
    if (anonRef) printer.csDFToken(dfVal.token)
    else
      s"${printer.csDFType(dfVal.dfType)} const ${printer.csDFToken(dfVal.token)}"
  def csDFValDcl(dfVal: Dcl): String =
    val noinit =
      s"${printer.csDFType(dfVal.dfType)} <> ${dfVal.modifier}"
    dfVal.externalInit match
      case Some(initSeq) if initSeq.size > 1 =>
        s"$noinit init ${printer.csDFTokenSeq(initSeq)}"
      case Some(initSeq) if initSeq.size == 1 =>
        s"$noinit init ${printer.csDFToken(initSeq.head)}"
      case _ => noinit
  def csDFValFunc(dfVal: Func, ref: Boolean)(using MemberGetSet): String = ???
  def csDFValAlias(dfVal: Alias, ref: Boolean)(using MemberGetSet): String = ???
  def csDFVal(dfVal: DFVal, fromOwner: Option[DFBlock])(using
      MemberGetSet
  ): String = ???
//    dfVal match
//      case dv if fromOwner.isDefined && !dfVal.isAnonymous => dfVal.getRelativeName
//      case dv: Const                      => csDFValConst(dv, ref)
//      case dv: Dcl                        => csDFValDcl(dv)
//      case dv: Func                       => csDFValFunc(dv, ref)
//      case dv: Alias                      => csDFValAlias(dv, ref)
end DFValPrinter
