package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*
import DFVal.*

protected trait DFValPrinter extends AbstractPrinter:
  extension (ref: DFVal.Ref)
    def refCodeString(using MemberGetSet): String =
      val dfVal = ref.get
      val callOwner = ref.originRef.get.getOwner
      csDFVal(dfVal, Some(callOwner))
  def csDFValConst(dfVal: Const): String =
    s"${printer.csDFType(dfVal.dfType)} const ${printer.csDFToken(dfVal.token)}"
  def csDFValConstRef(dfVal: Const): String =
    printer.csDFToken(dfVal.token)
  def csDFValDcl(dfVal: Dcl): String =
    val noinit =
      s"${printer.csDFType(dfVal.dfType)} <> ${dfVal.modifier}"
    dfVal.externalInit match
      case Some(initSeq) if initSeq.size > 1 =>
        s"$noinit init ${printer.csDFTokenSeq(initSeq)}"
      case Some(initSeq) if initSeq.size == 1 =>
        s"$noinit init ${printer.csDFToken(initSeq.head)}"
      case _ => noinit

  def csDFValFuncRef(dfVal: Func)(using MemberGetSet): String =
    dfVal.args match
      case argL :: argR :: Nil =>
        s"${argL.refCodeString.applyBrackets(true)} ${dfVal.op} ${argR.refCodeString.applyBrackets(true)}"
      case arg :: Nil =>
        val opStr = dfVal.op.toString
        val argStr = arg.refCodeString.applyBrackets(true)
        if (opStr.startsWith("unary_")) s"${opStr.last}$argStr"
        else s"${argStr}.${opStr}"
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ => args.map(_.refCodeString).mkStringBrackets
          case _ =>
            args
              .map(_.refCodeString.applyBrackets(true))
              .mkString(s" ${dfVal.op} ")
  def csDFValAliasRef(dfVal: Alias)(using MemberGetSet): String = ???
  def csDFVal(dfVal: DFVal, fromOwner: Option[DFOwner])(using
      MemberGetSet
  ): String =
    def valDef = s"val ${dfVal.name} = "
    def rhs = dfVal match
      case dv: Dcl   => csDFValDcl(dv)
      case dv: Const => csDFValConst(dv)
      case dv: Func  => csDFValFuncRef(dv)
      case dv: Alias => csDFValAliasRef(dv)
    (dfVal, fromOwner) match
      case (c: Const, Some(_)) if c.isAnonymous => csDFValConstRef(c)
      case (dv, Some(owner)) if !dv.isAnonymous =>
        dfVal.getRelativeName(owner)
      case (dv, None) if !dv.isAnonymous => valDef + rhs
      case _                             => rhs
end DFValPrinter
