package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*
import DFVal.*
import analysis.*

extension (ref: DFVal.Ref)
  def refCodeString(using getSet: MemberGetSet, printer: DFValPrinter): String =
    val dfVal = ref.get
    val callOwner = ref.originRef.get.getOwner
    printer.csDFVal(dfVal, Some(callOwner))

protected trait DFValPrinter extends AbstractPrinter:
  def csDFValConst(dfVal: Const): String =
    s"${printer.csDFType(dfVal.dfType)} const ${printer.csDFToken(dfVal.token)}"
  def csDFValConstRef(dfVal: Const): String =
    printer.csDFToken(dfVal.token)
  def csDFValDcl(dfVal: Dcl): String =
    s"${printer.csDFType(dfVal.dfType)} <> ${dfVal.modifier}"

  def csDFValFuncRef(dfVal: Func)(using MemberGetSet): String =
    dfVal.args match
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val opStr = dfVal.op match
          case Func.Op.=== => "=="
          case Func.Op.=!= => "!="
          case Func.Op.| | Func.Op.& if argL.get.dfType.width == 1 =>
            s"${dfVal.op}${dfVal.op}"
          case op => op.toString
        s"${argL.refCodeString.applyBrackets(true)} $opStr ${argR.refCodeString.applyBrackets(true)}"
      case arg :: Nil =>
        val opStr = dfVal.op.toString
        val argStr = arg.refCodeString.applyBrackets(true)
        if (opStr.startsWith("unary_")) s"${opStr.last}$argStr"
        else s"${argStr}.${opStr}"
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            //all args are the same ==> repeat function
            if (args.view.map(_.get).allElementsAreEqual)
              s"${(args.head.refCodeString).applyBrackets()}.repeat(${args.length})"
            //regular concatenation function
            else
              args.map(_.refCodeString).mkStringBrackets
          case _ =>
            args
              .map(_.refCodeString.applyBrackets(true))
              .mkString(s" ${dfVal.op} ")
  def csDFValAliasAsIs(dfVal: Alias.AsIs)(using MemberGetSet): String =
    val relValStr = dfVal.relValRef.refCodeString.applyBrackets()
    val fromType = dfVal.relValRef.get.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (DFSInt(tWidth), DFUInt(fWidth)) =>
        assert(tWidth == fWidth + 1)
        s"${relValStr}.signed"
      case (DFUInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"${relValStr}.uint"
      case (DFSInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"${relValStr}.sint"
      case (DFBits(tWidth), DFBits(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFBits(tWidth), _) =>
        assert(tWidth == fromType.width)
        s"${relValStr}.bits"
      case (DFUInt(tWidth), DFUInt(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFSInt(tWidth), DFSInt(_)) =>
        s"${relValStr}.resize($tWidth)"
      case (DFBit, DFBool) =>
        s"${relValStr}.bit"
      case (DFBool, DFBit) =>
        s"${relValStr}.bool"
      case (t, DFOpaque(_, ot)) if ot == t =>
        s"${relValStr}.actual"
      case (_, DFBits(_)) | (DFOpaque(_, _), _) =>
        s"${relValStr}.as(${printer.csDFType(toType)})"
      case _ =>
        throw new IllegalArgumentException("Unsupported alias/conversion")
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange)(using
      MemberGetSet
  ): String =
    val relValStr = dfVal.relValRef.refCodeString.applyBrackets()
    s"${relValStr}(${dfVal.relBitHigh}, ${dfVal.relBitLow})"
  def csDFValAliasRef(dfVal: Alias)(using MemberGetSet): String = dfVal match
    case dv: Alias.AsIs       => csDFValAliasAsIs(dv)
    case _: Alias.Prev        => ???
    case dv: Alias.ApplyRange => csDFValAliasApplyRange(dv)
    case dv: Alias.ApplyIdx   => ???
  def csDFVal(dfVal: DFVal, fromOwner: Option[DFOwner])(using
      MemberGetSet
  ): String =
    def valDef = s"val ${dfVal.name} = "
    def rhs = dfVal match
      case dv: Dcl   => csDFValDcl(dv)
      case dv: Const => csDFValConst(dv)
      case dv: Func  => csDFValFuncRef(dv)
      case dv: Alias => csDFValAliasRef(dv)
    def rhsInit = dfVal.getTagOf[ExternalInit] match
      case Some(ExternalInit(initSeq)) if initSeq.size > 1 =>
        s"$rhs init ${printer.csDFTokenSeq(initSeq)}"
      case Some(ExternalInit(initSeq)) if initSeq.size == 1 =>
        s"$rhs init ${printer.csDFToken(initSeq.head)}"
      case _ => rhs
    (dfVal, fromOwner) match
      case (c: Const, Some(_)) if c.isAnonymous => csDFValConstRef(c)
      case (dv, Some(owner)) if !dv.isAnonymous =>
        dfVal.getRelativeName(owner)
      case (dv, None) if !dv.isAnonymous => valDef + rhsInit
      case _                             => rhsInit
  end csDFVal
end DFValPrinter
