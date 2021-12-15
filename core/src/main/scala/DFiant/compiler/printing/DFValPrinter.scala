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

extension (alias: Alias)
  def relValCodeString(using
      getSet: MemberGetSet,
      printer: DFValPrinter
  ): String =
    alias.relValRef.refCodeString.applyBrackets()

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
        s"${argL.refCodeString.applyBrackets()} $opStr ${argR.refCodeString.applyBrackets()}"
      case arg :: Nil =>
        val opStr = dfVal.op.toString
        val argStr = arg.refCodeString.applyBrackets()
        if (opStr.startsWith("unary_")) s"${opStr.last}$argStr"
        else s"${argStr}.${opStr}"
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            // all args are the same ==> repeat function
            if (args.view.map(_.get).allElementsAreEqual)
              s"${(args.head.refCodeString).applyBrackets()}.repeat(${args.length})"
            // regular concatenation function
            else
              args.map(_.refCodeString).mkStringBrackets
          case _ =>
            args
              .map(_.refCodeString.applyBrackets())
              .mkString(s" ${dfVal.op} ")
  def csDFValAliasAsIs(dfVal: Alias.AsIs)(using MemberGetSet): String =
    val relValStr = dfVal.relValCodeString
    val fromType = dfVal.relValRef.get.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f => relValStr // ident
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
    s"${dfVal.relValCodeString}(${dfVal.relBitHigh}, ${dfVal.relBitLow})"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx)(using
      MemberGetSet
  ): String =
    val relIdxStr = dfVal.relIdx.refCodeString
    s"${dfVal.relValCodeString}($relIdxStr)"
  // when the tuple field number exceeds this number, the tuple
  // field selections changes from `dv._${idx+1}` to `dv($idx)`
  val TUPLE_MIN_INDEXING = 3
  def csDFValAliasSelectField(dfVal: Alias.SelectField)(using
      MemberGetSet
  ): String =
    val DFStruct(structName, fieldMap) = dfVal.relValRef.get.dfType
    val fieldSel =
      if (structName == DFStruct.ReservedTupleName)
        if (fieldMap.size > TUPLE_MIN_INDEXING) s"(${dfVal.fieldName})"
        else s"._${dfVal.fieldName.toInt + 1}"
      else s".${dfVal.fieldName}"
    s"${dfVal.relValCodeString}$fieldSel"
  def csDFValAliasRef(dfVal: Alias)(using MemberGetSet): String = dfVal match
    case dv: Alias.AsIs        => csDFValAliasAsIs(dv)
    case _: Alias.Prev         => ???
    case dv: Alias.ApplyRange  => csDFValAliasApplyRange(dv)
    case dv: Alias.ApplyIdx    => csDFValAliasApplyIdx(dv)
    case dv: Alias.SelectField => csDFValAliasSelectField(dv)
  def csDFVal(dfVal: DFVal, fromOwner: Option[DFOwner])(using
      MemberGetSet
  ): String =
    def valDef = s"val ${dfVal.name} = "
    def rhs = dfVal match
      case dv: Dcl   => csDFValDcl(dv)
      case dv: Const => csDFValConst(dv)
      case dv: Func  => csDFValFuncRef(dv)
      case dv: Alias => csDFValAliasRef(dv)
      case dv: DFIfElseBlock =>
        val firstNewLine = dv.prevBlockRef match
          case _: DFRef.Empty if !dv.isAnonymous => "\n"
          case _                                 => ""
        s"$firstNewLine${printer.csDFIfElseBlock(dv)}"
//        printer.csDFIfElseBlock(dv)
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
