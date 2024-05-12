package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VerilogValPrinter extends AbstractValPrinter:
  type TPrinter <: VerilogPrinter
  val supportLogicType: Boolean = true
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String = printer.unsupported
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    val arrRange = printer.csDFVectorRanges(dfVal.dfType)
    s"parameter ${printer.csDFType(dfVal.dfType).emptyOr(_ + " ")}${dfVal.getName}${arrRange} = ${csDFValExpr(dfVal)};"
  private def wireOrLogic: String = if (supportLogicType) "logic" else "wire"
  private def regOrLogic: String = if (supportLogicType) "logic" else "reg"
  def csDFValDclWithoutInit(dfVal: Dcl): String =
    val dfTypeStr = printer.csDFType(dfVal.dfType)
    val modifier = dfVal.modifier.dir match
      case Modifier.IN =>
        dfVal.dfType match
          case _: DFStruct => "input "
          case _           => "input wire "
      case Modifier.OUT   => "output "
      case Modifier.INOUT => "inout  "
      case Modifier.VAR   => ""
//        dfVal.dfType match
//          case _: DFEnum => ""
//          case _         => "logic"
    val arrRange = printer.csDFVectorRanges(dfVal.dfType)
    s"$modifier${dfTypeStr.emptyOr(_ + " ")}${dfVal.getName}$arrRange"
  end csDFValDclWithoutInit
  def csInitKeyword: String = "="
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = printer.unsupported
  def csDFValDclEnd(dfVal: Dcl): String = if (dfVal.isPort) "" else ";"
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
    dfVal.args match
      // repeat func
      case argL :: argR :: Nil if dfVal.op == Func.Op.repeat =>
        s"{${argR.refCodeString.applyBrackets()}{${argL.refCodeString}}}"
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val isInfix = dfVal.op match
          case Func.Op.max | Func.Op.min => false
          case _                         => true
        val opStr = dfVal.op match
          case Func.Op.=== => "=="
          case Func.Op.=!= => "!="
          case Func.Op.max => "`MAX"
          case Func.Op.min => "`MIN"
          case Func.Op.>> =>
            argL.get.dfType match
              case DFSInt(_) => ">>>"
              case _         => ">>"
          case op => op.toString
        val rhsStr = dfVal.op match
          case Func.Op.>> | Func.Op.<< => argR.simpleRefCodeString
          case _                       => argR.refCodeString
        if (isInfix)
          s"${argL.refCodeString.applyBrackets()} $opStr ${rhsStr.applyBrackets()}"
        else
          s"$opStr(${argL.refCodeString}, ${rhsStr})"
      // unary/postfix func
      case arg :: Nil =>
        val argStr = arg.refCodeString
        val argStrB = argStr.applyBrackets()
        dfVal.op match
          case Func.Op.rising  => s"posedge $argStrB"
          case Func.Op.falling => s"negedge $argStrB"
          case Func.Op.unary_- => s"-$argStrB"
          case Func.Op.unary_! => s"!$argStrB"
          case Func.Op.unary_~ => s"~$argStrB"
          case Func.Op.&       => s"&$argStrB"
          case Func.Op.|       => s"|$argStrB"
          case Func.Op.^       => s"^$argStrB"
          case Func.Op.clog2   => s"$$clog2($argStr)"
          case _               => printer.unsupported
      // multiarg func
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            dfVal.dfType match
              case DFStruct(_, _) =>
                args.map(_.refCodeString).mkString("'{", ", ", "}")
//              case DFVector(_, _) => printer.unsupported
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"{${args.length}{${args.head.refCodeString}}}"
              // regular concatenation function
              case _ => args.map(_.refCodeString).mkString("{", ", ", "}")
            end match
          case _ =>
            args
              .map(_.refCodeString.applyBrackets())
              .mkString(s" ${dfVal.op} ")

  def csDFValAliasAsIs(dfVal: Alias.AsIs): String =
    val relVal = dfVal.relValRef.get
    val relValStr = dfVal.relValRef.refCodeString
    val fromType = relVal.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f =>
        relValStr
      case (DFSInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth + 1)
        s"$$signed({1'b0, $relValStr})"
      case (DFUInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        relValStr
      case (DFBits(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth)
        relValStr
      case (DFSInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        s"$$signed($relValStr)"
      case (DFBits(tr @ Int(tWidth)), DFBits(fr @ Int(fWidth))) =>
        if (tWidth == fWidth) relValStr
        else if (tWidth < fWidth) s"${relValStr.applyBrackets()}[${tr.uboundCS}:0]"
        else s"{{(${tr.refCodeString}-${fr.refCodeString}){1'b0}}, $relValStr}"
      case (t, DFOpaque(_, _, ot)) if ot =~ t =>
        relValStr
      case (DFOpaque(_, _, _), _) =>
        relValStr
      case (DFBits(Int(tWidth)), _) =>
        assert(tWidth == fromType.width)
        s"{$relValStr}"
      case (DFUInt(tr @ Int(tWidth)), DFUInt(fr @ Int(fWidth))) =>
        if (tWidth == fWidth) relValStr
        else if (tWidth < fWidth) s"${relValStr.applyBrackets()}[${tr.uboundCS}:0]"
        else s"{{(${tr.refCodeString}-${fr.refCodeString}){1'b0}}, $relValStr}"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
      case (DFSInt(tWidthParamRef), DFSInt(_) | DFInt32) =>
        s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
      case (DFInt32, DFSInt(_)) => relValStr
      case (DFBit, DFBool)      => relValStr
      case (DFBool, DFBit)      => relValStr
      case (toStruct: DFStruct, _: DFBits) =>
        s"${toStruct.getName}'($relValStr)"
      case _ => printer.unsupported
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    s"${dfVal.relValCodeString}[${dfVal.relBitHigh}:${dfVal.relBitLow}]"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String =
    val relIdxStr = dfVal.relIdx.simpleRefCodeString
    s"${dfVal.relValCodeString}[$relIdxStr]"
  // when the tuple field number exceeds this number, the tuple
  // field selections changes from `dv._${idx+1}` to `dv($idx)`
  val TUPLE_MIN_INDEXING = 3
  def csDFValAliasSelectField(dfVal: Alias.SelectField): String =
    val dfType @ DFStruct(structName, fieldMap) = dfVal.relValRef.get.dfType: @unchecked
    val fieldSel =
      if (dfType.isTuple)
        if (fieldMap.size > TUPLE_MIN_INDEXING)
          s"(${dfVal.fieldName.drop(1).toInt - 1})"
        else s".${dfVal.fieldName}"
      else s".${dfVal.fieldName}"
    s"${dfVal.relValCodeString}$fieldSel"
  def csDFValAliasHistory(dfVal: Alias.History): String = printer.unsupported
  def csTimerIsActive(dfVal: Timer.IsActive): String = printer.unsupported
  def csDFValNamed(dfVal: DFVal): String =
    dfVal.stripPortSel match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case const @ DclConst()    => csDFValDclConst(const)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
      case _                     => ???
  end csDFValNamed
end VerilogValPrinter
