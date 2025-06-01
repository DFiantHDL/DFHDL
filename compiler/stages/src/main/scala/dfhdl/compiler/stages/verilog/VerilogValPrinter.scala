package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VerilogValPrinter extends AbstractValPrinter:
  type TPrinter <: VerilogPrinter
  val supportLogicType: Boolean = printer.dialect match
    case VerilogDialect.v2001 | VerilogDialect.v95 => false
    case _                                         => true
  val supportGlobalParameters: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String = printer.unsupported
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    if (supportGlobalParameters || !dfVal.isGlobal)
      val arrRange = printer.csDFVectorRanges(dfVal.dfType)
      val endOfStatement = if (dfVal.isGlobal) ";" else ""
      val default = dfVal match
        // for non-top-level design parameters, we fetch the default value if it is defined.
        // for all other cases, we get the parameter constant data and use that as default value.
        // using the constant data only happens in verilog.v95, since parameters are declared in
        // the body and must have defaults.
        case param: DesignParam =>
          param.defaultRef.get match
            case defaultVal: CanBeExpr if !param.getOwnerDesign.isTop => csDFValExpr(defaultVal)
            case _ => printer.csConstData(param.dfType, param.getConstData.get)
        case _ => csDFValExpr(dfVal)
      val csType = printer.csDFType(dfVal.dfType).emptyOr(_ + " ")
      val csTypeNoLogic = if (supportLogicType) csType else csType.replace("logic ", "")
      s"parameter ${csTypeNoLogic}${dfVal.getName}${arrRange} = $default$endOfStatement"
    else s"`define ${dfVal.getName} ${csDFValExpr(dfVal).replace("\n", " \\\n")}"

  def csDFValDclWithoutInit(dfVal: Dcl): String =
    val dfTypeStr = printer.csDFType(dfVal.dfType)
    val modifier = dfVal.modifier.dir match
      case Modifier.IN    => "input  wire "
      case Modifier.OUT   => "output "
      case Modifier.INOUT => "inout  "
      case Modifier.VAR   => ""
    def regOrWireRep = dfVal.modifier.dir match
      case Modifier.IN => ""
      case _           =>
        if (dfVal.getConnectionTo.nonEmpty) "wire"
        else "reg"
    val fixedDFTypeStr =
      if (supportLogicType) dfTypeStr else dfTypeStr.replace("logic", regOrWireRep)
    val arrRange = printer.csDFVectorRanges(dfVal.dfType)
    s"$modifier${fixedDFTypeStr.emptyOr(_ + " ")}${dfVal.getName}$arrRange"
  end csDFValDclWithoutInit
  def csInitKeyword: String = "="
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = printer.unsupported
  def csDFValDclEnd(dfVal: Dcl): String = ""
  val allowDoubleStarPowerSyntax: Boolean =
    printer.dialect match
      case VerilogDialect.v95 => false
      case _                  => true
  override def csDFMemberName(named: DFMember.Named): String =
    if (printer.supportGlobalParameters) named.getName
    else
      named match
        case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => s"`${named.getName}"
        case _                                          => named.getName
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
    def commonOpStr: String =
      dfVal.op match
        case Func.Op.max                               => "`MAX"
        case Func.Op.min                               => "`MIN"
        case Func.Op.** if !allowDoubleStarPowerSyntax => "power"
        case op                                        => op.toString()
    dfVal.args match
      // boolean sel function
      case cond :: onTrue :: onFalse :: Nil if dfVal.op == Func.Op.sel =>
        s"${cond.refCodeString.applyBrackets()} ? ${onTrue.refCodeString.applyBrackets()} : ${onFalse.refCodeString.applyBrackets()}"
      // repeat func
      case argL :: argR :: Nil if dfVal.op == Func.Op.repeat =>
        dfVal.dfType match
          case _: DFVector =>
            s"'{default: ${argL.refCodeString}}"
          case _ =>
            s"{${argR.refCodeString.applyBrackets()}{${argL.refCodeString}}}"
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val isInfix = dfVal.op match
          case Func.Op.max | Func.Op.min                 => false
          case Func.Op.** if !allowDoubleStarPowerSyntax => false
          case _                                         => true
        val opStr = dfVal.op match
          case Func.Op.=== => "=="
          case Func.Op.=!= => "!="
          case Func.Op.>>  =>
            argL.get.dfType match
              case DFSInt(_) => ">>>"
              case _         => ">>"
          case _ => commonOpStr
        (argL.get.dfType, dfVal.op) match
          case (
                DFSInt(widthRef),
                op @ (Func.Op.> | Func.Op.< | Func.Op.>= | Func.Op.<= | Func.Op.>>)
              ) if !printer.allowSignedKeywordAndOps =>
            val csWidth = widthRef.refCodeString
            val csArgL = argL.refCodeString
            val csArgR = argR.refCodeString
            val opStr = op match
              case Func.Op.>  => "SIGNED_GREATER_THAN"
              case Func.Op.<  => "SIGNED_LESS_THAN"
              case Func.Op.>= => "SIGNED_GREATER_EQUAL"
              case Func.Op.<= => "SIGNED_LESS_EQUAL"
              case Func.Op.>> => "SIGNED_SHIFT_RIGHT"
            s"`$opStr($csArgL, $csArgR, $csWidth)"
          case _ if (isInfix) =>
            s"${argL.refCodeString.applyBrackets()} $opStr ${argR.refCodeString.applyBrackets()}"
          case _ =>
            s"$opStr(${argL.refCodeString}, ${argR.refCodeString})"
        end match
      // unary/postfix func
      case arg :: Nil =>
        val argStr = arg.refCodeString
        val argStrB = argStr.applyBrackets()
        dfVal.op match
          case Func.Op.rising  => s"posedge $argStrB"
          case Func.Op.falling => s"negedge $argStrB"
          case Func.Op.unary_- => s"-$argStrB"
          case Func.Op.unary_! =>
            dfVal.dfType match
              case DFBool => s"!$argStrB"
              case _      => s"~${argStrB}"
          case Func.Op.unary_~ => s"~$argStrB"
          case Func.Op.&       => s"&$argStrB"
          case Func.Op.|       => s"|$argStrB"
          case Func.Op.^       => s"^$argStrB"
          case Func.Op.clog2   =>
            val internalLog = printer.dialect match
              case VerilogDialect.v95 | VerilogDialect.v2001 => ""
              case _                                         => "$"
            s"${internalLog}clog2($argStr)"
          case _ => printer.unsupported
        end match
      // multiarg func
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            dfVal.dfType match
              case DFStruct(_, _) | DFVector(_, _) =>
                args.map(_.refCodeString).csList("'{", ",", "}")
//              case DFVector(_, _) => printer.unsupported
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"{${args.length}{${args.head.refCodeString}}}"
              // regular concatenation function
              case _ => args.map(_.refCodeString).csList("{", ",", "}")
            end match
          case _ =>
            args
              .map(_.refCodeString.applyBrackets())
              .mkString(s" ${commonOpStr} ")
    end match
  end csDFValFuncExpr

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
        val extended = s"{1'b0, $relValStr}"
        if (printer.allowSignedKeywordAndOps) s"$$signed($extended)"
        else extended
      case (DFUInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        relValStr
      case (DFBits(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth)
        relValStr
      case (DFSInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        if (printer.allowSignedKeywordAndOps) s"$$signed($relValStr)"
        else relValStr
      case (DFBits(tr @ Int(tWidth)), DFBits(fr @ Int(fWidth))) =>
        if (tWidth == fWidth) relValStr
        else if (tWidth < fWidth) s"${relValStr.applyBrackets()}[${tr.uboundCS}:0]"
        else s"{{(${tr.refCodeString}-${fr.refCodeString}){1'b0}}, $relValStr}"
      case (t, DFOpaque(actualType = ot)) if ot =~ t =>
        relValStr
      case (DFOpaque(_, _, _), _) =>
        relValStr
      case (DFUInt(tr @ Int(tWidth)), DFUInt(fr @ Int(fWidth))) =>
        if (tWidth == fWidth) relValStr
        else if (tWidth < fWidth) s"${relValStr.applyBrackets()}[${tr.uboundCS}:0]"
        else s"{{(${tr.refCodeString}-${fr.refCodeString}){1'b0}}, $relValStr}"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        if (printer.allowWidthCastSyntax)
          s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
        else relValStr
      case (DFSInt(tWidthParamRef), DFInt32) =>
        if (printer.allowWidthCastSyntax)
          s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
        else relValStr
      case (DFSInt(tWidthParamRef), DFSInt(_)) =>
        if (printer.allowWidthCastSyntax)
          s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
        else s"{$relValStr}[${tWidthParamRef.refCodeString.applyBrackets()} - 1:0]"
      case (DFInt32, DFUInt(_) | DFSInt(_)) => relValStr
      case (DFBit, DFBool)                  => relValStr
      case (DFBool, DFBit)                  => relValStr
      case (toStruct: DFStruct, _: DFBits)  =>
        s"${toStruct.getName}'($relValStr)"
      case (toVector: DFVector, _: DFBits) =>
        def to_vector_conv(vectorType: DFVector, relHighIdx: Int): String =
          val vecLength = vectorType.length
          vectorType.cellType match
            case cellType: DFVector =>
              List.tabulate(vecLength)(i =>
                to_vector_conv(cellType, relHighIdx - i * cellType.width)
              ).csList("'{", ",", "}")
            case cellType: DFBits =>
              val cellWidth = cellType.width
              List.tabulate(vecLength)(i =>
                s"$relValStr[${relHighIdx - i * cellWidth}:${relHighIdx - (i + 1) * cellWidth + 1}]"
              ).csList("'{", ",", "}")
            case x =>
              println(x)
              printer.unsupported
        end to_vector_conv
        to_vector_conv(toVector, toVector.width - 1)
      case (DFBits(Int(tWidth)), fromVector: DFVector) =>
        def from_vector_conv(vectorType: DFVector, prevSelect: String): String =
          val vecLength = vectorType.length
          vectorType.cellType match
            case cellType: DFVector =>
              List.tabulate(vecLength)(i => from_vector_conv(cellType, s"[$i]"))
                .csList("{", ",", "}")
            case cellType: DFBits =>
              val cellWidth = cellType.width
              List.tabulate(vecLength)(i => s"$relValStr$prevSelect[$i]").csList("{", ",", "}")
            case x =>
              println(x)
              printer.unsupported
        end from_vector_conv
        assert(tWidth == fromType.width)
        from_vector_conv(fromVector, "")
      case (DFBits(Int(tWidth)), _) =>
        assert(tWidth == fromType.width)
        s"{$relValStr}"
      case x =>
        println(x)
        printer.unsupported
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    s"${dfVal.relValCodeString}[${dfVal.relBitHigh}:${dfVal.relBitLow}]"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String =
    val relIdxStr = dfVal.relIdx.refCodeString
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
  // def csTimerIsActive(dfVal: Timer.IsActive): String = printer.unsupported
  def csNOTHING(dfVal: NOTHING): String =
    dfVal.dfType match
      case DFBit         => "1'bz"
      case DFBits(width) =>
        val csWidth = width.refCodeString.applyBrackets()
        s"""${csWidth}'bz"""
      case _ => printer.unsupported
  def csDFValNamed(dfVal: DFVal): String =
    dfVal.stripPortSel match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case const @ DclConst()    => csDFValDclConst(const)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
      case _                     => ???
  end csDFValNamed
end VerilogValPrinter
