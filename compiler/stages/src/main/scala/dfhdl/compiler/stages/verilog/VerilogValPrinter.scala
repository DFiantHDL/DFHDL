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
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String = printer.unsupported
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    val arrRange = printer.csDFVectorRanges(dfVal.dfType)
    s"parameter ${printer.csDFType(dfVal.dfType).emptyOr(_ + " ")}${dfVal.getName}${arrRange} = ${csDFValExpr(dfVal)};"
  def csDFValDclWithoutInit(dfVal: Dcl): String =
    val dfTypeStr = printer.csDFType(dfVal.dfType)
    val (modifier, regOrWireRep) = dfVal.modifier.dir match
      case Modifier.IN    => ("input  ", "wire")
      case Modifier.OUT   => ("output ", "reg")
      case Modifier.INOUT => ("inout  ", "wire")
      case Modifier.VAR   => ("", "reg")
    val fixedDFTypeStr =
      if (supportLogicType) dfTypeStr else dfTypeStr.replace("logic", regOrWireRep)
    val arrRange = printer.csDFVectorRanges(dfVal.dfType)
    s"$modifier${fixedDFTypeStr.emptyOr(_ + " ")}${dfVal.getName}$arrRange"
  end csDFValDclWithoutInit
  def csInitKeyword: String = "="
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = printer.unsupported
  def csDFValDclEnd(dfVal: Dcl): String = if (dfVal.isPort) "" else ";"
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
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
        if (isInfix)
          s"${argL.refCodeString.applyBrackets()} $opStr ${argR.refCodeString.applyBrackets()}"
        else
          s"$opStr(${argL.refCodeString}, ${argR.refCodeString})"
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
      case (DFUInt(tr @ Int(tWidth)), DFUInt(fr @ Int(fWidth))) =>
        if (tWidth == fWidth) relValStr
        else if (tWidth < fWidth) s"${relValStr.applyBrackets()}[${tr.uboundCS}:0]"
        else s"{{(${tr.refCodeString}-${fr.refCodeString}){1'b0}}, $relValStr}"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
      case (DFSInt(tWidthParamRef), DFSInt(_) | DFInt32) =>
        s"${tWidthParamRef.refCodeString.applyBrackets()}'($relValStr)"
      case (DFInt32, DFUInt(_) | DFSInt(_)) => relValStr
      case (DFBit, DFBool)                  => relValStr
      case (DFBool, DFBit)                  => relValStr
      case (toStruct: DFStruct, _: DFBits) =>
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
  def csTimerIsActive(dfVal: Timer.IsActive): String = printer.unsupported
  def csNOTHING(dfVal: NOTHING): String =
    dfVal.dfType match
      case DFBit => "1'bz"
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
