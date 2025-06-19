package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VHDLValPrinter extends AbstractValPrinter:
  type TPrinter <: VHDLPrinter
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String = printer.unsupported
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    s"constant ${dfVal.getName} : ${printer.csDFType(dfVal.dfType)} := ${csDFValExpr(dfVal)};"
  def csDFValDclWithoutInit(dfVal: Dcl): String =
    val dfTypeStr = printer.csDFType(dfVal.dfType)
    if (dfVal.isPort) s"${dfVal.getName} : ${dfVal.modifier.toString.toLowerCase} $dfTypeStr"
    else
      val sigOrVar = dfVal.getOwnerNamed match
        case dsn: DFDesignBlock =>
          if (dfVal.modifier.isShared) "shared variable"
          else "signal"
        case _ => "variable"
      s"$sigOrVar ${dfVal.getName} : $dfTypeStr"
  end csDFValDclWithoutInit
  def csInitKeyword: String = ":="
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = printer.unsupported
  def csDFValDclEnd(dfVal: Dcl): String = if (dfVal.isPort) "" else ";"
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
    def commonOpStr: String =
      dfVal.op match
        case Func.Op.| => "or"
        case Func.Op.& => "and"
        case Func.Op.^ => "xor"
        case Func.Op.% => "rem"
        case op        => op.toString()
    dfVal.args match
      // boolean sel function
      case cond :: onTrue :: onFalse :: Nil if dfVal.op == Func.Op.sel =>
        val csCond = cond.refCodeString
        val csCondBool = cond.get.dfType match
          case DFBit => s"to_bool($csCond)"
          case _     => csCond
        s"bool_sel($csCondBool, ${onTrue.refCodeString}, ${onFalse.refCodeString})"
      // repeat func
      case argL :: argR :: Nil if dfVal.op == Func.Op.repeat =>
        dfVal.dfType match
          case dfType: DFBits =>
            s"repeat(${argL.refCodeString}, ${dfType.widthParamRef.refCodeString})"
          case dfType: DFVector =>
            s"(0 to ${dfType.cellDimParamRefs.head.uboundCS} => ${argL.refCodeString})"
          case _ =>
            println(dfVal)
            ???
      // infix/regular func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        var infix = true
        val opStr = dfVal.op match
          case Func.Op.=== => "="
          case Func.Op.=!= => "/="
          case Func.Op.>=  => "=>"
          case Func.Op.<<  =>
            argL.get.dfType match
              case DFSInt(_) => "sla"
              case DFUInt(_) => "sll"
              case _         =>
                infix = false
                "slv_sll"
          case Func.Op.>> =>
            argL.get.dfType match
              case DFSInt(_) =>
                infix = false
                "signed_sra"
              case DFUInt(_) => "srl"
              case _         =>
                infix = false
                "slv_srl"
          // if the result width for +/-/* ops is larger than the left argument width
          // then we have a carry-inclusive operation
          case op @ (Func.Op.+ | Func.Op.- | Func.Op.`*`)
              if dfVal.dfType.width > argL.get.dfType.width =>
            infix = false
            op match
              case Func.Op.+   => "cadd"
              case Func.Op.-   => "csub"
              case Func.Op.`*` => "cmul"
          case _ => commonOpStr
        if (infix)
          s"${argL.refCodeString.applyBrackets()} $opStr ${argR.refCodeString.applyBrackets()}"
        else s"${opStr}(${argL.refCodeString}, ${argR.refCodeString})"
      // unary/postfix func
      case arg :: Nil =>
        val argStr = arg.refCodeString
        val argStrB = argStr.applyBrackets()
        dfVal.op match
          case Func.Op.rising  => s"rising_edge($argStr)"
          case Func.Op.falling => s"falling_edge($argStr)"
          case Func.Op.unary_- => s"-$argStrB"
          case Func.Op.unary_! => s"not $argStrB"
          case Func.Op.unary_~ => s"not $argStrB"
          case Func.Op.&       => s"and reduce $argStrB"
          case Func.Op.|       => s"or reduce $argStrB"
          case Func.Op.^       => s"xor reduce $argStrB"
          case Func.Op.clog2   => s"clog2($argStr)"
          case _               => printer.unsupported
      // multiarg func
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            dfVal.dfType match
              case DFString =>
                args.map(_.refCodeString).mkString(" & ")
              case dfType @ DFStruct(_, _) =>
                printer.csDFStructTypeName(dfType) + dfType.fieldMap
                  .lazyZip(args.map(_.refCodeString))
                  .map { case ((n, _), d) => s"$n = $d" }
                  .mkStringBrackets

              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"(0 to ${args.length - 1} => ${args.head.refCodeString})"

              case DFVector(_, _) =>
                args.map(_.refCodeString).csList()
              // regular concatenation function
              case _ => args.map(_.refCodeString).mkString(" & ")
            end match
          case _ =>
            args
              .map(_.refCodeString.applyBrackets())
              .mkString(s" ${commonOpStr} ")
    end match
  end csDFValFuncExpr
  def csFixedCond(condRef: DFRef.TwoWay[DFVal, ?]): String =
    val requiresBoolConv =
      if (printer.inVHDL93)
        condRef.get.dfType match
          case DFBit => true
          case _     => false
      else false
    if (requiresBoolConv) s"to_bool(${condRef.refCodeString})"
    else condRef.refCodeString
  def csBitsToType(toType: DFType, csArg: String): String = toType match
    case DFBits(_)        => csArg
    case DFBool           => s"to_bool($csArg)"
    case DFBit            => s"to_sl($csArg)"
    case DFUInt(_)        => s"unsigned($csArg)"
    case DFSInt(_)        => s"signed($csArg)"
    case dfType: DFVector =>
      var loopType: DFType = dfType
      var desc: String = s"to_${printer.csDFVectorDclName(dfType)}($csArg"
      // starting the adding dimension only when unconstrained arrays are supported
      var inVector: Boolean = printer.supportUnconstrainedArrays
      while (inVector)
        loopType match
          case dfType: DFVector =>
            desc = s"$desc, ${dfType.cellDimParamRefs.head.refCodeString}"
            loopType = dfType.cellType
          case cellType =>
            val finale = cellType match
              case DFBits(width) => s", ${width.refCodeString}"
              case DFUInt(width) => s", ${width.refCodeString}"
              case DFSInt(width) => s", ${width.refCodeString}"
              case _             => ""
            desc = desc + finale
            inVector = false
      s"$desc)"
    case dfType: DFStruct => s"to_${printer.csDFStructTypeName(dfType)}($csArg)"
    case dfType: DFOpaque => csBitsToType(dfType.actualType, csArg)
    case _                => printer.unsupported

  def csToSLV(fromType: DFType, arg: String): String =
    fromType match
      case dt: DFBits => arg
      // opaques are subtypes, so they are transparent to `to_slv` operations
      case dt: DFOpaque => csToSLV(dt.actualType, arg)
      case _            => s"to_slv($arg)"
  def csDFValAliasAsIs(dfVal: Alias.AsIs): String =
    val relVal = dfVal.relValRef.get
    val relValStr = dfVal.relValRef.refCodeString
    val fromType = relVal.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f                           => relValStr
      case (DFSInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth + 1)
        s"signed($relValStr)"
      case (DFBits(tWidthParamRef), DFBits(_)) =>
        s"resize($relValStr, ${tWidthParamRef.refCodeString})"
      case (toType: DFType, fromType: DFBits) =>
        assert(toType.width == fromType.width)
        csBitsToType(toType, relValStr)
      case (DFBits(Int(tWidth)), fromType: DFType) =>
        assert(tWidth == fromType.width)
        csToSLV(fromType, relValStr)
      case (DFUInt(tWidthParamRef), DFUInt(_)) =>
        s"resize($relValStr, ${tWidthParamRef.refCodeString})"
      case (DFSInt(tWidthParamRef), DFSInt(_)) =>
        s"resize($relValStr, ${tWidthParamRef.refCodeString})"
      case (t, DFOpaque(actualType = ot)) if ot =~ t =>
        relValStr
      case (DFOpaque(_, _, _, _), _) =>
        relValStr
      case (DFBit, DFBool) =>
        s"to_sl($relValStr)"
      case (DFBool, DFBit) =>
        s"to_bool($relValStr)"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        s"to_unsigned($relValStr, ${tWidthParamRef.refCodeString})"
      case (DFSInt(tWidthParamRef), DFInt32) =>
        s"to_signed($relValStr, ${tWidthParamRef.refCodeString})"
      case (DFInt32, DFUInt(_) | DFSInt(_)) =>
        s"to_integer($relValStr)"
      case _ => printer.unsupported
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    dfVal.dfType match
      case DFBits(_) =>
        s"${dfVal.relValCodeString}(${dfVal.idxHigh} downto ${dfVal.idxLow})"
      case _ =>
        s"${dfVal.relValCodeString}(${dfVal.idxLow} to ${dfVal.idxHigh})"
    end match
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String =
    val relIdxStr = dfVal.relIdx.refCodeString
    s"${dfVal.relValCodeString}($relIdxStr)"
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
      case DFBit     => "'Z'"
      case DFBits(_) => "(others => 'Z')"
      case _         => printer.unsupported
  def csDFValNamed(dfVal: DFVal): String =
    dfVal.stripPortSel match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case const @ DclConst()    => csDFValDclConst(const)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
      case _                     => ???
  end csDFValNamed
end VHDLValPrinter
