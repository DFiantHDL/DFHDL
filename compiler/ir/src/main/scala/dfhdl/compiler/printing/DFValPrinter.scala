package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*
import DFVal.*
import analysis.*
import Func.Op as FuncOp

extension (ref: DFRef.TwoWayAny)
  def refCodeString(using printer: AbstractValPrinter): String = printer.csRef(ref, false)
  def refCodeString(typeCS: Boolean)(using printer: AbstractValPrinter): String =
    printer.csRef(ref, typeCS)

extension (list: List[String])
  def csList(open: String = "(", sep: String = ",", close: String = ")"): String =
    // Function to find the largest divisor less than or equal to the limit
    def findLargestDivisor(n: Int, limit: Int): Int =
      var maxDivisor = 1
      for (i <- 1 to math.sqrt(n).toInt if n % i == 0)
        if (i <= limit) maxDivisor = math.max(maxDivisor, i)
        if (n / i <= limit) maxDivisor = math.max(maxDivisor, n / i)
      maxDivisor
    val maxVectorLineCharacters: Int = 120
    val avgTextLength = list.view.map(_.length).sum / list.length
    val maxColElems = (maxVectorLineCharacters / avgTextLength).max(1)
    // the ideal column count is large and divisible by the number of elements
    var colCnt = findLargestDivisor(list.length, maxColElems)
    // however, if the number of columns is less than half of the maximum number
    // of columns, then use the maximum number of columns. this is to avoid the
    // case where the vector is printed very few columns, which is not readable.
    if (colCnt <= maxColElems / 2) colCnt = maxColElems
    val rowCnt = (list.length - 1) / colCnt + 1
    if (rowCnt == 1) list.mkString(open, sep + " ", close)
    else
      val csVecData = list.grouped(colCnt).map(_.mkString(sep + " ")).mkString(sep + "\n").hindent
      s"$open\n${csVecData}\n$close"
end extension

extension (intParamRef: IntParamRef)
  def refCodeString(typeCS: Boolean)(using printer: AbstractValPrinter): String = intParamRef match
    case ref: DFRef.TwoWayAny => printer.csRef(ref, typeCS)
    case int: Int             => int.toString
  def refCodeString(using printer: AbstractValPrinter): String = intParamRef.refCodeString(false)
  def uboundCS(using printer: AbstractValPrinter): String = intParamRef match
    case ref: DFRef.TwoWayAny =>
      // TODO: consider implementing an associative int operation reduction
      // import printer.getSet
      // ref.get match
      //   case func @ ir.DFVal.Func(
      //         ir.DFInt32,
      //         op @ (Func.Op.+ | Func.Op.-),
      //         List(argRef, ir.DFRef(const: ir.DFVal.Const)),
      //         _,
      //         _,
      //         _
      //       ) =>
      //     val int = const.data.asInstanceOf[Option[BigInt]].get.toInt
      //     val csArg = printer.csRef(argRef, false)
      //     if (int == 1) csArg
      //     else s"$csArg $op ${int - 1}"
      //   case _ =>
      s"${printer.csRef(ref, false)} - 1"
    case int: Int => (int - 1).toString
end extension

extension (alias: Alias)
  def relValCodeString(using printer: AbstractValPrinter): String = printer.csRelVal(alias)

trait AbstractValPrinter extends AbstractPrinter:
  final def csSimpleRef(ref: DFRef.TwoWayAny): String =
    ref.get match
      case DFVal.Const(dfType = _: DFDecimal, data = Some(i)) => i.toString
      case _                                                  => ref.refCodeString
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String
  def csDFMemberName(named: DFMember.Named): String =
    named.getName
  final def csRef(ref: DFRef.TwoWayAny, typeCS: Boolean): String =
    val member = ref.get
    extension (named: DFMember.Named)
      def nameCS: String = if (typeCS) s"${csDFMemberName(named)}.type" else csDFMemberName(named)
    member match
      case dfVal: DFVal.DesignParam                   => dfVal.nameCS
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        if (dfVal.isAnonymous) printer.csDFValExpr(dfVal)
        else dfVal.nameCS
      case dfVal: DFVal =>
        val callOwner = ref.originMember.getOwner
        val cs = printer.csDFValRef(dfVal, callOwner)
        val ret = dfVal match
          case ch: DFConditional.Header if ch.isAnonymous => csConditionalExprRel(cs, ch)
          case _                                          => cs
        if (dfVal.isAnonymous || !typeCS) ret
        else s"$ret.type"
      case named: DFMember.Named =>
        named.nameCS
      case _ =>
        throw new IllegalArgumentException("Fetching refCodeString from irrelevant member.")
    end match
  end csRef
  final def csRelVal(alias: Alias): String =
    alias.relValRef.refCodeString.applyBrackets()
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String
  final def csDFValConstExpr(dfVal: Const): String =
    printer.csConstData(dfVal.dfType, dfVal.data)
  def csDFValDclWithoutInit(dfVal: Dcl): String
  def csInitKeyword: String
  def csInitSingle(ref: Dcl.InitRef): String
  def csInitSeq(refs: List[Dcl.InitRef]): String
  def csDFValDclEnd(dfVal: Dcl): String
  val supportVectorInlineInit: Boolean = true
  final def csDFValDcl(dfVal: Dcl): String =
    val noInit = csDFValDclWithoutInit(dfVal)
    val init = dfVal.initRefList match
      // case DFRef(DFVector.Val(_)) :: _ if !printer.supportVectorInlineInit => ""
      case _
          if dfVal.dfType.isInstanceOf[DFVector] && !printer.supportVectorInlineInit => ""
      case DFRef(DFVal.Func(op = FuncOp.InitFile(format, path))) :: Nil =>
        val csInitFile = format match
          case InitFileFormat.Auto => s""""$path""""
          case _                   => s"""("$path", InitFileFormat.$format)"""
        s" initFile $csInitFile"
      case ref :: Nil => s" $csInitKeyword ${csInitSingle(ref)}"
      case Nil        => ""
      case refs       => s" $csInitKeyword ${csInitSeq(refs)}"
    val end = csDFValDclEnd(dfVal)
    s"$noInit$init$end"
  end csDFValDcl
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String
  def csDFValAliasAsIs(dfVal: Alias.AsIs): String
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String
  def csDFValAliasSelectField(dfVal: Alias.SelectField): String
  def csDFValAliasHistory(dfVal: Alias.History): String
  // def csTimerIsActive(dfVal: Timer.IsActive): String
  def csNOTHING(dfVal: NOTHING): String
  final def csDFValAliasExpr(dfVal: Alias): String = dfVal match
    case dv: Alias.AsIs        => csDFValAliasAsIs(dv)
    case dv: Alias.History     => csDFValAliasHistory(dv)
    case dv: Alias.ApplyRange  => csDFValAliasApplyRange(dv)
    case dv: Alias.ApplyIdx    => csDFValAliasApplyIdx(dv)
    case dv: Alias.SelectField => csDFValAliasSelectField(dv)
  final def csDFValExpr(dfValExpr: DFVal.CanBeExpr, typeCS: Boolean = false): String =
    dfValExpr match
      case dv: Const                => csDFValConstExpr(dv)
      case dv: Func                 => csDFValFuncExpr(dv, typeCS)
      case dv: Alias                => csDFValAliasExpr(dv)
      case dv: DFVal.DesignParam    => dv.dfValRef.refCodeString
      case dv: DFConditional.Header => printer.csDFConditional(dv)
      // case dv: Timer.IsActive       => csTimerIsActive(dv)
      case dv: NOTHING => csNOTHING(dv)
  def csDFValNamed(dfVal: DFVal): String
  final def csDFValRef(dfVal: DFVal, fromOwner: DFOwner | DFMember.Empty): String =
    dfVal.stripPortSel match
      case expr: CanBeExpr if expr.isAnonymous   => csDFValExpr(expr)
      case PortOfDesignDef(Modifier.OUT, design) =>
        if (design.isAnonymous) printer.csDFDesignDefInst(design)
        else design.getName
      case open: DFVal.OPEN => printer.csOpenKeyWord
      case dfVal            => dfVal.getRelativeName(fromOwner)
end AbstractValPrinter

protected trait DFValPrinter extends AbstractValPrinter:
  type TPrinter <: DFPrinter
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String =
    s"(${csExp.applyBrackets()}: ${printer.csDFType(ch.dfType, typeCS = true)} <> VAL)"
  def csDFValDclConst(dfVal: DFVal.CanBeExpr): String =
    s"${csDFValExpr(dfVal)}"
  def csDFValDclModifier(modifier: Modifier): String = modifier.toString
  def csDFValDclWithoutInit(dfVal: Dcl): String =
    s"${printer.csDFType(dfVal.dfType)} <> ${csDFValDclModifier(dfVal.modifier)}"
  def csInitKeyword: String = "init"
  def csInitSingle(ref: Dcl.InitRef): String = ref.refCodeString
  def csInitSeq(refs: List[Dcl.InitRef]): String = refs.view.map(_.refCodeString).mkStringBrackets
  def csDFValDclEnd(dfVal: Dcl): String = ""
  def csDFValFuncExpr(dfVal: Func, typeCS: Boolean): String =
    def commonOpStr: String = (dfVal.op, dfVal.dfType) match
      case (Func.Op.===, _) => "=="
      case (Func.Op.=!=, _) => "!="
      // boolean logical operations
      case (Func.Op.| | Func.Op.&, DFBit | DFBool) => s"${dfVal.op}${dfVal.op}"
      case (op, _)                                 => op.toString
    dfVal.args match
      // boolean sel function
      case cond :: onTrue :: onFalse :: Nil if dfVal.op == Func.Op.sel =>
        s"${cond.refCodeString.applyBrackets()}.sel(${onTrue.refCodeString}, ${onFalse.refCodeString})"
      // repeat func
      case argL :: argR :: Nil if dfVal.op == Func.Op.repeat =>
        dfVal.dfType match
          case _: DFVector => s"all(${argL.refCodeString})"
          case _           =>
            val csArgL = argL.refCodeString(typeCS)
            val csArgR = argR.refCodeString(typeCS)
            s"${csArgL.applyBrackets()}.repeat${csArgR.applyBrackets(onlyIfRequired = false)}"
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val csArgL = argL.refCodeString(typeCS)
        val csArgR = argR.refCodeString(typeCS)
        val opStr = dfVal.op match
          // if the result width for +/-/* ops is larger than the left argument width
          // then we have a carry-inclusive operation
          case Func.Op.+ | Func.Op.- | Func.Op.`*` if dfVal.dfType.width > argL.get.dfType.width =>
            s"${dfVal.op}^"
          case op => commonOpStr
        s"${csArgL.applyBrackets()} $opStr ${csArgR.applyBrackets()}"
      // unary/postfix func
      case arg :: Nil =>
        val csArg = arg.refCodeString(typeCS)
        val opStr = dfVal.op.toString
        dfVal.op match
          case Func.Op.unary_! | Func.Op.unary_- | Func.Op.unary_! | Func.Op.unary_~ =>
            s"${opStr.last}${csArg.applyBrackets()}"
          case Func.Op.clog2 =>
            if (typeCS) s"CLog2[$csArg]"
            else s"clog2($csArg)"
          case _ => s"${csArg.applyBrackets()}.${opStr}"
      // multiarg func
      case args =>
        val csArgs = args.map(_.refCodeString)
        dfVal.op match
          case DFVal.Func.Op.++ =>
            def argsInBrackets = csArgs.csList()
            dfVal.dfType match
              case DFString =>
                if (csArgs.length == 2)
                  s"${csArgs.head.applyBrackets()} + ${csArgs.last.applyBrackets()}"
                else ??? // TODO: handle more than 2 args
              case structType @ DFStruct(structName, fieldMap) =>
                if (structType.isTuple) argsInBrackets
                else
                  structType.name +
                    fieldMap
                      .lazyZip(csArgs)
                      .map { case ((n, _), r) =>
                        s"$n = $r"
                      }
                      .mkStringBrackets
              case dfType @ DFVector(_, _) =>
                s"DFVector(${printer.csDFVector(dfType, typeCS = false)})${csArgs.csList()}"
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"${(csArgs.head).applyBrackets()}.repeat(${args.length})"
              // regular concatenation function
              case _ => s"$argsInBrackets.toBits"
            end match
          case _ =>
            csArgs
              .map(_.applyBrackets())
              .mkString(s" ${commonOpStr} ")
        end match
    end match
  end csDFValFuncExpr
  def csDFValAliasAsIs(dfVal: Alias.AsIs): String =
    val relVal = dfVal.relValRef.get
    val relValStr = dfVal.relValCodeString
    val fromType = relVal.dfType
    val toType = dfVal.dfType
    (toType, fromType) match
      case (t, f) if t == f => // ident
        // an ident is used as a placeholder and therefore does not require
        // applying brackets
        val callOwner = dfVal.ownerRef.get
        printer.csDFValRef(relVal, callOwner)
      case (DFSInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
        assert(tWidth == fWidth + 1)
        s"${relValStr}.signed"
      case (DFUInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        s"${relValStr}.uint"
      case (DFSInt(Int(tWidth)), DFBits(Int(fWidth))) =>
        assert(tWidth == fWidth)
        s"${relValStr}.sint"
      case (DFBits(tWidthParamRef), DFBits(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFBits(Int(tWidth)), _) =>
        assert(tWidth == fromType.width)
        s"${relValStr}.bits"
      case (DFUInt(tWidthParamRef), DFUInt(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFInt32, DFSInt(_)) =>
        s"${relValStr}.toInt"
      case (DFSInt(tWidthParamRef), DFSInt(_)) =>
        s"${relValStr}.resize(${tWidthParamRef.refCodeString})"
      case (DFBit, DFBool) =>
        s"${relValStr}.bit"
      case (DFBool, DFBit) =>
        s"${relValStr}.bool"
      case (t, DFOpaque(actualType = ot)) if ot == t =>
        s"${relValStr}.actual"
      case (_, DFBits(_)) | (DFOpaque(_, _, _, _), _) =>
        s"${relValStr}.as(${printer.csDFType(toType)})"
      case (DFUInt(tWidthParamRef), DFInt32) =>
        s"""d"${printer.csWidthInterp(tWidthParamRef)}'$${${relValStr}}""""
      case (DFSInt(tWidthParamRef), DFInt32) =>
        s"""sd"${printer.csWidthInterp(tWidthParamRef)}'$${${relValStr}}""""
      case (DFInt32, DFUInt(_) | DFSInt(_)) =>
        s"${relValStr}.toInt"
      case _ =>
        throw new IllegalArgumentException("Unsupported alias/conversion")
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    dfVal.dfType match
      case DFBits(_) =>
        s"${dfVal.relValCodeString}(${dfVal.idxHighRef.refCodeString}, ${dfVal.idxLowRef.refCodeString})"
      case _ =>
        s"${dfVal.relValCodeString}(${dfVal.idxLowRef.refCodeString}, ${dfVal.idxHighRef.refCodeString})"
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
  def csDFValAliasHistory(dfVal: Alias.History): String =
    val opStr = dfVal.op match
      case Alias.History.Op.State =>
        dfVal.getOwnerDomain.domainType match
          case DomainType.DF    => ".prev"
          case DomainType.RT(_) => ".reg"
          case DomainType.ED    => ??? // impossible!
      case Alias.History.Op.Pipe => ".pipe"
    val appliedStr =
      dfVal.initRefOption match
        case Some(ref)            => s"$opStr(${dfVal.step}, init = ${ref.refCodeString})"
        case _ if dfVal.step == 1 => opStr
        case _                    => s"$opStr(${dfVal.step})"
    s"${dfVal.relValCodeString}$appliedStr"
  end csDFValAliasHistory
  // def csTimerIsActive(dfVal: Timer.IsActive): String =
  //   s"${dfVal.timerRef.refCodeString}.isActive"
  def csNOTHING(dfVal: NOTHING): String = "NOTHING"
  def csDFValNamed(dfVal: DFVal): String =
    def typeAnnot = dfVal match
      case dv: DFConditional.Header if dv.dfType != DFUnit => printer.csDFValType(dfVal.dfType)
      case const @ DclConst()                              => printer.csDFValConstType(dfVal.dfType)
      case _                                               => ""
    def valDef = s"val ${dfVal.getName}$typeAnnot ="
    val rhs = dfVal.stripPortSel match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case const @ DclConst()    => csDFValDclConst(const)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
      case _                     => ??? // unexpected
    val indentRHS =
      if (rhs.contains("\n")) s"\n${rhs.hindent}"
      else s" ${rhs}"
    s"$valDef$indentRHS"
  end csDFValNamed
end DFValPrinter
