package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VHDLValPrinter extends AbstractValPrinter:
  type TPrinter <: VHDLPrinter
  def csConditionalExprRel(csExp: String, ch: DFConditional.Header): String = printer.unsupported
  def csDFValConstDcl(dfVal: Const): String =
    s"constant ${dfVal.name} : ${printer.csDFType(dfVal.dfType)} := ${printer.csDFToken(dfVal.token)}"
  def csDFValDcl(dfVal: Dcl): String =
    val dfTypeStr = printer.csDFType(dfVal.dfType)
    val noInit =
      if (dfVal.isPort) s"${dfVal.name} : ${dfVal.modifier.toString.toLowerCase} $dfTypeStr"
      else
        val sigOrVar = dfVal.getOwnerNamed match
          case dsn: DFDesignBlock => "signal"
          case _                  => "variable"
        s"$sigOrVar ${dfVal.name} : $dfTypeStr"
    dfVal.getTagOf[ExternalInit] match
      case Some(ExternalInit(initSeq)) if initSeq.size > 1 => printer.unsupported
      case Some(ExternalInit(initSeq)) if initSeq.size == 1 =>
        s"$noInit := ${printer.csDFToken(initSeq.head)}"
      case _ => noInit

  def csDFValFuncExpr(dfVal: Func): String =
    dfVal.args match
      // infix func
      case argL :: argR :: Nil if dfVal.op != Func.Op.++ =>
        val opStr = dfVal.op match
          case Func.Op.=== => "="
          case Func.Op.=!= => "/="
          case Func.Op.|   => "or"
          case Func.Op.&   => "and"
          case Func.Op.^   => "xor"
          case Func.Op.>=  => "=>"
          case Func.Op.++  => "&"
          case Func.Op.<< =>
            argL.get.dfType match
              case DFSInt(_) => "sla"
              case _         => "sll"
          case Func.Op.>> =>
            argL.get.dfType match
              case DFSInt(_) => "sra"
              case _         => "srl"
          // if the result width for +/-/* ops is larger than the left argument width
          // then we have a carry-inclusive operation
          case Func.Op.+ | Func.Op.- | Func.Op.`*` if dfVal.dfType.width > argL.get.dfType.width =>
            printer.unsupported
          case op => op.toString
        val rhsStr = dfVal.op match
          case Func.Op.>> | Func.Op.<< => argR.simpleRefCodeString
          case _                       => argR.refCodeString
        s"${argL.refCodeString.applyBrackets()} $opStr ${rhsStr.applyBrackets()}"
      // unary/postfix func
      case arg :: Nil =>
        val argStr = arg.refCodeString.applyBrackets()
        dfVal.op match
          case Func.Op.rising  => s"rising_edge($argStr)"
          case Func.Op.falling => s"falling_edge($argStr)"
          case Func.Op.unary_- => s"-$argStr"
          case Func.Op.unary_! => s"not $argStr"
          case Func.Op.unary_~ => s"not $argStr"
          case _               => printer.unsupported
      // multiarg func
      case args =>
        dfVal.op match
          case DFVal.Func.Op.++ =>
            dfVal.dfType match
              case DFStruct(_, _) => printer.unsupported
              case DFVector(_, _) => printer.unsupported
              // all args are the same ==> repeat function
              case _ if args.view.map(_.get).allElementsAreEqual =>
                s"repeat(${args.head.refCodeString},${args.length})"
              // regular concatenation function
              case _ => args.map(_.refCodeString).mkString(" & ")
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
      case (t, f) if t == f => printer.unsupported
      case (DFSInt(tWidth), DFUInt(fWidth)) =>
        assert(tWidth == fWidth + 1)
        s"signed($relValStr)"
      case (DFUInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"unsigned($relValStr)"
      case (DFSInt(tWidth), DFBits(fWidth)) =>
        assert(tWidth == fWidth)
        s"signed($relValStr)"
      case (DFBits(tWidth), DFBits(_)) =>
        s"resize($relValStr, $tWidth)"
      case (DFBits(tWidth), _) =>
        assert(tWidth == fromType.width)
        fromType match
          case _ => s"to_slv($relValStr)"
      case (DFUInt(tWidth), DFUInt(_)) =>
        s"resize($relValStr, $tWidth)"
      case (DFSInt(tWidth), DFSInt(_)) =>
        s"resize($relValStr, $tWidth)"
      case (DFBit, DFBool) =>
        s"to_sl($relValStr)"
      case (DFBool, DFBit) =>
        s"to_bool($relValStr)"
      case _ => printer.unsupported
    end match
  end csDFValAliasAsIs
  def csDFValAliasApplyRange(dfVal: Alias.ApplyRange): String =
    s"${dfVal.relValCodeString}(${dfVal.relBitHigh}, ${dfVal.relBitLow})"
  def csDFValAliasApplyIdx(dfVal: Alias.ApplyIdx): String =
    val relIdxStr = dfVal.relIdx.simpleRefCodeString
    s"${dfVal.relValCodeString}($relIdxStr)"
  // when the tuple field number exceeds this number, the tuple
  // field selections changes from `dv._${idx+1}` to `dv($idx)`
  val TUPLE_MIN_INDEXING = 3
  def csDFValAliasSelectField(dfVal: Alias.SelectField): String =
    val DFStruct(structName, fieldMap) = dfVal.relValRef.get.dfType
    val fieldSel =
      if (structName.isEmpty)
        if (fieldMap.size > TUPLE_MIN_INDEXING)
          s"(${dfVal.fieldName.drop(1).toInt - 1})"
        else s".${dfVal.fieldName}"
      else s".${dfVal.fieldName}"
    s"${dfVal.relValCodeString}$fieldSel"
  def csDFValAliasHistory(dfVal: Alias.History): String = printer.unsupported
  def csDFValAliasRegDIN(dfVal: Alias.RegDIN): String = printer.unsupported
  def csTimerIsActive(dfVal: Timer.IsActive): String = printer.unsupported
  def csDFValNamed(dfVal: DFVal): String =
    dfVal match
      case dcl: DFVal.Dcl        => csDFValDcl(dcl)
      case c: DFVal.Const        => csDFValConstDcl(c)
      case expr: DFVal.CanBeExpr => csDFValExpr(expr)
  end csDFValNamed
end VHDLValPrinter
