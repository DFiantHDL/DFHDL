package dfhdl.core
import dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp
import ir.DFConditional.DFCaseBlock.Pattern

import collection.immutable.ListMap
import dfhdl.internals.metaContextIgnore
import dfhdl.internals.metaContextForward
object __For_Plugin:
  def toFunc1[R](block: => R): () => R = () => block
  def toTuple2[T1, T2](t1: T1, t2: T2): (T1, T2) = (t1, t2)
  def toTuple3[T1, T2, T3](t1: T1, t2: T2, t3: T3): (T1, T2, T3) = (t1, t2, t3)
  def fromBoolean(value: Boolean)(using DFC): DFValOf[DFBool] =
    DFVal.Const(DFBoolOrBit.Token(DFBool, value))
  // tuple of DFVals "concatenated" to be a DFVal of type tuple
  def structToDFVal[V <: DFValAny](product: FieldsOrTuple)(using DFC): V =
    DFVal.OrTupleOrStruct.unapply(product).get.asInstanceOf[V]
  def structDFValSelect[V <: DFValAny](dfVal: DFValAny, fieldName: String)(using
      DFC
  ): V =
    DFVal.Alias
      .SelectField(dfVal, fieldName)(using dfc.anonymize)
      .asInstanceOf[V]
  def patternSingleton(selector: DFValAny, value: Any): Pattern =
    val tokenIR = (selector.dfType.asIR, value) match
      case (dt: ir.DFBoolOrBit, v: Int) if v == 0 | v == 1 =>
        ir.DFBoolOrBit.Token(dt, Some(v > 0))
      case (dt: ir.DFBoolOrBit, v: Boolean) =>
        ir.DFBoolOrBit.Token(dt, Some(v))
      case (dt: ir.DFBits, allBit: BitOrBool) =>
        DFBits.Token(dt.width, SameElementsVector(allBit)).asIR
      case (dt: ir.DFDecimal, v: Int) =>
        ir.DFDecimal.Token(dt, Some(BigInt(v)))
      case (dt: ir.DFEnum, v: DFEncoding) =>
        ir.DFEnum.Token(dt, Some(v.bigIntValue))
      case (dt: ir.DFStruct, v) => ???
      case _                    => ???
    Pattern.Singleton(tokenIR)
  end patternSingleton
  def patternSingletonSI(si: Any): Pattern =
    si match
      case Some(Seq(DFVal(const: ir.DFVal.Const))) =>
        Pattern.Singleton(const.token)
      case _ => println(si); ???
  def patternAlternative(list: List[Pattern]): Pattern =
    Pattern.Alternative(list)
  def patternStruct(name: String, list: List[Pattern]): Pattern =
    Pattern.Struct(name, list)
  def patternCatchAll: Pattern = Pattern.CatchAll
  def extractValDcl[V <: DFValAny](selector: V, extractName: String)(using
      DFC
  ): V =
    val dcl =
      DFVal.Dcl(selector.dfType, Modifier.VAR)(using dfc.setName(extractName))
    dcl.assign(DFVal.Const(Bubble(selector.dfType)))
    dcl.asInstanceOf[V]
  def forcedAssign(toVal: DFValAny, fromVal: DFValAny)(using DFC): Unit =
    toVal.asInstanceOf[DFVarOf[DFTypeAny]].assign(fromVal)
  def bindVal[V <: DFValAny](selector: V, bindName: String)(using DFC): V =
    DFVal.Alias.AsIs.bind(selector, bindName).asInstanceOf[V]
  def bindValRange[V <: DFValAny](
      selector: V,
      bindName: String,
      relBitHigh: Int,
      relBitLow: Int
  )(using dfc: DFC): V =
    given DFC = dfc.anonymize
    val dfType = selector.dfType.asIR
    val selectorBitsIR: ir.DFVal = dfType match
      case _: ir.DFBits => selector.asIR
      case _ =>
        import DFVal.Ops.bits
        selector.bits(using Width.wide).asIR
    val rangeAlias = DFVal.Alias
      .ApplyRange(selectorBitsIR.asValOf[DFBits[Int]], relBitHigh, relBitLow)
    DFVal.Alias.AsIs.bind(rangeAlias, bindName).asInstanceOf[V]
  end bindValRange
  def patternBind(bindVal: DFValAny, pattern: Pattern)(using DFC): Pattern =
    Pattern.Bind(bindVal.asIR.ref, pattern)
  def patternBindSI(op: String, parts: List[String], bindVals: List[DFValAny])(using
      DFC
  ): Pattern =
    Pattern.BindSI(op, parts, bindVals.map(_.asIR.ref))
  @metaContextIgnore
  def genDesignParam[V <: DFValAny](paramValue: DFValAny, paramMeta: ir.Meta)(using dfc: DFC): V =
    import ir.DFVal.Alias.DesignParamTag
    given DFC = dfc.setMeta(paramMeta)
    DFVal.Alias.AsIs(paramValue.dfType, paramValue, x => x, forceNewAlias = true)
      .tag(DesignParamTag).asInstanceOf[V]
  @metaContextIgnore
  def designFromDefGetInput[V <: DFValAny](idx: Int)(using DFC): V =
    dfc.mutableDB.DesignContext.getDefInput(idx).asInstanceOf[V]
  @metaContextForward(2)
  def designFromDef[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      dclMeta: ir.Meta
  )(
      func: => V
  )(using DFC): V = trydf:
    val designBlock =
      Design.Block.apply(
        domain = ir.DomainType.DF,
        dclMeta = dclMeta,
        instMode = ir.DFDesignBlock.InstMode.Def
      )
    dfc.enterOwner(designBlock)
    val inputs = args.map { (arg, argMeta) =>
      DFVal.Dcl(arg.dfType, Modifier.IN)(using dfc.setMeta(argMeta))
    }
    val (isDuplicate, ret): (Boolean, V) =
      dfc.mutableDB.DesignContext.runFuncWithInputs(func, inputs)
    def exitAndConnectInputs() =
      dfc.exitOwner()
      inputs.lazyZip(args).foreach { case (input, (arg, _)) =>
        input.connect(arg)(using dfc.anonymize)
      }
    def genOutPort = DFVal.Dcl(ret.dfType, Modifier.OUT)(using dfc.setName("o"))
    if (ret.dfType.asIR == ir.DFUnit)
      exitAndConnectInputs()
      DFUnitVal().asInstanceOf[V]
    else if (isDuplicate)
      val output = genOutPort
      exitAndConnectInputs()
      output.asInstanceOf[V]
    else
      val retMeta = ret.asIR.meta
      val retIdent = DFVal.Alias.AsIs.ident(ret)(using dfc.setMeta(retMeta).anonymize)
      val output = genOutPort
      output.connect(retIdent)(using dfc.setMeta(retMeta.anonymize))
      exitAndConnectInputs()
      output.asInstanceOf[V]
    end if
  end designFromDef

end __For_Plugin
