package dfhdl.core
import dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp
import ir.DFConditional.DFCaseBlock.Pattern
import dfhdl.internals.Position
import collection.immutable.ListMap
import dfhdl.internals.metaContextIgnore
import dfhdl.internals.metaContextForward
import dfhdl.compiler.ir.DFConditional
import scala.annotation.Annotation
import dfhdl.hw.annotation.getActiveHWAnnotations
import dfhdl.compiler.printing.{Printer, DefaultPrinter}

object r__For_Plugin:
  def metaGen(
      nameOpt: Option[String],
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): ir.Meta = ir.Meta(nameOpt, position, docOpt, annotations.getActiveHWAnnotations)
  def toFunc1[R](block: => R): () => R = () => block
  def toTuple2[T1, T2](t1: T1, t2: T2): (T1, T2) = (t1, t2)
  def toTuple3[T1, T2, T3](t1: T1, t2: T2, t3: T3): (T1, T2, T3) = (t1, t2, t3)
  def fromBoolean(value: Boolean)(using DFC): DFValOf[DFBool] =
    DFVal.Const(DFBool, Some(value), named = false)
  // tuple of DFVals "concatenated" to be a DFVal of type tuple
  def structToDFVal[V <: DFValAny](product: FieldsOrTuple)(using DFC): V =
    DFVal.OrTupleOrStruct.unapply(product).get.asInstanceOf[V]
  def structDFValSelect[V <: DFValAny](dfVal: DFValAny, fieldName: String)(using
      DFC
  ): V =
    DFVal.Alias
      .SelectField(dfVal, fieldName)(using dfc.anonymize)
      .asInstanceOf[V]
  def patternSingleton(selector: DFValAny, value: Any)(using dfc: DFC): Pattern =
    import dfc.getSet
    given Printer = DefaultPrinter
    val const = (selector.dfType.asIR, value) match
      case (dt: ir.DFBoolOrBit, v: Int) if v == 0 | v == 1 =>
        DFVal.Const(dt.asFE[DFBoolOrBit], Some(v > 0))
      case (dt: ir.DFBoolOrBit, v: Boolean) =>
        DFVal.Const(dt.asFE[DFBoolOrBit], Some(v))
      case (dt: ir.DFBits, allBit: BitOrBool) =>
        val width = dt.widthIntOpt.getOrElse(throw new IllegalArgumentException(
          s"Cannot pattern match against parameterized `${selector.dfType.codeString}` type."
        ))
        // removing width as local parameter dependency in patterns
        val dfType = DFBits(width)
        SameElementsVector.bitsValOf(
          dfType.widthIntParam,
          SameElementsVector(allBit)
        )
      case (dt: ir.DFDecimal, v: Int) =>
        val width = dt.widthIntOpt.getOrElse(throw new IllegalArgumentException(
          s"Cannot pattern match against parameterized `${selector.dfType.codeString}` type."
        ))
        // removing width as local parameter dependency in patterns
        val dfType = dt.runtimeChecked match
          case ir.DFUInt(_) => DFUInt(width)
          case ir.DFSInt(_) => DFSInt(width)
        DFVal.Const(dfType.asFE[DFSInt[Int]], Some(BigInt(v)))
      case (dt: ir.DFEnum, v: DFEncoding) =>
        DFVal.Const(dt.asFE[DFEnum[DFEncoding]], Some(v.bigIntValue))
      case (dt: ir.DFStruct, v) => ???
      case _                    => ???
    DFMatch.Pattern.Singleton(const)
  end patternSingleton
  def patternSingletonSI(si: Any)(using DFC): Pattern =
    si match
      case Some(Seq(value)) =>
        DFMatch.Pattern.Singleton(value.asInstanceOf[DFValAny])
      case _ => println(si); ???
  def patternAlternative(list: List[Pattern]): Pattern =
    Pattern.Alternative(list)
  def patternStruct(name: String, list: List[Pattern]): Pattern =
    Pattern.Struct(name, list)
  def patternCatchAll: Pattern = Pattern.CatchAll
  def patternNamedArg(name: String, pattern: Pattern): Pattern =
    Pattern.NamedArg(name, pattern)
  def extractValDcl[V <: DFValAny](selector: V, extractName: String)(using
      DFC
  ): V =
    val dcl =
      DFVal.Dcl(selector.dfType, Modifier.VAR)(using dfc.setName(extractName))
    dcl.assign(Bubble.constValOf(selector.dfType, named = true))
    dcl.asInstanceOf[V]
  def forcedAssign(toVal: DFValAny, fromVal: DFValAny)(using DFC): Unit =
    toVal.asInstanceOf[DFVarOf[DFTypeAny]].assign(fromVal)
  def bindVal[V <: DFValAny](selector: V, bindName: String)(using DFC): V =
    DFVal.Alias.AsIs.bind(selector, bindName).asInstanceOf[V]
  def bindValRange[V <: DFValAny](
      selector: V,
      bindName: String,
      idxHigh: Int,
      idxLow: Int
  )(using dfc: DFC): V =
    given DFC = dfc.anonymize
    val dfType = selector.dfType.asIR
    val selectorBitsIR: ir.DFVal = dfType match
      case _: ir.DFBits => selector.asIR
      case _            =>
        import DFVal.Ops.bits
        selector.bits(using dfc)(using Width.wide).asIR
    val rangeAlias = DFVal.Alias.ApplyRange(selectorBitsIR.asValOf[DFBits[Int]], idxHigh, idxLow)
    DFVal.Alias.AsIs.bind(rangeAlias, bindName).asInstanceOf[V]
  end bindValRange
  def patternBind(bindVal: DFValAny, pattern: Pattern)(using DFC): Pattern =
    Pattern.Bind(bindVal.asIR.refTW[DFConditional.DFCaseBlock], pattern)
  def patternBindSI(op: String, parts: List[String], bindVals: List[DFValAny])(using
      DFC
  ): Pattern =
    Pattern.BindSI(op, parts, bindVals.map(_.asIR.refTW[DFConditional.DFCaseBlock]))
  // Builds the `__clsAppliedArgs` value for a DFHDL class (see `HasClsArgs`): this class's
  // (name, applied value) parameter pairs.
  def clsAppliedArgs(args: List[(String, DFValAny)]): List[(String, ir.DFVal)] =
    args.map((name, dfVal) => (name, dfVal.asIR))
  // A phantom design parameter (`phantom = true`) materializes an ED method's captured
  // outer constant (see the ed-methods plan): same construction as an explicit `<> CONST`
  // argument parameter, but tagged so printers hide it from signatures and call sites.
  // Its applied value flows per call through the design's `paramMap` (via `constArgs`),
  // exactly like explicit const args — sound under `@hw.pure` memoization.
  @metaContextIgnore
  def genContainerParam[V <: DFValAny](
      appliedVal: DFValAny,
      defaultVal: Option[DFValAny],
      paramMeta: ir.Meta,
      phantom: Boolean
  )(using DFC): V =
    // the applied values are not refernced in the usual way, so we inject a possible
    // global context here.
    appliedVal.asIR.injectGlobalCtx()
    val paramDFC = if (phantom) dfc.tag(ir.PhantomTag) else dfc
    trydf:
      dfc.mutableDB.DesignContext.getReachableNamedValue(
        appliedVal.asIR,
        DFVal.DesignParam(appliedVal, defaultVal)(using paramDFC.setMeta(paramMeta)).asIR
      ).asValAny.asInstanceOf[V]
  end genContainerParam

  @metaContextIgnore
  def designFromDefGetInput[V <: DFValAny](idx: Int)(using DFC): V =
    dfc.mutableDB.DesignContext.getDefInput(idx).asInstanceOf[V]
  @metaContextForward(2)
  def designFromDef[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny)],
      dclMeta: ir.Meta
  )(
      func: => V
  )(using DFC): V =
    designFromDefImpl(ir.DomainType.DF, args, Nil, constArgs, dclMeta)(func)
  // ED methods (HDL functions/tasks — see the ed-methods plan): same construction as DF
  // design defs, but under the ED domain. ED methods are `@hw.pure` by default —
  // `defaultPure` is false only when the user explicitly annotated the def (the plugin
  // checks for the annotation's presence, since an inactive `@hw.pure(false)` is dropped
  // from the meta annotations and would otherwise be indistinguishable from an absent one).
  // `phantomArgs` are the plugin-lifted captured outer references: they become
  // `PhantomTag`-tagged input ports (hidden by the HDL printers), evaluated inside the def
  // at call time and connected in the caller's scope exactly like explicit arguments —
  // which also keeps `@hw.pure` memoization sound (phantoms partake in the cache key and
  // reconnect per call site even on a cache hit).
  @metaContextForward(2)
  def designFromDefED[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      phantomArgs: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny)],
      defaultPure: Boolean,
      dclMeta: ir.Meta
  )(
      func: => V
  )(using DFC): V =
    val updatedMeta =
      if (defaultPure) dclMeta.copy(annotations = ir.annotation.Pure :: dclMeta.annotations)
      else dclMeta
    designFromDefImpl(ir.DomainType.ED, args, phantomArgs, constArgs, updatedMeta)(func)
  private def designFromDefImpl[V <: DFValAny](
      domain: ir.DomainType,
      args: List[(DFValAny, ir.Meta)],
      phantomArgs: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny)],
      dclMeta: ir.Meta
  )(
      func: => V
  )(using DFC): V = trydf:
    val designBlock =
      Design.Block.apply(
        domain = domain,
        instMode = ir.DFDesignBlock.InstMode.Def
      )(using dfc.setMeta(dclMeta))
    dfc.enterOwner(designBlock)
    val explicitInputs = args.map { (arg, argMeta) =>
      DFVal.Dcl(arg.dfType, Modifier.IN)(using dfc.setMeta(argMeta))
    }
    val phantomInputs = phantomArgs.map { (arg, argMeta) =>
      DFVal.Dcl(arg.dfType, Modifier.IN)(using dfc.setMeta(argMeta).tag(ir.PhantomTag))
    }
    val inputs = explicitInputs ++ phantomInputs
    val allArgs = args ++ phantomArgs
    val (isDuplicate, ret) =
      dfc.mutableDB.DesignContext.runFuncWithInputs(func, inputs)
    val paramEntries = Design.Inst.collectParamEntries(clsAppliedArgs(constArgs))
    def exitAndConnectInputs() =
      val endedDesign = designBlock.asIR
      dfc.exitOwner()
      Design.Inst(endedDesign, paramEntries)
      val phantomStartIdx = args.length
      inputs.lazyZip(allArgs).lazyZip(LazyList.from(0)).foreach { case (input, (arg, _), i) =>
        // phantom connects (and the port-by-name selects they create) carry the
        // PhantomTag so printers/stages can detect phantom wiring locally, without
        // resolving the method design's members
        val connDFC =
          if (i >= phantomStartIdx) dfc.anonymize.tag(ir.PhantomTag)
          else dfc.anonymize
        input.connect(arg)(using connDFC)
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
  end designFromDefImpl
  def identVal[V <: DFValAny](value: V)(using DFC): V =
    DFVal.Alias.AsIs.ident(value).asInstanceOf[V]
  object defaults:
    def bool(using DFC): DFConstOf[DFBool] = DFVal.Const.synthetic(DFBool)
    def bit(using DFC): DFConstOf[DFBit] = DFVal.Const.synthetic(DFBit)
    def int32(using DFC): DFConstOf[DFInt32] = DFVal.Const.synthetic(DFInt32)
    def string(using DFC): DFConstOf[DFString] = DFVal.Const.synthetic(DFString)
    def double(using DFC): DFConstOf[DFDouble] = DFVal.Const.synthetic(DFDouble)
    def bits[W <: Int](width: Int)(using DFC): DFConstOf[DFBits[W]] =
      DFVal.Const.synthetic(DFBits.forced[W](width))
    def uint[W <: Int](width: Int)(using DFC): DFConstOf[DFUInt[W]] =
      DFVal.Const.synthetic(DFUInt.forced[W](width))
    def sint[W <: Int](width: Int)(using DFC): DFConstOf[DFSInt[W]] =
      DFVal.Const.synthetic(DFSInt.forced[W](width))
  end defaults
end r__For_Plugin
