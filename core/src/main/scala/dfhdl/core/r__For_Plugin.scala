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
  @metaContextIgnore
  def genContainerParam[V <: DFValAny](
      appliedVal: DFValAny,
      defaultVal: Option[DFValAny],
      paramMeta: ir.Meta
  )(using DFC): V =
    // the applied values are not refernced in the usual way, so we inject a possible
    // global context here.
    appliedVal.asIR.injectGlobalCtx()
    trydf:
      dfc.mutableDB.DesignContext.getReachableNamedValue(
        appliedVal.asIR,
        DFVal.DesignParam(appliedVal, defaultVal)(using dfc.setMeta(paramMeta)).asIR
      ).asValAny.asInstanceOf[V]

  @metaContextIgnore
  def designFromDefGetInput[V <: DFValAny](idx: Int)(using DFC): V =
    dfc.mutableDB.DesignContext.getDefInput(idx).asInstanceOf[V]
  @metaContextIgnore
  def designFromDefGetParam[V <: DFValAny](idx: Int)(using DFC): V =
    dfc.mutableDB.DesignContext.getDefParam(idx).asInstanceOf[V]
  @metaContextForward(2)
  def designFromDef[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny, ir.Meta)],
      dclMeta: ir.Meta,
      scalaArgs: List[Any],
      phantomArgs: List[(DFValAny, ir.Meta)],
      phantomConstArgs: List[(DFValAny, ir.Meta)]
  )(
      func: => V
  )(using DFC): V = trydf:
    val designBlock =
      Design.Block.apply(
        domain = ir.DomainType.DF,
        instMode = ir.DFDesignBlock.InstMode.Def
      )(using dfc.setMeta(dclMeta))
    dfc.enterOwner(designBlock)
    // deterministic phantom naming: after the captured value itself (its own meta, exactly
    // like `cloneUnreachable` auto-parameters), falling back to the plugin-provided meta
    // (leaf name + declaration position) for anonymous applied values
    def phantomMeta(arg: DFValAny, fallback: ir.Meta): ir.Meta =
      if (arg.asIR.meta.isAnonymous) fallback else arg.asIR.meta
    // Phantom input ports materialize the def body's captured non-constant DFHDL values,
    // making the generated design self-contained. They are appended after the explicit
    // inputs (the body fetches both through the same `designFromDefGetInput` index space)
    // and tagged so the design-def view form hides them.
    val inputs = args.map { (arg, argMeta) =>
      DFVal.Dcl(arg.dfType, Modifier.IN)(using dfc.setMeta(argMeta))
    } ++ phantomArgs.map { (arg, fallbackMeta) =>
      DFVal.Dcl(arg.dfType, Modifier.IN)(using
        dfc.setMeta(phantomMeta(arg, fallbackMeta)).tag(ir.PhantomTag)
      )
    }
    // The design parameters are created by this harness rather than by the body, so a pure
    // cache hit, which skips the body, still creates fresh parameters bound to this call's
    // applied values (the body fetches them via `designFromDefGetParam`). Phantom
    // parameters (captured constants) are appended after the explicit ones and tagged.
    val params = constArgs.map { (_, arg, argMeta) =>
      genContainerParam[DFValAny](arg, None, argMeta)
    } ++ phantomConstArgs.map { (arg, fallbackMeta) =>
      genContainerParam[DFValAny](arg, None, phantomMeta(arg, fallbackMeta))(using
        dfc.tag(ir.PhantomTag)
      )
    }
    // all the design's (name, applied value) parameter entries, explicit and phantom
    val namedConstArgs = constArgs.map((name, arg, _) => (name, arg)) ++
      phantomConstArgs.map((arg, fallbackMeta) => (phantomMeta(arg, fallbackMeta).name, arg))
    // Params named data-impure by the design def's `pure` annotation (synthesized by the
    // PureCheck plugin phase or declared by the user) contribute their applied type+data to
    // the elaboration cache key. Phantom parameters fit the same scheme: PureCheck records
    // their predicted names on the annotation like any explicit parameter's. Unknown applied
    // data (no snapshot, e.g. unattainable during this elaboration) yields None, which makes
    // this call uncacheable (runs live; structural dedup still unifies identical bodies).
    val impureParamsKeyOpt: Option[List[Any]] =
      val impureParamNames = dclMeta.annotations.collectFirst {
        case ir.annotation.Pure(true, names) if names.nonEmpty => names.toSet
      }.getOrElse(Set.empty)
      if (impureParamNames.isEmpty) Some(Nil)
      else
        // `"*"` (e.g. from a user-written `@pure(true, "*")`) marks ALL params data-impure
        val allImpure = impureParamNames.contains("*")
        val keyPartOpts = params.zip(namedConstArgs).collect {
          case (param, (name, _)) if allImpure || impureParamNames.contains(name) =>
            param.asIR match
              case dp: ir.DFVal.DesignParam => dp.appliedData.map(data => (dp.dfType, data))
              case _                        => None
        }
        if (keyPartOpts.forall(_.isDefined)) Some(keyPartOpts.map(_.get)) else None
    end impureParamsKeyOpt
    val (isDuplicate, ret, paramEntries) =
      dfc.mutableDB.DesignContext.runFuncWithInputs(
        func, inputs, params, scalaArgs, impureParamsKeyOpt
      ):
        Design.Inst.collectParamEntries(clsAppliedArgs(namedConstArgs))
    def exitAndConnectInputs() =
      val endedDesign = designBlock.asIR
      dfc.exitOwner()
      Design.Inst(endedDesign, paramEntries)
      val allArgs = args.map(_._1) ++ phantomArgs.map(_._1)
      val phantomFlags = List.fill(args.length)(false) ++ List.fill(phantomArgs.length)(true)
      inputs.lazyZip(allArgs).lazyZip(phantomFlags).foreach { (input, arg, isPhantom) =>
        // a phantom input's call-site wiring is tagged through its port selection, which
        // the design-def view form uses to hide the connection
        val connDFC = if (isPhantom) dfc.anonymize.tag(ir.PhantomTag) else dfc.anonymize
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
  end designFromDef
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
