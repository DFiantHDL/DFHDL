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

  // A design class's applied parameter, lifted OUT of the class body by the plugin
  // (`DesignClsSkipPhase`): the harness (`Design.__clsBodyGate`) creates the design's parameter
  // members from these before the body runs, exactly as `designFromDef` does for a design def, and
  // the body's parameter declarations fetch them back (`Design.__clsGetParam`). The design's public
  // interface is thus in place before the body-skip gate decides, and a skipped body creates
  // nothing the instantiation site needs.
  final case class ClsParam(applied: DFValAny, default: Option[DFValAny], meta: ir.Meta)
  @metaContextIgnore
  def clsParam(applied: DFValAny, default: Option[DFValAny], meta: ir.Meta): ClsParam =
    ClsParam(applied, default, meta)

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
      phantomConstArgs: List[(DFValAny, ir.Meta)],
      ownerClass: Class[?]
  )(
      func: => V
  )(using DFC): V =
    designFromDefImpl(ir.DomainType.DF, args, constArgs, dclMeta, scalaArgs, phantomArgs,
      phantomConstArgs, ownerClass)(func)
  // ED methods (HDL functions/tasks/procedures — see the ed-methods plan): same
  // construction, caching, and purity treatment as DF design defs, but under the ED
  // domain (the design prints as an HDL subprogram rather than a module).
  @metaContextForward(2)
  def designFromDefED[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny, ir.Meta)],
      dclMeta: ir.Meta,
      scalaArgs: List[Any],
      phantomArgs: List[(DFValAny, ir.Meta)],
      phantomConstArgs: List[(DFValAny, ir.Meta)],
      ownerClass: Class[?]
  )(
      func: => V
  )(using DFC): V =
    designFromDefImpl(ir.DomainType.ED, args, constArgs, dclMeta, scalaArgs, phantomArgs,
      phantomConstArgs, ownerClass)(func)
  // Static functions (`<> CONSTRET` — see the static-domain plan): same construction, caching,
  // and purity treatment as the other design defs, but under the static domain.
  //
  // The plugin requires every DFHDL argument of a static function to be `<> CONST` and every
  // capture to be a constant, so `args` and `phantomArgs` are always empty and the formals are
  // exactly the const-arg/const-capture input ports the subprogram path of the impl below
  // creates (plus the return port).
  @metaContextForward(2)
  def designFromDefStatic[V <: DFValAny](
      args: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny, ir.Meta)],
      dclMeta: ir.Meta,
      scalaArgs: List[Any],
      phantomArgs: List[(DFValAny, ir.Meta)],
      phantomConstArgs: List[(DFValAny, ir.Meta)],
      ownerClass: Class[?]
  )(
      func: => V
  )(using DFC): V =
    designFromDefImpl(ir.DomainType.Static, args, constArgs, dclMeta, scalaArgs, phantomArgs,
      phantomConstArgs, ownerClass)(func)
  private def designFromDefImpl[V <: DFValAny](
      domain: ir.DomainType,
      args: List[(DFValAny, ir.Meta)],
      constArgs: List[(String, DFValAny, ir.Meta)],
      dclMeta: ir.Meta,
      scalaArgs: List[Any],
      phantomArgs: List[(DFValAny, ir.Meta)],
      phantomConstArgs: List[(DFValAny, ir.Meta)],
      ownerClass: Class[?]
  )(
      func: => V
  )(using DFC): V = trydf:
    // A phantom actual is evaluated at the CALL SITE, and it names a value of the design that
    // DECLARED the def — which, for a def called from another def's body, is not the design we
    // are in: the calling def's own design sits between the two, and a value cannot be
    // referenced across it. The plugin propagates such a capture to the calling def as well
    // (see `CapturePhase.discoverCaptures`), so the value does have a stand-in here: that def's
    // own phantom member for it, which is what this call binds to. Read before this def's
    // design context opens, while the calling design's context is still the current one.
    val callerPhantoms = dfc.mutableDB.DesignContext.getDefPhantoms
    def localize(captured: DFValAny): DFValAny =
      callerPhantoms.getOrElse(captured.asIR, captured)
    // A subprogram def (an ED method or a static function) prints as an HDL subprogram and
    // keeps NO design instance: its application is a first-class call expression (`Func`
    // with `Op.Def`) whose args are the actuals, and ALL its DFHDL inputs (const args and
    // const captures included) are input ports, the subprogram's formals. DF/RT design
    // defs keep the design-instance model (their terminal form is a real design instance)
    // with const args as design parameters (the generated module's generics).
    val isSubprogram = domain match
      case ir.DomainType.ED | ir.DomainType.Static => true
      case _                                       => false
    val designBlock =
      Design.Block.apply(
        domain = domain,
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
    // The const-argument formals are created by this harness rather than by the body,
    // keeping the design's public interface harness-owned (the body fetches them via
    // `designFromDefGetParam`). Phantoms (captured constants) are appended after the
    // explicit ones and tagged. For a SUBPROGRAM def they are input ports exactly like the
    // value args above (an HDL subprogram formal is inherently a call-time value, and the
    // call's `Func` args bind them positionally); for a DF/RT design def they are design
    // parameters (the generated module's generics).
    val params =
      if (isSubprogram)
        constArgs.map { (_, arg, argMeta) =>
          DFVal.Dcl(arg.dfType, Modifier.IN)(using dfc.setMeta(argMeta))
        } ++ phantomConstArgs.map { (arg, fallbackMeta) =>
          DFVal.Dcl(arg.dfType, Modifier.IN)(using
            dfc.setMeta(phantomMeta(arg, fallbackMeta)).tag(ir.PhantomTag)
          )
        }
      else
        constArgs.map { (_, arg, argMeta) =>
          genContainerParam[DFValAny](arg, None, argMeta)
        } ++ phantomConstArgs.map { (arg, fallbackMeta) =>
          genContainerParam[DFValAny](localize(arg), None, phantomMeta(arg, fallbackMeta))(using
            dfc.tag(ir.PhantomTag)
          )
        }
    // all the design's (name, applied value) parameter entries, explicit and phantom
    val namedConstArgs = constArgs.map((name, arg, _) => (name, arg)) ++
      phantomConstArgs.map((arg, fallbackMeta) =>
        (phantomMeta(arg, fallbackMeta).name, localize(arg))
      )
    // this design's phantoms, by the captured value each materializes: a nested call in the
    // body that captures the same value binds to the phantom rather than to the value itself
    // (`localize`)
    dfc.mutableDB.DesignContext.addDefPhantoms(
      phantomArgs.view.map(_._1.asIR).zip(inputs.drop(args.length)) ++
        phantomConstArgs.view.map(_._1.asIR).zip(params.drop(constArgs.length))
    )
    // Params named data-impure by the design def's `pure` annotation (synthesized by the
    // PureCheck plugin phase or declared by the user) contribute their applied type+data
    // to the design load key. Phantom parameters fit the same scheme: PureCheck records
    // their predicted names on the annotation like any explicit parameter's. Unknown
    // applied data (no snapshot, e.g. unattainable during this elaboration) yields None,
    // which makes this call unloadable (runs live; structural dedup still unifies
    // identical bodies).
    val impureParamsKeyOpt: Option[List[(ir.DFType, Any)]] =
      val impureParamNames = dclMeta.annotations.collectFirst {
        case ir.annotation.Pure(true, names) if names.nonEmpty => names.toSet
      }.getOrElse(Set.empty)
      if (impureParamNames.isEmpty) Some(Nil)
      else
        // `"*"` (e.g. from a user-written `@pure(true, "*")`) marks ALL params data-impure
        val allImpure = impureParamNames.contains("*")
        val keyPartOpts = params.zip(namedConstArgs).collect {
          case (param, (name, arg)) if allImpure || impureParamNames.contains(name) =>
            param.asIR match
              case dp: ir.DFVal.DesignParam => dp.appliedData.map(data => (dp.dfType, data))
              // a subprogram formal is a port and carries no applied snapshot; the applied
              // data comes from the actual the harness holds at the call site
              case _ =>
                import dfc.getSet
                arg.asIR.getConstDataThroughParams[ir.Data].map(data => (arg.dfType.asIR, data))
        }
        if (keyPartOpts.forall(_.isDefined)) Some(keyPartOpts.map(_.get)) else None
      end if
    end impureParamsKeyOpt
    // the body fetches inputs and parameters through `designFromDefGetInput/Param`
    val ctx = dfc.mutableDB.DesignContext.current
    ctx.defInputs = inputs
    ctx.defParams = params
    val gate = dfc.mutableDB.DesignLoadGate
    val cacheEnable: Boolean = dfc.elaborationOptions.cacheEnable
    val keyOpt = DesignLoadKey.designDefKeyWith(inputs, scalaArgs, impureParamsKeyOpt)
    // on a design-load hit (intra-run or the sub-design cache service) the body is
    // skipped: the shell context holds only the harness-created public interface, and the
    // loaded design's DB provides the return DFType for the fresh output port
    val skipRetDFType =
      keyOpt
        .flatMap(gate.lookup(_, ownerClass, cacheEnable)(using dfc.refGen))
        .map(_.subDesignRetDFType)
    def exitAndConnectInputs(paramEntries: List[(String, ir.DFVal)]): ir.DFDesignBlock =
      val endedDesign = designBlock.asIR
      dfc.exitOwner()
      Design.Inst(endedDesign, paramEntries)
      val allArgs = args.map(_._1) ++ phantomArgs.map((arg, _) => localize(arg))
      val phantomFlags = List.fill(args.length)(false) ++ List.fill(phantomArgs.length)(true)
      inputs.lazyZip(allArgs).lazyZip(phantomFlags).foreach { (input, arg, isPhantom) =>
        // a phantom input's call-site wiring is tagged through its port selection, which
        // the design-def view form uses to hide the connection
        val connDFC = if (isPhantom) dfc.anonymize.tag(ir.PhantomTag) else dfc.anonymize
        input.connect(arg)(using connDFC)
      }
      endedDesign
    end exitAndConnectInputs
    // The subprogram application: exit the def design and create the call expression in the
    // parent context, a `Func` with `Op.Def` carrying the design's hierarchy key and the
    // actuals as args in the FORMAL MEMBER ORDER (value args, phantom value captures, const
    // args, phantom const captures). The key points at the CANONICAL design from the start:
    // on a gate hit this shell design duplicates a recorded canonical (and the shell,
    // referenced by nothing, drops out of the final assembly); otherwise the ended design
    // is its own canonical. No design instance, no port selections, no nets.
    def exitAndMakeCall(retDFTypeIR: ir.DFType): V =
      val designKey = dfc.mutableDB.DesignContext.current.duplicateOf
        .getOrElse(ir.StaticRef(designBlock.asIR.ownerRef))
      dfc.exitOwner()
      val actuals =
        args.map(_._1) ++ phantomArgs.map((arg, _) => localize(arg)) ++
          constArgs.map(_._2) ++ phantomConstArgs.map((arg, _) => localize(arg))
      DFVal.Func[DFTypeAny, Any](
        retDFTypeIR.asFE[DFTypeAny],
        ir.DFVal.Func.Op.Def(designKey),
        actuals.map(_.asIR)
      ).asInstanceOf[V]
    end exitAndMakeCall
    def genOutPort(retDFTypeIR: ir.DFType) =
      DFVal.Dcl(retDFTypeIR.asFE[DFTypeAny], Modifier.OUT)(using dfc.setName("o"))
    skipRetDFType match
      // the body was skipped: for a subprogram the call itself is the returned value; for a
      // design inst a fresh out port is the returned value (the connection to the body's
      // return value lives in the canonical body)
      case Some(retDFTypeIR) =>
        if (isSubprogram) exitAndMakeCall(retDFTypeIR)
        else
          val paramEntries = Design.Inst.collectParamEntries(clsAppliedArgs(namedConstArgs))
          if (retDFTypeIR == ir.DFUnit)
            exitAndConnectInputs(paramEntries)
            DFUnitVal().asInstanceOf[V]
          else
            val output = genOutPort(retDFTypeIR)
            exitAndConnectInputs(paramEntries)
            output.asInstanceOf[V]
      case None =>
        val preFuncMembers = dfc.mutableDB.DesignContext.getMembersNum
        val ret = func
        // design parameters auto-created by the body itself (see `completed`) make this call
        // unloadable: a skipped body would create none. Detected before exiting the design
        // context.
        val bodyCreatedParams = dfc.mutableDB.DesignContext
          .getMembers(preFuncMembers, dfc.mutableDB.DesignContext.getMembersNum)
          .exists {
            case _: ir.DFVal.DesignParam => true
            case _                       => false
          }
        val retDFTypeIR = ret.dfType.asIR
        if (isSubprogram)
          // the return statement: the body's result connects to the single out port
          if (retDFTypeIR != ir.DFUnit)
            val retMeta = ret.asIR.meta
            val retIdent = DFVal.Alias.AsIs.ident(ret)(using dfc.setMeta(retMeta).anonymize)
            val output = genOutPort(retDFTypeIR)
            output.connect(retIdent)(using dfc.setMeta(retMeta.anonymize))
          val endedDesign = designBlock.asIR
          val retVal = exitAndMakeCall(retDFTypeIR)
          if (!bodyCreatedParams)
            keyOpt.foreach(gate.completed(_, endedDesign, ownerClass, cacheEnable))
          retVal
        else
          val paramEntries = Design.Inst.collectParamEntries(clsAppliedArgs(namedConstArgs))
          val (endedDesign, retVal) =
            if (retDFTypeIR == ir.DFUnit)
              (exitAndConnectInputs(paramEntries), DFUnitVal().asInstanceOf[V])
            else
              val retMeta = ret.asIR.meta
              val retIdent = DFVal.Alias.AsIs.ident(ret)(using dfc.setMeta(retMeta).anonymize)
              val output = genOutPort(retDFTypeIR)
              output.connect(retIdent)(using dfc.setMeta(retMeta.anonymize))
              (exitAndConnectInputs(paramEntries), output.asInstanceOf[V])
          if (!bodyCreatedParams)
            keyOpt.foreach(gate.completed(_, endedDesign, ownerClass, cacheEnable))
          retVal
        end if
    end match
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
