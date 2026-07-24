package dfhdl.core

import dfhdl.compiler.ir
import dfhdl.compiler.printing.DefaultPrinter

/** The design-load gate's key: the identity that decides whether two method or design-class
  * instantiations are the same load. Every IR-bearing part is reduced to a stable default-printer
  * `codeString` (see `dfTypeKey`/`dataKey`), so the key is plain data comparable by value, and the
  * same strings serve the cross-run service `localKey` unchanged. Plain Scala arguments key by
  * their own value equality. The gate (in `MutableDB`) only stores and looks keys up; building them
  * needs the elaboration context, so it lives here and runs off the ambient `DFC` (which IS the
  * context).
  */
final case class DesignLoadKey(
    dclMeta: ir.Meta,
    inputTypes: List[String],
    scalaArgs: List[Any],
    impureParamsKey: List[String]
):
  /** The cross-run content key: a stable digest of the key parts, used by the sub-design cache
    * service. `dclMeta` serializes through its IR writer; the DFType and impure-data parts are
    * already codeStrings; plain Scala args fold through their string forms. Best effort: unstable
    * string forms (e.g. identity toStrings) can only cause MISSES, never a false hit of a different
    * declaration, since the service anchors entries by the owner class's code digest and `dclMeta`
    * is part of this digest.
    */
  def localKey: String =
    import upickle.default.write
    val sb = new StringBuilder
    sb ++= write(dclMeta)
    sb += '|'
    inputTypes.foreach { s => sb ++= s; sb += ',' }
    sb += '|'
    scalaArgs.foreach { a => sb ++= a.toString; sb += ',' }
    sb += '|'
    impureParamsKey.foreach { s => sb ++= s; sb += ',' }
    java.security.MessageDigest.getInstance("SHA-256")
      .digest(sb.toString.getBytes("UTF-8"))
      .map("%02x".format(_)).mkString
end DesignLoadKey

object DesignLoadKey:
  // A DFType keyed by its default-printer `codeString`. `dropUnreachableRefs` first
  // rewires every type reference to a reachable member (one that has an origin), so
  // printing always resolves and never throws; the resulting string is construction- and
  // run-independent.
  private def dfTypeKey(dfType: ir.DFType)(using dfc: DFC): String =
    DefaultPrinter(using dfc.getSet).csDFType(dfType.dropUnreachableRefs)

  // A data-impure parameter's applied data keyed by its default-printer const-data
  // `codeString`. The printer takes the reachable dfType as context together with the
  // data, so the string encodes both; the data is never structurally decomposed
  // (decomposing, e.g., a BitVector's internal rope would compare arrays by reference and
  // wrongly split identical keys).
  private def dataKey(dfType: ir.DFType, data: Any)(using dfc: DFC): String =
    DefaultPrinter(using dfc.getSet).csConstData(dfType.dropUnreachableRefs, data)

  // A plain Scala argument keys by its own value equality, except a DFType (front-end or
  // IR) passed as an argument (e.g. `new IDGen(SInt(w))`), which keys by its codeString so
  // dependent types carrying fresh reference tokens still unify across instantiations.
  private def scalaArgKey(arg: Any)(using DFC): Any = arg match
    case irType: ir.DFType           => dfTypeKey(irType)
    case fe: dfhdl.core.DFType[?, ?] => dfTypeKey(fe.asIR)
    case _                           => arg

  /** The key of the CURRENT method instantiation, or None when the call is uncacheable (the design
    * is impure, or a data-impure parameter's applied data is unknown during this elaboration,
    * signaled by an empty `impureParamsKeyOpt`).
    */
  def methodDesignKeyWith(
      inputs: List[DFValAny],
      scalaArgs: List[Any],
      impureParamsKeyOpt: Option[List[(ir.DFType, Any)]]
  )(using dfc: DFC): Option[DesignLoadKey] =
    val currentDesign = dfc.mutableDB.OwnershipContext.currentDesign
    if (currentDesign.isPure)
      impureParamsKeyOpt.map(impureParamsKey =>
        DesignLoadKey(
          currentDesign.dclMeta,
          inputs.map(i => dfTypeKey(i.dfType.asIR)),
          scalaArgs.map(scalaArgKey),
          impureParamsKey.map((dfType, data) => dataKey(dfType, data))
        )
      )
    else None
  end methodDesignKeyWith

  /** The key of the CURRENT class-design instantiation, computed at the design's END (a class body
    * always runs live for now; the key unifies identical designs), or None when the design is
    * uncacheable. Class designs have no call-site inputs (ports are body-declared); their plain
    * Scala constructor parameters and template captures arrive through the plugin-injected
    * `__clsScalaArgs` chain, and their data-impure parameters (named on the `pure` annotation) key
    * their applied type+data, resolved from the design's own DesignParam members by name.
    */
  def designClsKeyWith(scalaArgs: List[Any])(using dfc: DFC): Option[DesignLoadKey] =
    val currentDesign = dfc.mutableDB.OwnershipContext.currentDesign
    if (currentDesign.isPure)
      val impureParamNames = currentDesign.dclMeta.annotations.collectFirst {
        case ir.annotation.Pure(true, names) if names.nonEmpty => names.toSet
      }.getOrElse(Set.empty)
      // a vendor IP blackbox bakes its applied parameter values into the emitted IP
      // instance (there is no parametric module), so ALL its parameters key their
      // applied data
      val allImpure = impureParamNames.contains("*") || currentDesign.isVendorIPBlackbox
      val impureParamsKeyOpt: Option[List[(ir.DFType, Any)]] =
        if (!allImpure && impureParamNames.isEmpty) Some(Nil)
        else
          val params = dfc.mutableDB.DesignContext.current.getImmutableMemberList.collect {
            case dp: ir.DFVal.DesignParam => dp
          }
          val marked = params.filter(dp => allImpure || impureParamNames.contains(dp.meta.name))
          // strict name resolution: every recorded name must match a parameter member
          // with a known applied-data snapshot. A forced-ONLY capture, for example,
          // creates no parameter member, so its data cannot be keyed and the design is
          // unloadable (runs live, never unifies).
          val allNamesResolved = allImpure ||
            (impureParamNames -- marked.view.map(_.meta.name).toSet).isEmpty
          val keyPartOpts = marked.map(dp => dp.appliedData.map(data => (dp.dfType, data)))
          if (allNamesResolved && keyPartOpts.forall(_.isDefined))
            Some(keyPartOpts.map(_.get))
          else None
      impureParamsKeyOpt.map(impureParamsKey =>
        DesignLoadKey(
          currentDesign.dclMeta,
          Nil,
          scalaArgs.map(scalaArgKey),
          impureParamsKey.map((dfType, data) => dataKey(dfType, data))
        )
      )
    else None
    end if
  end designClsKeyWith
end DesignLoadKey
