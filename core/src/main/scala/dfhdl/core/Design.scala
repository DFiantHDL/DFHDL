package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import dfhdl.compiler.analysis.*
import ir.DFDesignBlock.InstMode

import scala.annotation.{Annotation, implicitNotFound}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.reflect.ClassTag

trait Design extends Container, HasClsMeta, HasClsArgs:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode: InstMode = InstMode.Normal
  private[dfhdl] def initOwner: TOwner =
    import dfc.getSet
    getSet.setGlobalTag(dfc.elaborationOptions.defaultRTDomainCfgTag)
    getSet.setGlobalTag(ir.DFHDLVersionTag(dfhdl.dfhdlVersion))
    // Build the design block directly from the `__clsMeta` chain (the
    // plugin-injected, per-class metadata, most-derived first). The leaf names
    // the design (meta); for a blackbox IP, the base-most concrete class
    // extending the IP marker names the IP type (`typeName`).
    val chain = __clsMeta
    val instMode = mkInstMode match
      case InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, _)) if chain.nonEmpty =>
        InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, chain.last.name))
      // a foreign IP's identity is its base-most concrete class (the one defining the bundled
      // resources/wrapper), not the most-derived design name — so default the resource root from the
      // chain (like VendorIP's `typeName`); its leaf is the IP name (the design block's `dclName`).
      case InstMode.BlackBox(f: InstMode.BlackBox.Source.ForeignIP)
          if chain.nonEmpty && f.resourcePath.isEmpty =>
        InstMode.BlackBox(f.copy(resourcePath = s"dfhdl-ips/${chain.last.name}"))
      case other => other
    val blockDFC = chain.headOption match
      case Some(meta) => dfc.setMeta(meta)
      case None       => dfc.anonymize
    Design.Block(__domainType, instMode)(using blockDFC)
  end initOwner
  // ~~~ the class-design body-skip gate (compiler-plugin rigging) ~~~
  // Called at the head of a design class's body, after the design parameters (which the key
  // needs) and before any body statement, from the guard the plugin injects into every
  // skippable design class. It decides through the design load gate whether this
  // instantiation reuses an already-loaded elaboration of the same key (this run's
  // canonical, or a sub-design cache entry adopted here), in which case the plugin-guarded
  // body statements do not run and `__clsSkipBody` reports the decision to each of them.
  // What still runs on a skip is the class's public interface (its port, constant and
  // interface declarations, which the plugin leaves unguarded), so the instantiation site
  // binds the same ports and applied parameters it would have bound live; the shell design
  // itself is a duplicate of the loaded design and drops out of the final assembly.
  final protected def __clsBodyGate(
      bodyClass: Class[?],
      params: List[r__For_Plugin.ClsParam],
      skippable: Boolean,
      hasBody: Boolean
  ): Unit =
    import dfc.getSet
    val ctx = dfc.mutableDB.DesignContext.current
    // The design's parameters, created HERE and not by the body: the plugin lifts the class's
    // applied `<> CONST` parameters out of the body and into this call, and the body's parameter
    // declarations fetch them back (`__clsGetParam`). The design's public interface is therefore
    // complete before the gate decides, on a skip as on a live run.
    params.zipWithIndex.foreach { (param, idx) =>
      ctx.clsParams((bodyClass, idx)) =
        r__For_Plugin.genContainerParam[DFValAny](param.applied, param.default, param.meta)
    }
    val design = dfc.mutableDB.OwnershipContext.currentDesign
    // The gate only ever loads a design in place of its own (leaf) declaring class's body. A base
    // design class's body runs as part of every instantiation of a class extending it, where the
    // gate must stand down: the leaf's constructor arguments and captures (which the key needs)
    // are not yet initialized during a base template's run, and the leaf's own body has not run
    // yet, so nothing keyed there would describe the design.
    val isDclBody = Design.dclClassOf(this) eq bodyClass
    // ...and once a base class's body HAS run live into this design, no later gate in the chain
    // may skip either: the design would end up holding half a body.
    if (skippable && isDclBody && !design.isTop && !ctx.clsBodyRanLive)
      val keyOpt = DesignLoadKey.designClsKeyWith(__clsScalaArgs)
      ctx.clsLoadKey = keyOpt
      ctx.clsDclClass = Some(bodyClass)
      ctx.clsGateParamNum = ctx.designParamNum
      ctx.clsSkipBody = keyOpt.exists(key =>
        dfc.mutableDB.DesignLoadGate
          .lookup(key, bodyClass, dfc.elaborationOptions.cacheEnable)(using dfc.refGen)
          .isDefined
      )
    if (hasBody && !ctx.clsSkipBody) ctx.clsBodyRanLive = true
  end __clsBodyGate
  // the gate's decision, read by every plugin-guarded body statement
  final protected def __clsSkipBody: Boolean =
    dfc.mutableDB.DesignContext.current.clsSkipBody
  // a harness-created design parameter, fetched by the body declaration the plugin rewired
  final protected def __clsGetParam[V <: DFValAny](bodyClass: Class[?], idx: Int): V =
    dfc.mutableDB.DesignContext.current.clsParams((bodyClass, idx)).asInstanceOf[V]

  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    import dfc.getSet
    val paramEntries = Design.Inst.collectParamEntries(__clsAppliedArgs)
    if (dfc.owner.asIR.getThisOrOwnerDesign.isDeviceTop)
      handleResourceConstraints()
      dfc.mutableDB.ResourceOwnershipContext.emptyTopResourceOwners()
    val endedDesign = containedOwner.asIR
    // Route this class design through the design load gate: designs unify ONLY through
    // the gate's key (the structural dedup is retired). The key is the one the body-skip
    // gate already computed, or, for a class the plugin could not guard, one computed here
    // at the design's end (its body ran live). Either way it is taken BEFORE exiting the
    // owner so the design context can be marked as its canonical's duplicate; on a miss the
    // ended design is recorded as the key's canonical after the exit.
    val gate = dfc.mutableDB.DesignLoadGate
    val ctx = dfc.mutableDB.DesignContext.current
    // Design parameters the body created on its own (an auto-created capture parameter,
    // `cloneUnreachable`) can join the key's impure-parameter data, which the gate cannot
    // see before the body runs: such a design re-keys here and is never stored, so no later
    // run's gate can skip a body that has parameters to create.
    val bodyParams = ctx.clsLoadKey.nonEmpty && ctx.designParamNum != ctx.clsGateParamNum
    val keyOpt =
      if (endedDesign.isTop) None
      else if (ctx.clsLoadKey.nonEmpty && !bodyParams) ctx.clsLoadKey
      else DesignLoadKey.designClsKeyWith(__clsScalaArgs)
    val joinedCanonical = keyOpt.exists(gate.joinCanonicalOf)
    dfc.exitOwner()
    Design.Inst(endedDesign, paramEntries)
    if (!joinedCanonical)
      keyOpt.foreach(
        gate.completed(
          _,
          endedDesign,
          ctx.clsDclClass.getOrElse(Design.dclClassOf(this)),
          cacheEnable = dfc.elaborationOptions.cacheEnable && !bodyParams
        )
      )
    dfc.enterLate()
  end onCreateStartLate
  private[dfhdl] def skipChecks: Boolean = false

  def customTopChecks(): Unit = {}
  private def handleResourceConstraints(): Unit =
    import dfhdl.{OUT, NOTHING}
    import ir.constraints.{IO, SigConstraint}
    import dfhdl.platforms.resources.*
    import dfhdl.platforms.devices.Pin
    import dfc.getSet
    def addUnusedPinPort(pinID: String, constraints: List[SigConstraint]): Unit =
      val missingPullDownSupport = constraints.collectFirst {
        case IO(missingPullDownSupport = missingPullDownSupport: Boolean) =>
          missingPullDownSupport
      }.getOrElse(false)
      val unusedPullMode = constraints.collectFirst {
        case IO(unusedPullMode = unusedPullMode: IO.PullMode) => unusedPullMode
      }.get
      // missing pull down support and unused pull mode is down, so we drive the pin to zero
      val driveZero = missingPullDownSupport && unusedPullMode == IO.PullMode.DOWN
      val updatedConstraints =
        if (driveZero) constraints
        // setting the pull mode as the unused pull mode
        else (IO(pullMode = unusedPullMode) :: constraints).merge
      val updatedAnnotations = ir.annotation.Unused.Keep :: updatedConstraints
      val port =
        DFVal.Dcl(DFBit, OUT)(using
          dfc.setName(s"Pin_${pinID}_unused").setAnnotations(updatedAnnotations)
        )
      if (driveZero) port.connect(DFVal.Const(DFBit, Some(false), named = false))
      else port.connect(NOTHING(DFBit)(using dfc.anonymize))
    end addUnusedPinPort
    val usedPinIDs: Set[String] =
      dfc.mutableDB.ResourceOwnershipContext
        .getConnectedDclResourceMap.values.flatten
        .flatMap(_._2.allSigConstraints)
        .collect { case IO(loc = pinID: String) => pinID }.toSet
    val clkResources = mutable.Set.empty[ClkResource]
    def addUnusedPinPorts(resourceOwner: ResourceOwner): Unit =
      resourceOwner.getChildren.foreach(addUnusedPinPorts)
      resourceOwner.getResources.foreach {
        case pin: Pin if (!usedPinIDs.contains(pin.id)) =>
          val unusedPullMode = pin.allSigConstraints.collectFirst {
            case IO(unusedPullMode = unusedPullMode: IO.PullMode) => unusedPullMode
          }
          unusedPullMode.foreach(unusedPullMode =>
            addUnusedPinPort(pin.id, pin.allSigConstraints)
          )
        case clkResource: ClkResource => clkResources += clkResource
        case _                        =>
      }
    dfc.mutableDB.ResourceOwnershipContext.getTopResourceOwners.foreach(addUnusedPinPorts)
    val clkPorts = mutable.ListBuffer.empty[ir.DFVal.Dcl]
    dfc.mutableDB.DesignContext.current.getImmutableMemberList.foreach {
      case port: ir.DFVal.Dcl if port.isPortIn && port.isClkDcl =>
        clkPorts += port
      case _ =>
    }
  end handleResourceConstraints

  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    if (hasStartedLate)
      dfc.exitLate()
    else
      dfc.exitOwner()
    import dfc.getSet
    // At the end of the top-level instance we check for warnings and errors
    if (containedOwner.asIR.isTop && thisOwner.isEmpty)
      val warnings = dfc.getWarnings
      if (warnings.nonEmpty)
        System.err.println(
          warnings.map(_.toString).mkString("\n\n")
        )
        if (dfc.elaborationOptions.Werror.toBoolean)
          dfc.logEvent(
            DFError.Basic(
              "Werror",
              new IllegalArgumentException(
                "Warnings found with -Werror enabled. Fix the warnings or disable the Werror flag."
              )
            )
          )
      val errors = dfc.getErrors
      // If we have errors, then we print them to stderr and exit
      if (errors.nonEmpty)
        exitWithError(
          errors.collect { case basicErr: DFError.Basic => basicErr.toString }.mkString("\n\n")
        )
      if (!skipChecks)
        try
          import Design.latchesCheck
          val designDB = dfc.mutableDB.immutable
          designDB.check // various checks post initial elaboration
          designDB.latchesCheck()
          customTopChecks() // custom user/library checks
        catch
          case err: (IllegalArgumentException | AssertionError) =>
            exitWithError(err.getMessage)
          case others => throw others
    end if
  end onCreateEnd
end Design

object Design:
  import ir.DFDesignBlock.InstMode
  // The class DECLARING a design: the plugin wraps a regular instantiation in an anonymous
  // subclass (carrying only the `__dfc` override), and a user-written anonymous subclass
  // (`new MyDesign { ... }`) contributes no design body either (its statements run late,
  // outside the design), so an anonymous leaf is never the declaring class. This is the
  // design's code identity for the sub-design cache (`SubDesignRef`), the counterpart of a
  // design def's nearest enclosing class.
  private[core] def dclClassOf(dsn: Design): Class[?] =
    var cls: Class[?] = dsn.getClass
    while (cls.getSuperclass != null && (cls.isAnonymousClass || cls.getSimpleName.isEmpty))
      cls = cls.getSuperclass
    cls
  type Block = DFOwner[ir.DFDesignBlock]
  object Block:
    def apply(domain: ir.DomainType, instMode: InstMode)(using DFC): Block =
      ir.DFDesignBlock(
        domain, instMode, dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags
      ).addMember.asFE
    end apply
  end Block
  object Inst:
    // Collect (name, appliedVal) entries while still inside the child design context.
    // Must be called BEFORE `dfc.exitOwner()` because it relies on the child context's
    // member list. The design's `DesignParam` members are scanned in member order; each
    // parameter's applied value comes from the compiler-plugin-provided `clsAppliedArgs`
    // (`__clsAppliedArgs` for design/interface classes, the explicit const args for design
    // defs), matched by the parameter's name. Parameters NOT in the plugin list — base-class
    // constructor parameters and AUTO-CREATED parameters (an unreachable named value captured
    // from an enclosing design — see `cloneUnreachable`) — recover their applied value from
    // the key of their creation entry in the context's `unreachableNamedValues` memoization.
    // That map also memoizes resolutions of child-design values, so a creation entry is
    // identified by its key originating OUTSIDE this design (globals trivially so).
    protected[core] def collectParamEntries(
        clsAppliedArgs: List[(String, ir.DFVal)]
    )(using dfc: DFC): List[(String, ir.DFVal)] =
      import dfc.getSet
      val appliedMap = clsAppliedArgs.toMap
      val ctx = dfc.mutableDB.DesignContext.current
      val endingDesign = dfc.owner.asIR
      def autoAppliedValOf(dp: ir.DFVal.DesignParam): Option[ir.DFVal] =
        ctx.unreachableNamedValues.collectFirst {
          case (appliedVal, `dp`)
              if appliedVal.isGlobal || !appliedVal.isInsideOwner(endingDesign) =>
            appliedVal
        }
      ctx.getImmutableMemberList.view.flatMap {
        case dp: ir.DFVal.DesignParam =>
          val name = dp.getName
          appliedMap.get(name).orElse(autoAppliedValOf(dp)).map(name -> _)
        case _ => None
      }.toList
    end collectParamEntries
    // Construct a DFDesignInst member in the parent context that points back
    // at `designBlock`. Called from `onCreateStartLate` after
    // `dfc.exitOwner()` so `dfc.ownerOrEmptyRef` resolves to the enclosing
    // owner. The top-level design has no instantiation site (no instance
    // name, no applied parameters — only defaults), so we skip it.
    // The paramMap's TwoWay refs are built here so they are registered in the
    // current (parent) context — important for duplicate designs whose child
    // refTable is only partially transferred up (public members only). For
    // top designs we skip building entirely because there is no DFDesignInst
    // to register as the refs' origin member, which would orphan the refs.
    protected[core] def apply(
        designBlock: ir.DFDesignBlock,
        paramEntries: List[(String, ir.DFVal)]
    )(using dfc: DFC): Unit =
      import dfc.getSet
      if (!designBlock.isTop)
        val paramMap = ListMap.from(paramEntries.view.map { (name, dfVal) =>
          name -> dfVal.refTW[ir.DFDesignInst](knownReachable = true)
        })
        val inst = ir.DFDesignInst(
          designRef = designBlock.ref,
          paramMap = paramMap,
          ownerRef = dfc.owner.ref,
          meta = dfc.getMeta,
          tags = dfc.tags
        )
        // Resolve the ref to reach the current DB version of the block so the
        // cache lives on the block that `getDesignInst` looks up later (the
        // captured `designBlock` IR could otherwise be an older revision).
        inst.designRef.asRef.get.setDesignInstCache(inst)
        dfc.mutableDB.addMember(inst)
      end if
    end apply
  end Inst
  extension [D <: Design](dsn: D)
    // The compiled design DB is hierarchical BY CONSTRUCTION (root + per-design
    // sub-DBs, see `MutableDB.hierarchical`): the stage pipeline runs natively on
    // this form.
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
    // A meta-design's immutable DB: a FLAT container of the freshly-created members
    // to inject through the patch system, with no design hierarchy (this is the
    // meta-programming form of `immutable`; a root DB would have empty `members`).
    def getDBOld: ir.DB = dsn.dfc.mutableDB.immutable
    infix def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): D =
      import dfc.getSet
      dsn.containedOwner.asIR
        .setTags(_.tag(customTag))
        .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
      dsn
    infix def setName(name: String)(using dfc: DFC): D =
      import dfc.getSet
      dsn.containedOwner.asIR
        .setMeta(m =>
          if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
          else m.setName(name)
        )
      dsn
  end extension

  extension (designDB: ir.DB)
    def latchesCheck(): Unit =
      // the root has empty members and a non-functional getSet, so only iterate
      // the sub-DBs (which already cover every design)
      val allDBs = designDB.subDBs.values.toList
      val danglingVars = allDBs.view.flatMap { db =>
        given ir.MemberGetSet = db.getSet
        db.getImplicitStateVarsRT.view
          .map { v =>
            s"""|DFiant HDL connectivity/assignment error!
                |Position:  ${v.meta.position}
                |Hierarchy: ${v.getOwnerDomain.getFullName}
                |Message:   Found a latch variable `${v.getName}`. Latches are not allowed under RT domains.""".stripMargin
          }
      }
      if (danglingVars.nonEmpty)
        throw new IllegalArgumentException(danglingVars.mkString("\n"))
  end extension
end Design

abstract class DFDesign extends DomainContainer(DomainType.DF), Design

abstract class RTDesign extends RTDomainContainer, Design

abstract class EDDesign extends DomainContainer(DomainType.ED), Design

abstract class EDBlackBox extends EDDesign:
  // `source` is a `def` (not a constructor-param field) so `mkInstMode` is safe
  // to evaluate during construction — before this class's fields are assigned —
  // which lets the design block be built with its final `instMode` at creation.
  protected def source: EDBlackBox.Source
  override private[core] def mkInstMode: InstMode = InstMode.BlackBox(source)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
  import ir.constraints.DeviceID.Vendor
  abstract class QsysIP extends EDBlackBox:
    override protected def source: Source =
      Source.VendorIP(Vendor.AlteraIntel(pro = true), typeName = "")
    val version: String <> CONST
  abstract class VivadoIP extends EDBlackBox:
    override protected def source: Source = Source.VendorIP(Vendor.XilinxAMD, typeName = "")
    val version: String <> CONST

  /** A foreign IP blackbox: its HDL wrapper and per-system simulator-integration binaries (DPI /
    * VPI / VHPI shims) ship as classpath resources bundled with the IP library. DFHDL mirrors all
    * resources under [[resourcePath]] into `dfhdl-ips/<ipName>` in the project when committing, and
    * at simulate time loads the right shim for the chosen tool/system and invokes the optional
    * [[simHookClass]] around the run (e.g. to launch a viewer). The IP name is the design's class
    * name; subclasses just set the relevant FFI base names ([[resourcePath]] defaults to
    * `dfhdl-ips/<ipName>`).
    */
  abstract class ForeignIP extends EDBlackBox:
    // FQN of this IP class, used when re-emitting DFHDL that re-instantiates the IP.
    // The runtime class is an elaboration-time anonymous subclass of the IP class (the
    // instantiation is wrapped to attach meta), so `getClass.getName` would yield that synthetic
    // wrapper's name. Walk the superclass chain (bounded to the `ForeignIP` subtypes) and take the
    // base-most *concrete* class — the one that actually defines the IP (the class chain's base
    // entry) — and use its fully-qualified name.
    protected def clsName: String =
      val foreignIPCls = classOf[ForeignIP]
      var cls: Class[?] = getClass
      var ipCls: Class[?] = cls
      while (cls != null && foreignIPCls.isAssignableFrom(cls))
        if (
          !cls.isAnonymousClass && !cls.isSynthetic &&
          !java.lang.reflect.Modifier.isAbstract(cls.getModifiers)
        )
          ipCls = cls
        cls = cls.getSuperclass
      ipCls.getName.replace("$", ".")
    // Where the bundled resources live on the classpath. Defaults (resolved at elaboration) to
    // `dfhdl-ips/<ipName>` — a non-package root (`dfhdl-ips/...`) so the resource directory is never
    // read by the Scala compiler as a package colliding with the IP class/object name. The IP name
    // itself is derived from the class chain (the base-most concrete IP class), not supplied here.
    protected def resourcePath: String = ""
    protected def dpiLib: String = ""
    protected def vpiModule: String = ""
    protected def vhpiLib: String = ""
    protected def simHookClass: String = ""
    // set true when the HDL wrapper uses delay (`#`) controls so Verilator builds with `--timing`
    protected def needsTiming: Boolean = false
    final override protected def source: Source =
      // an empty `resourcePath` is defaulted at elaboration from the class chain to
      // `dfhdl-ips/<ipName>` (see `Design.initOwner`)
      Source.ForeignIP(clsName, resourcePath, dpiLib, vpiModule, vhpiLib, simHookClass, needsTiming)
  end ForeignIP
end EDBlackBox
