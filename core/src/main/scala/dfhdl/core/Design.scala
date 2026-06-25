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

trait Design extends Container, HasClsMetaArgs, HasConstParams:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode: InstMode = InstMode.Normal
  private[dfhdl] def initOwner: TOwner =
    import dfc.getSet
    getSet.setGlobalTag(dfc.elaborationOptions.defaultRTDomainCfgTag)
    getSet.setGlobalTag(ir.DFHDLVersionTag(dfhdl.dfhdlVersion))
    // Build the design block directly from the `__clsMetaArgs` chain (the
    // plugin-injected, per-class metadata, most-derived first). The leaf names
    // the design (meta); for a blackbox IP, the base-most concrete class
    // extending the IP marker names the IP type (`typeName`).
    val chain = __clsMetaArgs
    val instMode = mkInstMode match
      case InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, _)) if chain.nonEmpty =>
        InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, chain.last.name))
      case other => other
    val blockDFC = chain.headOption match
      case Some(a) =>
        dfc.setMeta(r__For_Plugin.metaGen(Some(a.name), a.position, a.docOpt, a.annotations))
      case None => dfc.anonymize
    Design.Block(__domainType, instMode)(using blockDFC)
  end initOwner
  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    import dfc.getSet
    val paramEntries = Design.Inst.collectParamEntries
    if (dfc.owner.asIR.getThisOrOwnerDesign.isDeviceTop)
      handleResourceConstraints()
      dfc.mutableDB.ResourceOwnershipContext.emptyTopResourceOwners()
    val endedDesign = containedOwner.asIR
    dfc.exitOwner()
    Design.Inst(endedDesign, paramEntries)
    dfc.enterLate()
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
          designDB.oldToNew.check // various checks post initial elaboration
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
    // Must be called BEFORE `dfc.exitOwner()` because it relies on the cached
    // applied value and the child context's member list.
    protected[core] def collectParamEntries(using dfc: DFC): List[(String, ir.DFVal)] =
      import dfc.getSet
      dfc.mutableDB.DesignContext.current.getImmutableMemberList.view.collect {
        case dp: ir.DFVal.DesignParam =>
          val dfVal = dp.appliedValOpt.get
          // invalidating the param cache value after design elaboration
          dp.clearCachedAppliedVal()
          dp.getName -> dfVal
      }.toList
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
    // The compiled design DB is hierarchical (root + per-design sub-DBs): the
    // stage pipeline runs natively on this form. `oldToNew` is applied ONCE here
    // at the source so the staged DB is hierarchical end-to-end (no per-stage
    // round-trips).
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable.oldToNew
    // The raw FLAT immutable DB (the pre-`oldToNew` form). Needed where a design's
    // members are consumed as a flat container without the hierarchy — e.g. a
    // meta-design in the patch system, whose DB is just the freshly-created
    // members to inject (root would have empty `members`).
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
      val newDB = designDB.oldToNew
      // Under B-pure, root has empty members and a non-functional getSet —
      // only iterate the sub-DBs (which already cover every design).
      val allDBs = newDB.subDBs.values.toList
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
