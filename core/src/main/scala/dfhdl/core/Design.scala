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

trait Design extends Container, HasClsMetaArgs:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode: InstMode = InstMode.Normal
  private[dfhdl] def initOwner: TOwner =
    Design.Block(__domainType, ir.Meta(Some("???"), Position.unknown, None, Nil), InstMode.Normal)(
      using dfc
    )
  final protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit =
    import dfc.getSet
    val designBlock = containedOwner.asIR
    // the first class extending EDBlackBox.xyzIP will set the actual typeName of the IP
    val instMode = mkInstMode match
      case InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, _)) =>
        designBlock.instMode match
          // preserve the current typeName if it is already set
          case InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, typeName))
              if typeName.nonEmpty =>
            designBlock.instMode
          case _ =>
            // set the IP typeName to the name of the class
            InstMode.BlackBox(InstMode.BlackBox.Source.VendorIP(vendor, name))
      case instMode => instMode
    // the default RT Domain configuration is set as a global tag
    getSet.setGlobalTag(ir.DefaultRTDomainCfgTag(dfc.elaborationOptions.defaultRTDomainCfg))
    // the DFHDL version is set as a global tag
    getSet.setGlobalTag(ir.DFHDLVersionTag(dfhdl.dfhdlVersion))
    getSet.replace(designBlock)(
      designBlock.copy(
        dclMeta = r__For_Plugin.metaGen(Some(name), position, docOpt, annotations),
        instMode = instMode
      )
    )
  end setClsNamePos
  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    import dfc.getSet
    Design.Block.updateWithParams(containedOwner.asIR)
    if (dfc.owner.asIR.getThisOrOwnerDesign.isDeviceTop)
      handleResourceConstraints()
      dfc.mutableDB.ResourceOwnershipContext.emptyTopResourceOwners()
    val endedDesign = containedOwner.asIR
    dfc.exitOwner()
    Design.Inst(endedDesign)
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
          designDB.check() // various checks post initial elaboration
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
    def apply(domain: ir.DomainType, dclMeta: ir.Meta, instMode: InstMode)(using DFC): Block =
      val paramMap = ListMap.from(
        dfc.mutableDB.DesignContext.getDesignParamValueMap.view.mapValues(
          _.asIR.refTW[ir.DFDesignBlock]
        )
      )
      ir.DFDesignBlock(
        domain, dclMeta, instMode, paramMap, dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags
      ).addMember.asFE
    end apply
    protected[core] def updateWithParams(designBlock: ir.DFDesignBlock)(using dfc: DFC): Unit =
      import dfc.getSet
      val paramMap =
        ListMap.from(
          dfc.mutableDB.DesignContext.current.getImmutableMemberList.view.collect {
            case dp: ir.DFVal.DesignParam =>
              val dfVal = dp.appliedValOpt.get
              // invalidating the param cache value after design elaboration
              dp.clearCachedAppliedVal()
              dp.getName -> dfVal.refTW[ir.DFDesignBlock](knownReachable = true)
          }.toMap
        )
      getSet.replace(designBlock)(designBlock.copy(paramMap = paramMap))
  end Block
  object Inst:
    // Construct a DFDesignInst member in the parent context that points back
    // at `designBlock`. Called from `onCreateStartLate` after
    // `dfc.exitOwner()` so `dfc.ownerOrEmptyRef` resolves to the enclosing
    // owner. The top-level design has no instantiation site (no instance
    // name, no applied parameters — only defaults), so we skip it. paramMap
    // is left empty in phase 1 of the DFDesignInst split refactor.
    protected[core] def apply(designBlock: ir.DFDesignBlock)(using dfc: DFC): Unit =
      import dfc.getSet
      if (!designBlock.isTop)
        val designRef: ir.DFRef.OneWay[ir.DFDesignBlock] = designBlock.ref
        // Phase 1 parks the DFDesignInst as an anonymous inert member so it
        // does not collide with the DFDesignBlock's instance name in
        // UniqueNames / printers. Phase 2 will migrate the instance name off
        // DFDesignBlock.meta onto DFDesignInst.meta as consumers flip over.
        val inst = ir.DFDesignInst(
          designRef = designRef,
          paramMap  = ListMap.empty,
          ownerRef  = dfc.ownerOrEmptyRef,
          meta      = designBlock.meta.anonymize,
          tags      = designBlock.tags
        )
        dfc.mutableDB.addMember(inst)
  end Inst
  extension [D <: Design](dsn: D)
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
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
      import designDB.getSet
      val danglingVars = designDB.getImplicitStateVarsRT.view
        .map { v =>
          s"""|DFiant HDL connectivity/assignment error!
              |Position:  ${v.meta.position}
              |Hierarchy: ${v.getOwnerDomain.getFullName}
              |Message:   Found a latch variable `${v.getName}`. Latches are not allowed under RT domains.""".stripMargin
        }
      if (danglingVars.nonEmpty)
        throw new IllegalArgumentException(danglingVars.mkString("\n"))
  end extension
end Design

abstract class DFDesign extends DomainContainer(DomainType.DF), Design

abstract class RTDesign(cfg: RTDomainCfg = RTDomainCfg.Derived)
    extends RTDomainContainer(cfg),
      Design:
  related =>
  abstract class RelatedDomain extends RTDomain(RTDomainCfg.Related(related))

abstract class EDDesign extends DomainContainer(DomainType.ED), Design

abstract class EDBlackBox(source: EDBlackBox.Source) extends EDDesign:
  override private[core] def mkInstMode: InstMode = InstMode.BlackBox(source)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
  import ir.constraints.DeviceID.Vendor
  abstract class QsysIP
      extends EDBlackBox(Source.VendorIP(Vendor.AlteraIntel(pro = true), typeName = "")):
    val version: String <> CONST
  abstract class VivadoIP extends EDBlackBox(Source.VendorIP(Vendor.XilinxAMD, typeName = "")):
    val version: String <> CONST
