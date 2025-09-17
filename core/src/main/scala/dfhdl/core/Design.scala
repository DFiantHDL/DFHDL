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
    // the first class extending EDBlackBox.QsysIP will set the actual typeName of the IP
    val instMode = mkInstMode match
      case InstMode.BlackBox(InstMode.BlackBox.Source.Qsys(_)) =>
        designBlock.instMode match
          // preserve the current typeName if it is already set
          case InstMode.BlackBox(InstMode.BlackBox.Source.Qsys(typeName)) if typeName.nonEmpty =>
            designBlock.instMode
          case _ =>
            // set the IP typeName to the name of the class
            InstMode.BlackBox(InstMode.BlackBox.Source.Qsys(name))
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
    if (dfc.owner.asIR.getThisOrOwnerDesign.isDeviceTop)
      handleResourceConstraints()
      dfc.mutableDB.ResourceOwnershipContext.emptyTopResourceOwners()
    dfc.exitOwner()
    dfc.enterLate()
  private[dfhdl] def skipChecks: Boolean = false

  def customTopChecks(): Unit = {}
  private def handleResourceConstraints(): Unit =
    import dfhdl.{<>, OUT, NOTHING}
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
        DFBit.<>(OUT)(using dfc.setName(s"Pin_${pinID}_unused").setAnnotations(updatedAnnotations))
      if (driveZero) port <> DFVal.Const(DFBit, Some(false), named = false)
      else port <> NOTHING(DFBit)(using dfc.anonymize)
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
    // At the end of the top-level instance we check for errors
    if (containedOwner.asIR.isTop && thisOwner.isEmpty)
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
      ir.DFDesignBlock(
        domain, dclMeta, instMode, dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags
      )
        .addMember
        .asFE
    end apply
  end Block
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
  abstract class QsysIP extends EDBlackBox(Source.Qsys(typeName = "")):
    val version: String <> CONST
