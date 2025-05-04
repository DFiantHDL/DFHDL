package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import dfhdl.compiler.analysis.*
import ir.DFDesignBlock.InstMode

import scala.annotation.{Annotation, implicitNotFound}
import scala.collection.immutable.ListMap
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
    val designBlock = owner.asIR
    // the default RT Domain configuration is set as a global tag
    getSet.setGlobalTag(ir.DefaultRTDomainCfgTag(dfc.elaborationOptions.defaultRTDomainCfg))
    setOwner(
      getSet.replace(designBlock)(
        designBlock.copy(
          dclMeta = ir.Meta.gen(Some(name), position, docOpt, annotations),
          instMode = mkInstMode
        )
      ).asFE
    )
  end setClsNamePos
  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    dfc.exitOwner()
    dfc.enterLate()
  private[dfhdl] def skipChecks: Boolean = false

  def customTopChecks(): Unit = {}
  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    if (hasStartedLate)
      dfc.exitLate()
    else
      dfc.exitOwner()
    import dfc.getSet
    // At the end of the top-level instance we check for errors
    if (owner.asIR.isTop && thisOwner.isEmpty)
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
      dsn.setOwner(
        dsn.owner.asIR
          .setTags(_.tag(customTag))
          .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
          .asFE
      )
    infix def setName(name: String)(using dfc: DFC): D =
      import dfc.getSet
      dsn.setOwner(
        dsn.owner.asIR
          .setMeta(m =>
            if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
            else m.setName(name)
          ).asFE
      )
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

abstract class EDBlackBox(verilogSrc: EDBlackBox.Source, vhdlSrc: EDBlackBox.Source)
    extends EDDesign:
  override private[core] def mkInstMode: InstMode =
    InstMode.BlackBox(verilogSrc, vhdlSrc)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
