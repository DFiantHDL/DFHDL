package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import ir.DFDesignBlock.InstMode

import scala.annotation.{Annotation, implicitNotFound}
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

private[dfhdl] trait Design extends Container, HasClsMetaArgs:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode(args: ListMap[String, Any]): InstMode = InstMode.Normal
  private[dfhdl] def initOwner: TOwner =
    Design.Block(__domainType, ir.Meta(Some("???"), Position.unknown, None, Nil), InstMode.Normal)(
      using dfc
    )
  final protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation],
      args: ListMap[String, Any]
  ): Unit =
    val designBlock = owner.asIR
    setOwner(
      dfc.getSet.replace(designBlock)(
        designBlock.copy(
          dclMeta = ir.Meta.gen(Some(name), position, docOpt, annotations),
          instMode = mkInstMode(args)
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
        try dfc.mutableDB.immutable.check() // various checks post initial elaboration
        catch
          case err: IllegalArgumentException =>
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
      // top level is tagged with the default RT Domain configuration
      // (the top level may be an ED or DF design, so it cannot save the default RT configuration as part
      // of the domain type, but this could be needed later for compilation stages)
      val tags =
        if (dfc.ownerOption.isEmpty) dfc.tags.tag(dfc.elaborationOptions.defaultRTDomainCfg)
        else dfc.tags
      ir.DFDesignBlock(
        domain, dclMeta, instMode, dfc.ownerOrEmptyRef, dfc.getMeta, tags
      )
        .addMember
        .asFE
    end apply
  end Block
  extension [D <: Design](dsn: D)
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
    def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): D =
      import dfc.getSet
      dsn.setOwner(
        dsn.owner.asIR
          .setTags(_.tag(customTag))
          .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
          .asFE
      )
    def setName(name: String)(using dfc: DFC): D =
      import dfc.getSet
      dsn.setOwner(
        dsn.owner.asIR
          .setMeta(m =>
            if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta.setName(name)
            else m.setName(name)
          ).asFE
      )
  end extension

end Design

abstract class DFDesign extends DomainContainer(DomainType.DF), Design

abstract class RTDesign(cfg: RTDomainCfg = DerivedCfg) extends RTDomainContainer(cfg), Design

object RTDesign:
  protected[core] final case class Clk_main() extends DFOpaque.Clk
  protected[core] final case class Rst_main() extends DFOpaque.Rst

abstract class EDDesign extends DomainContainer(DomainType.ED), Design

abstract class EDBlackBox(verilogSrc: EDBlackBox.Source, vhdlSrc: EDBlackBox.Source)
    extends EDDesign:
  override private[core] def mkInstMode(args: ListMap[String, Any]): InstMode =
    InstMode.BlackBox(args, verilogSrc, vhdlSrc)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
