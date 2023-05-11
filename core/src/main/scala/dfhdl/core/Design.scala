package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import ir.DFDesignBlock.InstMode

import scala.annotation.{Annotation, implicitNotFound}
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

private[dfhdl] abstract class Design(using DFC) extends Container, HasNamePos:
  private[core] type TScope = DFC.Scope.Design
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Design
  private[core] def mkInstMode(args: ListMap[String, Any]): InstMode = InstMode.Normal
  final private[core] def initOwner: TOwner =
    Design.Block(__domainType, ir.Meta(Some("???"), Position.unknown, None, Nil), InstMode.Normal)
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
          dclMeta = ir.Meta(Some(name), position, docOpt, annotations.getActiveHWAnnotations),
          instMode = mkInstMode(args)
        )
      ).asFE
    )
  end setClsNamePos
  final override def onCreateStartLate: Unit =
    dfc.enterLate()
end Design

object Design:
  import ir.DFDesignBlock.InstMode
  type Block = DFOwner[ir.DFDesignBlock]
  object Block:
    def apply(domain: ir.DomainType, dclMeta: ir.Meta, instMode: InstMode)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DFDesignBlock(
        domain,
        dclMeta,
        instMode,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      )
        .addMember
        .asFE
    end apply
  end Block
  extension [D <: Design](dsn: D)
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
    def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using
        dfc: DFC
    ): D =
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

abstract class DFDesign(using DFC) extends Design:
  private[core] type TDomain = DFC.Domain.DF
  final protected given TDomain = DFC.Domain.DF
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.DF

abstract class RTDesign(
    cfg: ir.RTDomainCfg = ir.DerivedCfg
)(using DFC)
    extends Design:
  private[core] type TDomain = DFC.Domain.RT
  final protected given TDomain = DFC.Domain.RT
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.RT(cfg)

  //  /** This is a reference to the clock used. `clkCfg` must be explicitly defined with a name before
//    * using this value.
//    */
//  final lazy val clk = clkCfg match
//    case ClkCfg.Explicit(name: String, _) =>
//      DFVal.Dcl(DFBit, Modifier.IN)(using dfc.setName(name))
//    case _ =>
//      throw new IllegalArgumentException(
//        "Tried to access `clk` but `clkCfg` has no explicit clock name."
//      )
//  // forcing the clock to be added if the name is explicitly defined
//  clkCfg match
//    case ClkCfg.Explicit(_: String, _) => clk // touching lazy value
//    case _                             => // do nothing
//  /** This is a reference to the reset used. `rstCfg` must be explicitly defined with a name before
//    * using this value.
//    */
//  lazy val rst = rstCfg match
//    case RstCfg.Explicit(name: String, _, _) =>
//      DFVal.Dcl(DFBit, Modifier.IN)(using dfc.setName(name))
//    case _ =>
//      throw new IllegalArgumentException(
//        "Tried to access `rst` but `rstCfg` has no explicit reset name."
//      )
//  // forcing the reset to be added if the name is explicitly defined
//  rstCfg match
//    case RstCfg.Explicit(_: String, _, _) => rst
//    case _                                => // do nothing
end RTDesign

abstract class EDDesign(using DFC) extends Design:
  private[core] type TDomain = DFC.Domain.ED
  final protected given TDomain = DFC.Domain.ED
  final private[core] lazy val __domainType: ir.DomainType = ir.DomainType.ED

abstract class EDBlackBox(verilogSrc: EDBlackBox.Source, vhdlSrc: EDBlackBox.Source)(using DFC)
    extends EDDesign:
  override private[core] def mkInstMode(args: ListMap[String, Any]): InstMode =
    InstMode.BlackBox(args, verilogSrc, vhdlSrc)
object EDBlackBox:
  export ir.DFDesignBlock.InstMode.BlackBox.Source
