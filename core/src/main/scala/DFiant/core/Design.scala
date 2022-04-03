package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import ir.{ClkCfg, RstCfg}
import DFiant.compiler.printing.*

private[DFiant] abstract class Design(using DFC) extends Container, HasNamePos:
  private[core] type TKind = Container.Kind.Design
  final protected given TKind = Container.Kind.Design
  private[core] final override lazy val owner: Design.Block =
    Design.Block(__domainType, "???", Position.unknown)
  final protected def setClsNamePos(name: String, position: Position): Unit =
    val designBlock = owner.asIR
    dfc.getSet.replace(designBlock)(
      designBlock.copy(dclName = name, dclPosition = position)
    )
  final override def onCreateStartLate: Unit =
    dfc.enterLate()

object Design:
  type Block = DFOwner[ir.DFDesignBlock]
  object Block:
    def apply(domain: ir.DomainType, dclName: String, dclPosition: Position)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DFDesignBlock(
        domain,
        dclName,
        dclPosition,
        false,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
  extension [D <: Design](dsn: D) def getDB: ir.DB = dsn.dfc.mutableDB.immutable
//    def printCodeString(): D =
//      import dsn.dfc.getSet
//      given Printer = DefaultPrinter
//      println(getDB.codeString)
//      dsn
end Design

abstract class DFDesign(using DFC) extends Design:
  private[core] type TDomain = ir.DomainType.DF
  private[core] lazy val __domainType: TDomain = ir.DomainType.DF

abstract class RTDesign(
    clkCfg: ClkCfg = ClkCfg(),
    rstCfg: RstCfg = RstCfg()
)(using DFC)
    extends Design:
  private[core] class TDomain extends ir.DomainType.RT(clkCfg, rstCfg)
  private[core] lazy val __domainType: TDomain = new TDomain
//  lazy val clk = clkCfg match
//    case RT.NoClock =>
//      throw new IllegalArgumentException(
//        "Tried to access `clk` but `clkCfg` are set to `NoClock`."
//      )
//    case RT.WithClock(name, _) =>
//      DFVal.Dcl(DFBit, IN)(using dfc.setName(name))
//  clkCfg match
//    case _: RT.WithClock => clk
//    case _               => // do nothing
//  lazy val rst = rstCfg match
//    case RT.NoReset =>
//      throw new IllegalArgumentException(
//        "Tried to access `rst` but `rstCfg` are set to `NoReset`."
//      )
//    case RT.WithReset(name, _, _) =>
//      DFVal.Dcl(DFBit, IN)(using dfc.setName(name))
//  rstCfg match
//    case _: RT.WithReset => rst
//    case _               => // do nothing
end RTDesign

abstract class EDDesign(using DFC) extends Design:
  private[core] class TDomain extends ir.DomainType.ED
  private[core] lazy val __domainType: TDomain = new TDomain
