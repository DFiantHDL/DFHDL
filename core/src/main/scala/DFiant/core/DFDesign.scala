package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

private abstract class Design(using DFC)
    extends Container(_ ?=> DFDesign.Block("???", Position.unknown)),
      HasNamePos:
  final protected def setClsNamePos(name: String, position: Position): Unit =
    val designBlock = owner.asIR
    dfc.getSet.replace(designBlock)(
      designBlock.copy(dclName = name, dclPosition = position)
    )

abstract class DFDesign(using DFC) extends Design:
  private[core] type TDmn = Domain.DF
  protected given TDmn = Domain.DF
abstract class RTDesign(using DFC) extends Design:
  private[core] class TDmn extends Domain.RT.HL
  protected given TDmn = new TDmn
abstract class LLRTDesign(using DFC) extends Design:
  private[core] class TDmn extends Domain.RT.LL
  protected given TDmn = new TDmn

object DFDesign:
  type Block = DFOwner[ir.DFDesignBlock]
  object Block:
    def apply(dclName: String, dclPosition: Position)(using DFC): Block =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DFDesignBlock(
        dclName,
        dclPosition,
        false,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
  extension [D <: DFDesign](dsn: D) def getDB: ir.DB = dsn.dfc.mutableDB.immutable
//    def printCodeString(): D =
//      import dsn.dfc.getSet
//      given Printer = DefaultPrinter
//      println(getDB.codeString)
//      dsn
end DFDesign
