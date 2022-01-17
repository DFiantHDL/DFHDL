package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

abstract class DFDesign(using DFC) extends OnCreateEvents, LateConstruction, HasNamePos, HasDFC:
  final val dfc: DFC = summon[DFC]
  private final val owner: DFDesign.Block = DFDesign.Block("???", Position.unknown)
  final protected def setClsNamePos(name: String, position: Position): Unit =
    val designBlock = owner.asIR
    dfc.getSet.replace(designBlock)(
      designBlock.copy(dclName = name, dclPosition = position)
    )
  dfc.enterOwner(owner)

  override def onCreateEnd: Unit =
    dfc.exitOwner()
end DFDesign

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
  extension [D <: DFDesign](dsn: D)
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
    def printCodeString(): D =
      given Printer = DefaultPrinter
      import dsn.dfc.getSet
      println(getDB.codeString)
      dsn
end DFDesign
