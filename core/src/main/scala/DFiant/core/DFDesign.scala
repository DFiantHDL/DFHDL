package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

abstract class DFDesign(using DFC) extends OnCreateEvents, LateConstruction, HasTypeName, HasDFC:
  final val dfc: DFC = summon[DFC]
  private final val owner: DFOwner = DFDesign.Block(typeName)
  dfc.enterOwner(owner)

  override def onCreateEnd: Unit =
    dfc.exitOwner()

object DFDesign:
  object Block:
    def apply(designType: String)(using DFC): DFOwner =
      val ownerRef: ir.DFOwner.Ref =
        dfc.ownerOption.map(_.asIR.ref).getOrElse(ir.DFRef.OneWay.Empty)
      ir.DFDesignBlock(
        designType,
        false,
        ownerRef,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
  extension [D <: DFDesign](dsn: D)(using TopLevel)
    def getDB: ir.DB = dsn.dfc.mutableDB.immutable
    def printCodeString(): D =
      given Printer = DefaultPrinter
      println(getDB.codeString)
      dsn
end DFDesign
