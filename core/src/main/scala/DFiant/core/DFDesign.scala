package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

import scala.reflect.classTag
abstract class DFDesign(using val dfc: DFC)
    extends OnCreateEvents,
      LateConstruction,
      HasTypeName:
  private final val owner: DFOwner = DFDesign.Block(typeName)
  dfc.enterOwner(owner)

  override def onCreateEnd: Unit =
    dfc.exitOwner()

object DFDesign:
  object Block:
    def apply(designType: String)(using DFC): DFOwner =
      val ownerRef = dfc.ownerOption match
        case Some(owner) => owner.asIR.ref
        case None        => ir.DFOwner.EmptyRef
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
