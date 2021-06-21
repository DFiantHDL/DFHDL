package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import scala.reflect.classTag
abstract class DFDesign(using DFC) extends OnCreateEvents, LateConstruction:
  val owner: DFOwner = DFDesign.Block()
  dfc.enterOwner(owner)

  override def onCreateEnd: Unit =
    dfc.exitOwner()

object DFDesign:
  object Block:
    def apply()(using DFC): DFOwner =
      val ownerRef = dfc.ownerOption match
        case Some(owner) => owner.asIR.ref
        case None        => ir.DFOwner.EmptyRef
      ir.DFDesignBlock(false, ownerRef, dfc.getOwnerMeta, ir.DFTags.empty).asFE
