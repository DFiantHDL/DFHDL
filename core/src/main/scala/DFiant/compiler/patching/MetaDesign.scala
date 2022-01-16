package DFiant.compiler.patching
import DFiant.core.*
import DFiant.compiler.ir

abstract class MetaDesign extends DFDesign(using DFC.empty):
  final def plantMember[T <: ir.DFMember](member: T): T =
    ??? // dfc.mutableDB.plantMember(this, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()

  export DFiant.hdl.*
//  final protected implicit val __lateConstructionConfig: LateConstructionConfig =
//    LateConstructionConfig.Force(lateConstruction)
