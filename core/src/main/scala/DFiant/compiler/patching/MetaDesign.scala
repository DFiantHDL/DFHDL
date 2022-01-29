package DFiant.compiler.patching
import DFiant.core.*
import DFiant.compiler.ir
import DFiant.compiler.ir.DFVal.Modifier

abstract class MetaDesign extends DFDesign(using DFC.empty) with reflect.Selectable:
  final def plantMember[T <: ir.DFMember](member: T): T =
    dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()

  export DFiant.hdl.*
  export DFiant.core.{asValAny, asVarAny}
  extension [T <: DFTypeAny, M <: Modifier](dfVal: DFVal[T, M])
    def asInitialized: DFVal[T, M & Modifier.Initialized] =
      dfVal.asInstanceOf[DFVal[T, M & Modifier.Initialized]]
