package DFiant.compiler.patching
import DFiant.core.*
import DFiant.compiler.ir

import scala.annotation.unchecked.uncheckedVariance

type MetaDesignAny = MetaDesign[ir.DomainType]
abstract class MetaDesign[+D <: ir.DomainType](domainType: D = ir.DomainType.DF)
    extends Design(using DFC.empty)
    with reflect.Selectable:
  final type TDomain = D @uncheckedVariance
  final lazy val __domainType: TDomain = domainType
  final def plantMember[T <: ir.DFMember](member: T): T =
    dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate errornous designs
  final override private[DFiant] def skipChecks: Boolean = true

  export DFiant.hdl.*
  export DFiant.core.{asValAny, asVarAny}
  extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized]]]
end MetaDesign
