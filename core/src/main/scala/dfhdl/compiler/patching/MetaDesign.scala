package dfhdl.compiler.patching
import dfhdl.core.*
import dfhdl.compiler.ir

import scala.annotation.unchecked.uncheckedVariance

type MetaDesignAny = MetaDesign[DFC.Domain]
abstract class MetaDesign[+D <: DFC.Domain](domainType: D = DFC.Domain.DF)
    extends Design(using DFC.empty)
    with reflect.Selectable:
  final type TDomain = D @uncheckedVariance
  final protected given TDomain = domainType
  // we don't really care about the IR domain type of a meta design, since it is removed.
  // only the compile-time domain type is important for meta-programming with the relevant constraints.
  final lazy val __domainType: ir.DomainType = ir.DomainType.DF

  final def plantMember[T <: ir.DFMember](member: T): T =
    dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate errornous designs
  final override private[dfhdl] def skipChecks: Boolean = true

  export dfhdl.hdl.{RTDomainCfg => _, ClkCfg => _, RstCfg => _, *}
  export dfhdl.core.{asValAny, asVarAny, asValOf}
  extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T, Modifier[A, C, I]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized]]]
end MetaDesign
