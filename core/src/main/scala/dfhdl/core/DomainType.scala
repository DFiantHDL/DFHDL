package dfhdl.core

import dfhdl.compiler.ir

opaque type DomainType <: ir.DomainType = ir.DomainType
object DomainType:
  opaque type DF <: DomainType = DomainType
  val DF: DF = ir.DomainType.DF
  given DF = DF

  opaque type RT <: DomainType = DomainType
  object RT:
    def apply(cfg: RTDomainCfg): RT = ir.DomainType.RT(cfg.asIR)

  opaque type ED <: DomainType = DomainType
  val ED: ED = ir.DomainType.ED

  extension (domainType: DomainType) def asIR: ir.DomainType = domainType
