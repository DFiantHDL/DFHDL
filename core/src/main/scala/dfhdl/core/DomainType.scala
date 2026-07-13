package dfhdl.core

import dfhdl.compiler.ir
import scala.annotation.implicitNotFound

opaque type DomainType <: ir.DomainType = ir.DomainType
object DomainType:
  opaque type DF <: DomainType = DomainType
  given DF: DF = ir.DomainType.DF

  opaque type RT <: DomainType = DomainType
  val RT: RT = ir.DomainType.RT

  // summoned only at ED method (`<> EDRET`) call sites, hence the tailored message
  @implicitNotFound("An ED method can only be invoked inside an event-driven (ED) domain.")
  opaque type ED <: DomainType = DomainType
  val ED: ED = ir.DomainType.ED

  extension (domainType: DomainType) def asIR: ir.DomainType = domainType
