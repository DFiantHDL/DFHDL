package dfhdl.platforms.resources
import dfhdl.core.ClkCfg.Rate
import Resource.CanConnect
import dfhdl.compiler.ir.constraints.Timing
import dfhdl.compiler.ir.DFDomainOwner
import dfhdl.*

trait ClkResource extends IO:
  val rate: Rate
  injectConstraint(Timing.Clock(rate))
object ClkResource:
  given [R <: ClkResource, C <: (Clk <> VAL)](using DFC): CanConnect[R, C] = (r, c) => r.connect(c)
  given [R <: ClkResource, DC <: core.RTDomainContainer](using dfc: DFC): CanConnect[R, DC] =
    (clkResource, domainContainer) =>
      dfc.mutableDB.ResourceOwnershipContext.connectDomainOwner(
        domainContainer.containedOwner.asIR.asInstanceOf[DFDomainOwner],
        clkResource
      )
