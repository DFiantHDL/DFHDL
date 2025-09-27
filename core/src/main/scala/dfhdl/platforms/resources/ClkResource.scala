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
  given [R <: ClkResource, C <: (Clk <> VAL)](using DFC): CanConnect[R, C] with
    def connect(resource: R, dfVal: C)(using DFC): Unit = resource.connect(dfVal)
  given [R <: ClkResource, DC <: core.RTDomainContainer](using dfc: DFC): CanConnect[R, DC] with
    def connect(clkResource: R, domainContainer: DC)(using DFC): Unit =
      dfc.mutableDB.ResourceOwnershipContext.connectDomainOwner(
        domainContainer.containedOwner.asIR.asInstanceOf[DFDomainOwner],
        clkResource
      )
