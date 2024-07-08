package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

type ClkCfg = ir.ClkCfg
object ClkCfg:
  type Edge = ir.ClkCfg.Edge
  final val Edge = ir.ClkCfg.Edge

  def apply(
      edge: Edge = Edge.Rising
  ): ClkCfg = ir.ClkCfg.Explicit(edge)

type RstCfg = ir.RstCfg
object RstCfg:
  type Mode = ir.RstCfg.Mode
  final val Mode = ir.RstCfg.Mode
  type Active = ir.RstCfg.Active
  final val Active = ir.RstCfg.Active
  def apply(
      mode: Mode = Mode.Sync,
      active: Active = Active.High
  ): RstCfg = ir.RstCfg.Explicit(mode, active)

opaque type RTDomainCfg <: ir.RTDomainCfg = ir.RTDomainCfg
object RTDomainCfg:
  def forced(name: String, clkCfg: ClkCfg, rstCfg: RstCfg): RTDomainCfg =
    ir.RTDomainCfg.Explicit(name, clkCfg, rstCfg)
  def apply(clkCfg: ClkCfg, rstCfg: RstCfg)(using ctName: CTName): RTDomainCfg =
    forced(ctName.value, clkCfg, rstCfg)
  extension (cfg: RTDomainCfg) def asIR: ir.RTDomainCfg = cfg
  extension (cfg: ir.RTDomainCfg) def asFE: RTDomainCfg = cfg
  protected[core] object RelatedCfg:
    def apply(design: RTDesign)(using DFC): RTDomainCfg =
      ir.RTDomainCfg.RelatedCfg(design.owner.asIR.refTW)
    def apply(domain: RTDomain)(using DFC): RTDomainCfg =
      ir.RTDomainCfg.RelatedCfg(domain.owner.asIR.refTW)

final val CombCfg: RTDomainCfg = RTDomainCfg.forced("comb", None, None)
final val DerivedCfg: RTDomainCfg = ir.RTDomainCfg.DerivedCfg
