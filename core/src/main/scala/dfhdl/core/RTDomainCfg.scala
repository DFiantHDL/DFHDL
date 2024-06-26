package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

final val DerivedCfg = ir.DerivedCfg
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

type RTDomainCfg = ir.RTDomainCfg
object RTDomainCfg:
  def apply(clkCfg: ClkCfg, rstCfg: RstCfg)(using ctName: CTName): RTDomainCfg =
    ir.RTDomainCfg.Explicit(ctName.value, clkCfg, rstCfg)
