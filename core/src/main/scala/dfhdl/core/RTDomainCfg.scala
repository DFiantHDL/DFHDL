package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import dfhdl.options.ElaborationOptions
import Freq.Ops.*

type ClkCfg = ir.ClkCfg
object ClkCfg:
  type Edge = ir.ClkCfg.Edge
  final val Edge = ir.ClkCfg.Edge

  def apply(
      edge: Edge = Edge.Rising,
      rate: Rate = 50.MHz,
      portName: String = "clk"
  ): ClkCfg = ir.ClkCfg.Explicit(edge, rate.asIR, portName)

type RstCfg = ir.RstCfg
object RstCfg:
  type Mode = ir.RstCfg.Mode
  final val Mode = ir.RstCfg.Mode
  type Active = ir.RstCfg.Active
  final val Active = ir.RstCfg.Active
  def apply(
      mode: Mode = Mode.Sync,
      active: Active = Active.High,
      portName: String = "rst"
  ): RstCfg = ir.RstCfg.Explicit(mode, active, portName)

opaque type RTDomainCfg <: ir.RTDomainCfg = ir.RTDomainCfg
object RTDomainCfg:
  def forced(name: String, clkCfg: ClkCfg, rstCfg: RstCfg): RTDomainCfg =
    ir.RTDomainCfg.Explicit(name, clkCfg, rstCfg)
  def apply(clkCfg: ClkCfg, rstCfg: RstCfg)(using ctName: CTName): RTDomainCfg =
    forced(ctName.value, clkCfg, rstCfg)
  val Comb: RTDomainCfg = RTDomainCfg.forced("RTDomainCfg.Comb", None, None)
  def Default(using dfc: DFC): RTDomainCfg = dfc.elaborationOptions.defaultRTDomainCfg
  val Derived: RTDomainCfg = ir.RTDomainCfg.Derived
  extension (cfg: RTDomainCfg)
    def asIR: ir.RTDomainCfg = cfg
    def norst: RTDomainCfg = cfg.asIR.norst
  extension (cfg: ir.RTDomainCfg) def asFE: RTDomainCfg = cfg
  protected[core] object Related:
    def apply(design: RTDesign)(using DFC): RTDomainCfg =
      ir.RTDomainCfg.Related(design.owner.asIR.refTW)
    def apply(domain: RTDomain)(using DFC): RTDomainCfg =
      ir.RTDomainCfg.Related(domain.owner.asIR.refTW)
end RTDomainCfg
