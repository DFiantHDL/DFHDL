package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
object RTDomainCfg:
  def apply(clkCfg: ir.ClkCfg, rstCfg: ir.RstCfg)(using ctName: CTName): ir.RTDomainCfg =
    ir.RTDomainCfg.Explicit(ctName.value, clkCfg, rstCfg)
