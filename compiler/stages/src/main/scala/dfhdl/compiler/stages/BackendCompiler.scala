package dfhdl.compiler.stages

import dfhdl.core.Design

trait BackendCompiler:
  def apply[D <: Design](sd: StagedDesign[D]): CompiledDesign[D]
object BackendCompiler:
  given BackendCompiler = dfhdl.backends.verilog.sv2005
