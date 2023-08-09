package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}

trait BackendCompiler:
  def compile[D <: Design](
      sd: StagedDesign[D]
  )(using CompilerOptions, PrinterOptions): CompiledDesign[D]
object BackendCompiler:
  given BackendCompiler = dfhdl.backends.verilog.sv2005
