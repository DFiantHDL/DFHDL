package dfhdl

import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign}
import dfhdl.core.Design
import dfhdl.options.CompilerOptions
import dfhdl.compiler.printing.Printer
export compiler.stages.printCodeString

extension [D <: Design](dsn: D)
  def compile(using
      co: CompilerOptions
  ): CompiledDesign[D] = StagedDesign(dsn).compile
