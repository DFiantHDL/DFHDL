package dfhdl

import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign}
import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
export compiler.stages.printCodeString

extension (dsn: Design)
  def compile(using CompilerOptions, PrinterOptions): CompiledDesign = StagedDesign(dsn).compile
