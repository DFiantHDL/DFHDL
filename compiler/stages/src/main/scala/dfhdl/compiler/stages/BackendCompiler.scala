package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir.DB

trait BackendCompiler:
  final def compile(
      sd: StagedDesign
  )(using co: CompilerOptions, po: PrinterOptions): CompiledDesign =
    CompiledDesign(sd.newStage(printer(sd.stagedDB).printedDB))
  end compile
  def printer(
      designDB: DB
  )(using CompilerOptions, PrinterOptions): Printer
end BackendCompiler
