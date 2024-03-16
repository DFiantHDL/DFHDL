package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir.DB

trait BackendCompiler:
  final def compile[D <: Design](
      sd: StagedDesign[D]
  )(using CompilerOptions, PrinterOptions): CompiledDesign[D] =
    CompiledDesign(sd.newStage(printer(sd.stagedDB).printedDB))
  def printer(
      designDB: DB
  )(using CompilerOptions, PrinterOptions): Printer
