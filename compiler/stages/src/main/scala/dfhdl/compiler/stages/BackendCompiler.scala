package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.ir.DB

//TODO: consider a way to combine the two printer methods into one,
//without causing recompilation in DFApp when printing the backend code
trait BackendCompiler:
  final def compile(
      sd: StagedDesign
  )(using co: CompilerOptions, po: PrinterOptions): CompiledDesign =
    CompiledDesign(sd.newStage(printer(sd.stagedDB).printedDB))
  end compile
  def printer(
      cd: CompiledDesign
  )(using CompilerOptions, PrinterOptions): Printer
  def printer(
      designDB: DB
  )(using CompilerOptions, PrinterOptions): Printer
end BackendCompiler
