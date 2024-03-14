package dfhdl

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign, StageRunner}
import dfhdl.compiler.stages.verilog.{VerilogBackend, VerilogPrinter, VerilogDialect}
import dfhdl.compiler.stages.vhdl.{VHDLBackend, VHDLPrinter, VHDLDialect}
object backends:
  protected[dfhdl] class verilog(val dialect: VerilogDialect) extends BackendCompiler:
    def compile[D <: Design](
        sd: StagedDesign[D]
    )(using CompilerOptions, PrinterOptions): CompiledDesign[D] =
      val designDB = StageRunner.run(VerilogBackend)(sd.stagedDB)
      val printer = new VerilogPrinter(using designDB.getSet)
      CompiledDesign(sd.newStage(printer.printedDB))
  object verilog extends verilog(VerilogDialect.sv2005):
    val v2001: verilog = new verilog(VerilogDialect.v2001)
    val sv2005: verilog = this
    val sv2012: verilog = new verilog(VerilogDialect.sv2012)
    val sv2017: verilog = new verilog(VerilogDialect.sv2017)

  protected[dfhdl] class vhdl(val dialect: VHDLDialect) extends BackendCompiler:
    def compile[D <: Design](
        sd: StagedDesign[D]
    )(using CompilerOptions, PrinterOptions): CompiledDesign[D] =
      val designDB = StageRunner.run(VHDLBackend)(sd.stagedDB)
      val printer = new VHDLPrinter(using designDB.getSet)
      CompiledDesign(sd.newStage(printer.printedDB))
  object vhdl extends vhdl(VHDLDialect.v2008):
    val v93: vhdl = new vhdl(VHDLDialect.v93)
    val v2008: vhdl = this
    val v2019: vhdl = new vhdl(VHDLDialect.v2019)
end backends
