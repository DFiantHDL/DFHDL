package dfhdl

import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign, StageRunner}
import dfhdl.compiler.stages.verilog.{VerilogBackend, VerilogPrinter, VerilogDialect}
import dfhdl.compiler.stages.vhdl.{VHDLBackend, VHDLPrinter, VHDLDialect}
import dfhdl.compiler.ir.DB
object backends:
  protected[dfhdl] class verilog(val dialect: VerilogDialect) extends BackendCompiler:
    def printer(
        designDB: DB
    )(using CompilerOptions, PrinterOptions): Printer =
      val compiledDB = StageRunner.run(VerilogBackend)(designDB)
      new VerilogPrinter(dialect)(using compiledDB.getSet)
    override def toString(): String = s"verilog.$dialect"
  object verilog extends verilog(VerilogDialect.sv2009):
    val v95: verilog = new verilog(VerilogDialect.v95)
    val v2001: verilog = new verilog(VerilogDialect.v2001)
    val sv2005: verilog = new verilog(VerilogDialect.sv2005)
    val sv2009: verilog = this
    val sv2012: verilog = new verilog(VerilogDialect.sv2012)
    val sv2017: verilog = new verilog(VerilogDialect.sv2017)

  protected[dfhdl] class vhdl(val dialect: VHDLDialect) extends BackendCompiler:
    def printer(
        designDB: DB
    )(using CompilerOptions, PrinterOptions): Printer =
      val compiledDB = StageRunner.run(VHDLBackend)(designDB)
      new VHDLPrinter(dialect)(using compiledDB.getSet)
    override def toString(): String = s"vhdl.$dialect"
  object vhdl extends vhdl(VHDLDialect.v2008):
    val v93: vhdl = new vhdl(VHDLDialect.v93)
    val v2008: vhdl = this
    val v2019: vhdl = new vhdl(VHDLDialect.v2019)
end backends
