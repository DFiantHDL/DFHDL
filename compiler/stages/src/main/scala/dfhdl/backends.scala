package dfhdl

import dfhdl.core.Design
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign, StageRunner}
import dfhdl.compiler.stages.verilog.{VerilogBackend, VerilogPrinter}
object backends:
  protected class verilog extends BackendCompiler:
    def compile[D <: Design](sd: StagedDesign[D])(using CompilerOptions): CompiledDesign[D] =
      val designDB = StageRunner.run(VerilogBackend)(sd.stagedDB)
      val printer = new VerilogPrinter(using designDB.getSet)
      CompiledDesign(sd.newStage(printer.printedDB))
      
    given v2001: BackendCompiler = ???
    given sv2005: BackendCompiler = this
    given sv2012: BackendCompiler = sv2005
    given sv2017: BackendCompiler = sv2005
  given verilog: verilog = new verilog

  protected class vhdl extends BackendCompiler:
    def compile[D <: Design](sd: StagedDesign[D])(using CompilerOptions): CompiledDesign[D] = ???
    given v93: BackendCompiler = ???
    given v2008: BackendCompiler = ???
    given v2019: BackendCompiler = this

  given vhdl: vhdl = new vhdl
end backends
