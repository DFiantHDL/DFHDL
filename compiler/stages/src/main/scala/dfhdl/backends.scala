package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.{BackendCompiler, CompiledDesign, StagedDesign, StageRunner}
import dfhdl.compiler.stages.verilog.{VerilogBackend, VerilogPrinter}
object backends:
  object verilog:
    given v2001: BackendCompiler = ???
    given sv2005: BackendCompiler = new BackendCompiler:
      def apply[D <: Design](sd: StagedDesign[D]): CompiledDesign[D] =
        val designDB = StageRunner.run(VerilogBackend)(sd.stagedDB)
        val printer = new VerilogPrinter(using designDB.getSet)
        CompiledDesign(sd.newStage(printer.printedDB))
    given sv2012: BackendCompiler = sv2005
    given sv2017: BackendCompiler = sv2005

  object vhdl:
    given v93: BackendCompiler = ???
    given v2008: BackendCompiler = ???
    given v2019: BackendCompiler = v2008
end backends
