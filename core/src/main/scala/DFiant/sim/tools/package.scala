package DFiant.sim

import DFiant.compiler.backend.{BackendStage, vhdl, verilog}

package object tools {
  implicit def ghdl[D <: DFSimDesign, R <: vhdl.Revision](
    implicit revision : R
  ) : Simulator[D, vhdl.Backend[R], GHDLSimulation[D, R]] =
    (cc : BackendStage.CommittedCompilation[D, vhdl.Backend[R]]) => GHDLSimulation[D, R](cc.db, cc.fileNameSeq)
  implicit def verilator[D <: DFSimDesign, R <: verilog.Revision](
    implicit revision : R
  ) : Simulator[D, verilog.Backend[R], VerilatorSimulation[D, R]] =
    (cc : BackendStage.CommittedCompilation[D, verilog.Backend[R]]) => VerilatorSimulation[D, R](cc.db, cc.fileNameSeq)
  implicit def modelsim[D <: DFSimDesign, B <: BackendStage](
    implicit supportedBackend : ModelsimSimulation.SupportedBackend[B]
  ) : Simulator[D, B, ModelsimSimulation[D, B]] =
    (cc : BackendStage.CommittedCompilation[D, B]) => ModelsimSimulation[D, B](cc.db, cc.fileNameSeq)

}
