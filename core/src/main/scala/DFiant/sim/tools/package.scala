package DFiant.sim

import DFiant.compiler.backend.{Backend, vhdl, verilog}

package object tools {
  implicit def ghdl[D <: DFSimDesign, R <: vhdl.Revision](
    implicit revision : R
  ) : Simulator[D, vhdl.VHDLBackend[R], GHDLSimulation[D, R]] =
    (cc : Backend.CommittedCompilation[D, vhdl.VHDLBackend[R]]) => GHDLSimulation[D, R](cc.db, cc.fileNameSeq)
  implicit def verilator[D <: DFSimDesign, R <: verilog.Revision](
    implicit revision : R
  ) : Simulator[D, verilog.VerilogBackend[R], VerilatorSimulation[D, R]] =
    (cc : Backend.CommittedCompilation[D, verilog.VerilogBackend[R]]) => VerilatorSimulation[D, R](cc.db, cc.fileNameSeq)

}
