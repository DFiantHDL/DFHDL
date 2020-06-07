package DFiant.sim

import DFiant.compiler.backend.Backend
import DFiant.compiler.backend.vhdl.{Revision, VHDLBackend}

package object tools {
  implicit def ghdl[D <: DFSimDesign, R <: Revision](
    implicit revision : R
  ) : Simulator[D, VHDLBackend[R], GHDLSimulation[D, R]] =
    (cc : Backend.CommittedCompilation[D, VHDLBackend[R]]) => GHDLSimulation[D, R](cc.db, cc.fileNameSeq)

}
