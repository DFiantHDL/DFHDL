package DFiant.sim

import DFiant.DFDesign
import DFiant.compiler.backend.BackendStage

import scala.annotation.implicitNotFound

trait Simulation[D <: DFSimDesign, B <: BackendStage] {
  val db : DFDesign.DB
  val fileNameSeq : Seq[String]

  /**
    * Runs the simulator for the given simulation
    * @return this simulation
    */
  def run() : this.type
}

@implicitNotFound("Missing a simulator import that supports the backend ${B} (e.g., `import sim.tools.modelsim`")
trait Simulator[D <: DFSimDesign, B <: BackendStage, S <: Simulation[D, B]] {
  def apply(c : BackendStage.CommittedCompilation[D, B]) : S
}

