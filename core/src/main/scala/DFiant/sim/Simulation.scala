package DFiant.sim

import DFiant.DFDesign
import DFiant.compiler.backend.Backend

import scala.annotation.implicitNotFound

trait Simulation[D <: DFSimDesign, B <: Backend.Stage] {
  val db : DFDesign.DB
  val fileNameSeq : Seq[String]
  def run() : this.type
}

@implicitNotFound("Missing a simulator import that supports the backend ${B}")
trait Simulator[D <: DFSimDesign, B <: Backend.Stage, S <: Simulation[D, B]] {
  def apply(c : Backend.CommittedCompilation[D, B]) : S
}

