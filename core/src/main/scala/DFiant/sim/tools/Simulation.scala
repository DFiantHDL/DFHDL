package DFiant
package sim
package tools

import DFiant.compiler.{Compilation, IRCompilation}
import DFiant.compiler.backend.Backend
import shapeless.{:: => #:}

import scala.annotation.implicitNotFound

trait Simulation[D <: DFSimDesign, B <: Backend.Stage, S <: Simulation.Tool] {
  val db : DFDesign.DB
  val fileSeq : Seq[String]
}

object Simulation {
//  implicit def fromDFSimDesign[D <: DFSimDesign, B <: Backend](
//    implicit compiler : Compiler[B]
//  ) : D => Simulation[D, B] = simDesign => compiler(simDesign)
//  implicit def fromCompilation[D <: DFSimDesign, B <: Backend, H <: shapeless.HList]
//  : Compilation[D, B #: H] => Simulation[D, B] = c => ??? //Simulation(c.db, )
  trait Tool extends Product with Serializable
}

@implicitNotFound("Missing a simulator import that supports the backend ${B}")
trait Simulator[D <: DFSimDesign, B <: Backend.Stage, S <: Simulation.Tool] {
  def apply(c : Backend.CommittedCompilation[D, B]) : Simulation[D, B, S]
}

