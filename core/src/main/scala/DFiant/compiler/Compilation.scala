package DFiant
package compiler
import backend.Backend
import shapeless.{:: => #:}

trait Compilation[D <: DFDesign] {
  val db : DFDesign.DB
}

final case class IRCompilation[D <: DFDesign, H <: shapeless.HList](db : DFDesign.DB) extends Compilation[D]{
  def newStage[NS <: Compilation.Stage](updatedDB : DFDesign.DB)
  : IRCompilation[D, NS #: H] = IRCompilation[D, NS #: H](updatedDB)
}

object Compilation {
  implicit def fromDFDesign[D <: DFDesign] : D => IRCompilation[D, shapeless.HNil] =
    design => IRCompilation[D, shapeless.HNil](design.getDB)

  implicit def fromCompilation[D <: DFDesign] : Compilation[D] => IRCompilation[D, shapeless.HNil] =
    c => IRCompilation[D, shapeless.HNil](c.db)

  trait Stage
}