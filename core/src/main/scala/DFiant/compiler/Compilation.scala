package DFiant
package compiler
import backend.BackendStage
import shapeless.{:: => #:}

trait Compilation[D <: DFDesign] {
  val dsn : D
  val db : DFDesign.DB
}

final case class IRCompilation[D <: DFDesign, H <: shapeless.HList](dsn : D, db : DFDesign.DB) extends Compilation[D]{
  def newStage[NS <: Compilation.Stage](updatedDB : DFDesign.DB)
  : IRCompilation[D, NS #: H] = IRCompilation[D, NS #: H](dsn, updatedDB)
}

object Compilation {
//  implicit def fromDB : DFDesign.DB => IRCompilation[DFDesign, shapeless.HNil] =
//    db => IRCompilation[DFDesign, shapeless.HNil](db.top, db)

  implicit def fromDFDesign[D <: DFDesign] : D => IRCompilation[D, shapeless.HNil] =
    dsn => IRCompilation[D, shapeless.HNil](dsn, dsn.getDB)

  implicit def fromCompilation[D <: DFDesign] : Compilation[D] => IRCompilation[D, shapeless.HNil] =
    c => IRCompilation[D, shapeless.HNil](c.dsn, c.db)

  trait Stage
}
