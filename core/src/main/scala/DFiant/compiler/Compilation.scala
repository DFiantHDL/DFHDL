package DFiant
package compiler

trait Compilation[D <: DFDesign] {
  val dsn : D
  val db : DFDesign.DB
}

final case class IRCompilation[D <: DFDesign](dsn : D, db : DFDesign.DB) extends Compilation[D]{
  def newStage(updatedDB : DFDesign.DB) : IRCompilation[D] = IRCompilation[D](dsn, updatedDB)
}

object Compilation {
  implicit def fromDB : DFDesign.DB => IRCompilation[DFDesign] =
    db => IRCompilation[DFDesign](null, db)

  implicit def fromDFDesign[D <: DFDesign] : D => IRCompilation[D] =
    dsn => IRCompilation[D](dsn, dsn.getDB)

  implicit def fromCompilation[D <: DFDesign] : Compilation[D] => IRCompilation[D] =
    c => IRCompilation[D](c.dsn, c.db)
}
