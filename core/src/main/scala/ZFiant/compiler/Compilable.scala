package ZFiant
package compiler
import shapeless.{:: => #:}

final case class Compilable[D <: DFDesign, S <: shapeless.HList](db : DFDesign.DB, cmdSeq : Seq[Compilable.Cmd]) {
  def newStage[NS <: Compilable.Stage](updatedDB : DFDesign.DB, addedSeq : Seq[Compilable.Cmd])
  : Compilable[D, NS #: S] = Compilable[D, NS #: S](updatedDB, cmdSeq ++ addedSeq)
}

object Compilable {
//  implicit def fromDB[D <: DFDesign, S <: shapeless.HList](db : DFDesign.DB) : Compilable[D, S] = Compilable[D, S](db)
  implicit def fromDFDesign[D <: DFDesign](design : D) : Compilable[D, shapeless.HNil] = Compilable[D, shapeless.HNil](design.getDB, Seq())
//  implicit def fromCompilable[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) : Compilable[D, S] = c

  trait Stage

  sealed trait Cmd extends Product with Serializable
  object Cmd {
    final case class GenFile(fileName : String, contents : String) extends Cmd
  }
}
