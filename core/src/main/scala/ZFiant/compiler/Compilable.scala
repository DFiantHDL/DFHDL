package ZFiant
package compiler

trait Compilable[-T] {
  def apply(t : T) : DFDesign.DB
}
object Compilable {
  def apply[T](implicit comp : Compilable[T]) : Compilable[T] = comp
  implicit val fromDB : Compilable[DFDesign.DB] = t => t
  implicit val fromDFDesign : Compilable[DFDesign] = t => t.db
}
