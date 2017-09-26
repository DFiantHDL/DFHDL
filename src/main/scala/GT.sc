type ¬[A] = A => Nothing
type ¬¬[A] = ¬[¬[A]]
type ∨[T, U] = ¬[¬[T] with ¬[U]]
type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

sealed class =!=[A,B]

trait LowerPriorityImplicits {
  implicit def equal[A]: =!=[A, A] = sys.error("should not be called")
}
object =!= extends LowerPriorityImplicits {
  implicit def nequal[A,B](implicit same: A =:= B = null): =!=[A,B] =
    if (same != null) sys.error("should not be called explicitly with same type")
    else new =!=[A,B]
}

case class Foo[A,B](a: A, b: B)(implicit e: A =!= B)

trait Printy {
  def print()
}

case class Printy1() extends Printy {
  def print() = println("Printy1")
}

case class Printy2() extends Printy {
  def print() = println("Printy2")
}

case class Printy3() extends Printy {
  def print() = println("Printy3")
}

//case class PrintService[P1 <: Printy, P2 <: Printy, P3 <: Printy]() {
//  def print(p : P1) = p.print
//  def print(p : P2)(implicit d1: DummyImplicit) = p.print
//  def print(p : P3)(implicit d1: DummyImplicit, d2: DummyImplicit) = p.print
//}


abstract class PrintServiceA() {
  type P1 <: Printy
  type P2 <: Printy
  type P3 <: Printy
  def print(p : P1) = p.print
  def print(p : P2)(implicit d1: DummyImplicit) = p.print
  def print(p : P3)(implicit d1: DummyImplicit, d2: DummyImplicit) = p.print
}


case class PrintService() extends PrintServiceA {
  type P1 = Printy1
//  type P2 = Printy2
//  type P3 = Printy3
}

val ps = PrintService()
ps.print(Printy1())
