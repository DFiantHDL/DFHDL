import DFiant._
import singleton.ops._
import scodec.bits._
import DFiant.internals._
import shapeless._

abstract class Able[A[T0], T](value : T)
object Able {
  implicit def fromInt[R <: Int, A[T]](i : R)(implicit gen : Generic[A[R]])
  : Able[A, R] = gen.from((i :: HNil).asInstanceOf[gen.Repr]).asInstanceOf[Able[A, R]]
}


case class AbleY[T](value : T) extends Able[AbleY, T](value)
object AbleY {
//  implicit def fromInt(i : Int) : AbleY[Int] = ???
}


trait Foo {
  def := [T](that : AbleY[T]) : Unit = {}
}

//val a = implicitly[Generic[AbleY[Int]]]
//
//val b = a.to(AbleY[Int](2))
//val c = a.from((2 :: HNil).asInstanceOf[a.Repr])
val f = new Foo {}


f := 1


