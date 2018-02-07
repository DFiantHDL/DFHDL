import DFiant._
import singleton.ops._
import scodec.bits._
import DFiant.internals._

trait Able[T]
object Able {
  implicit def fromInt(i : Int) : Able[Int] = ???
}

trait Builder[L, R]
object Builder {
  implicit def ev[T] : Able[T] = ???
}


trait Foo {
  def := [T](that : Able[T])(implicit bld : Builder[]) : Unit = {}
}


val f = new Foo {}


f := 1


