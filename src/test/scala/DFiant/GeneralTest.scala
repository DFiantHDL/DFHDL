package DFiant

import DFiant._
import DFiant.core._
import DFiant.internals._
import singleton.twoface._
import DFiant.tokens._
//import DFiant.fixedpoint._

import scala.annotation.implicitNotFound
/**
  * Created by soronpo on 12/26/16.
  */
import singleton.ops._
object GeneralTest {
  val d = DFUInt(2)
  d + d// Addable.DFUIntDFUInt[d.Width, (d)
  val one = 100
  d + one
//  d + (d + d).wc
  val aaa = TokenUInt(8, 3)

  val ff = DFUInt[8]
  ff.init(1, 7L, aaa, 100, Φ, Seq(aaa, aaa))

  ff.prev(1) + 5


  @scala.annotation.implicitNotFound("Type mismatch. ${T} is not supported.")
  trait TypeOfFoo[T] {
    def apply(foo : Foo, value : T) : T
  }
  trait Foo {
    def test[T, Out <: TypeOfFoo[T]](that : T)(implicit typeOfFoo : Out) : T = that
  }

  object TypeOfFoo{
    implicit def fromInt[R <: Int] : TypeOfFoo[R] = new TypeOfFoo[R] {
      def apply(foo : Foo, value : R) : R = value
    }
  }

  val foo = new Foo{}
  final val o = 1
  final val out = foo test o
//  implicitly[out.type =:= 1]
//  f == 1.0 //Type mismatch. Double is not supported.

}
