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
  val two = 2
  val d = DFUInt(2)
  val d9 = DFUInt(9)
//  d + (d + d).wc
//  d.extendable == d2
//  d.extendable == 100
  val one = 100
//  d2 < d
//  d + one
//  d + (d + d).wc
  val aaa = TokenUInt(8, 3)

  val dd = d9 + 200
  implicitly[dd.type <:< DFUInt[9]]
//  -10 + d
  d := -1L
//  10L > d && d < 10

//  d < 10
//  -10 - d
//  (1 < d) && (d > 3)

  val ff = DFUInt[8]
  ff.init(1, 7L, aaa, 100, Φ, Seq(aaa, aaa))

//  ff + d
  val u = DFUInt(one)

  DFUInt(one)

//  ff == 1
//  d + ff
//  ff.prev(1) + 510

  @scala.annotation.implicitNotFound("Type mismatch. ${T} is not supported.")
  trait TypeOfFoo[T] {
    def apply(foo : Foo, value : T) : T
  }
  type Zero = 0
  trait Foo {
    def test[T, Out](that : T)(implicit getArg: GetArg.Aux[Zero, Out], typeOfFoo : TypeOfFoo[Out]) = typeOfFoo(this, getArg.value)
  }

  object TypeOfFoo{
    implicit def fromInt[R <: Int] : TypeOfFoo[R] = new TypeOfFoo[R] {
      def apply(foo : Foo, value : R) : R = value
    }
  }

  val foo = new Foo{}
  val o = 1
  final val out = foo test o
//  implicitly[out.type =:= o.type]
//  foo test 1.0 //Type mismatch. Double is not supported.

}

object Bla {
  import DFiant._
  val a = DFBits(8).init(1)
}