
/**
  * Type-level encoding of GCD.
  *
  * @author George Leontiev
  */
object GCDExamples {
  import shapeless._
  import nat._
  import ops.nat._
  import test._

  trait GCD[X <: Nat, Y <: Nat] { type Out <: Nat }

  object GCD {
    def gcd[N <: Nat](x : Nat, y : Nat)(implicit gcd : Aux[x.N, y.N, N], wn : Witness.Aux[N]): N = wn.value

    type Aux[X <: Nat, Y <: Nat, Z <: Nat] = GCD[X, Y] { type Out = Z }

    implicit def gcd0[X <: Nat]: Aux[X, X, X] = new GCD[X, X] { type Out = X }
    implicit def gcd1[X <: Nat, Y <: Nat, Z <: Nat, Out0 <: Nat]
    (implicit ev0 : LT[X, Y], ev1 : Diff.Aux[Y, X, Z], ev2 : Aux[X, Z, Out0]): Aux[X, Y, Out0] =
      new GCD[X, Y] { type Out = Out0 }
    implicit def gcd2[X <: Nat, Y <: Nat, Out0 <: Nat]
    (implicit ev0 : LT[Y, X], ev1 : Aux[Y, X, Out0]): Aux[X, Y, Out0] = new GCD[X, Y] { type Out = Out0}
  }

  import GCD._


  val g1 = gcd(2, 3)
  g1.toInt
  typed[_1](g1)

  val g2 = gcd(4, 10)
  typed[_2](g2)

  val g3 = {
    gcd(apply(15), 6)
  }
  g3.toInt
  typed[_3](g3)

  val g4 = gcd(8, 12)
  typed[_4](g4)
}