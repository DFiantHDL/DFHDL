import shapeless._, syntax.singleton._
import shapeless._
import syntax.singleton._

trait Father {
  val f = 0
}
trait Son extends Father {
  val s = 0
}

object Test {
	val (wTrue, wFalse) = (Witness(true), Witness(false))
	type True = wTrue.T
	type False = wFalse.T
	trait Select[B] { type Out }
	implicit val seaaalFather = new Select[True] { type Out = Father }
	implicit val selSon= new Select[False] { type Out = Son }
	def select(b: WitnessWith[Select])(t: b.instance.Out) = t
	val f = select(true)(new Son{})
	val s = select(false)(new Son{})
	println(f.f)
	println(f.s)
	println(s.f)
	println(s.s)
}

import DFiant.core._

