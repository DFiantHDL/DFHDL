object Test {
  trait Foo {
    type TVal = Foo
    final val left : TVal = this.asInstanceOf[TVal]
    final def f1 = 1
    final def f2[R](right : R)(implicit one : OtherOne[TVal, R]) = one(left, right)
  }

  trait One[L, R] {
    type Out
    def apply(left : L, right : R) : Out
  }

  trait OtherOne[L, R] extends One[L, R]
}
