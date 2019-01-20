trait Foo {
  trait Bar {
    def run : Unit
  }
  val bar : Bar
}

trait Foo2 extends Foo {
  trait Bar extends super.Bar {
    override def run: Unit = println("Foo2")
  }
  val bar : Bar = new Bar { }
}

trait Foo3 extends Foo2 {
  trait Bar extends super.Bar {
    override def run: Unit = println("Foo3")
  }
  override val bar : Bar = new Bar { }
}

val foo2 = new Foo2 {}

val foo3 = new Foo3 {}

foo2.bar.run
foo3.bar.run