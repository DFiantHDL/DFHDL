trait Foo {
  trait Bar {
    def run : Unit
  }
  val bar : Bar
}

trait Foo2 extends Foo {
  trait Foo2Bar extends Bar {
    override def run: Unit = println("Foo2")
  }
  val bar : Foo2Bar = new Foo2Bar { }
}

trait Foo3 extends Foo2 {
  trait Foo3Bar extends Foo2Bar {
    override def run: Unit = println("Foo3")
  }
  override val bar : Foo3Bar = new Foo3Bar { }
}

trait Foo4 extends Foo3 {
  trait Foo4Bar extends Foo3Bar with Foo2Bar {
    override def run: Unit = println("Foo3")
  }
  override val bar : Foo4Bar = new Foo4Bar { }
}

val foo2 = new Foo2 {}

val foo3 = new Foo3 {}

foo2.bar.run
foo3.bar.run