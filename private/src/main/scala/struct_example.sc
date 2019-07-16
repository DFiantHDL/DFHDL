object Test {
  trait Baz {
    type BNF <: Baz
    //Will refer to a concrete Baz with No Foo
    type BWF <: BNF with BazWithFoo //Will refer to a concrete Baz With Foo
  }

  trait BazWithFoo extends Baz {
    //Just like Baz, but adds foo
    def foo(): Unit = {}
  }


  trait BazLightyear extends Baz {
    type BNF = BazLightyear
    type BWF = BazLightyearWF

    def toInfinity(): Unit = {}
  }

  case class BazLightyearWF() extends BazLightyear with BazWithFoo {
    def andBeyond(): Unit = {}
  }


  trait BazOfABee extends Baz {
    type BNF = BazOfABee
    type BWF = BazOfABeeWF

    def isLoud(): Unit = {}
  }

  case class BazOfABeeWF() extends BazOfABee with BazWithFoo {
    def isDangerous(): Unit = {}
  }


  trait BazContainer extends Baz {
    //Container is also a Baz
    type B1 <: Baz
    //Containable Baz type 1
    type B2 <: Baz
    //Containable Baz type 2
    type B3 <: Baz //Containable Baz type 3

    type Ret_B1 <: B1#BNF
    type Ret_B2 <: B2#BNF
    type Ret_B3 <: B3#BNF

    //insert accepts only `WithFoo` Baz instances.
    //insert returns:
    //If the container is Baz (without Foo), then return the `without Foo` casting of the input baz
    //If the container is BazWithFoo, then return the `WithFoo` casting of the input baz
    def insert(b1: B1#BWF): Ret_B1 = b1.asInstanceOf[Ret_B1]

    def insert(b2: B2#BWF)(implicit di1: DummyImplicit): Ret_B2 = b2.asInstanceOf[Ret_B2]

    def insert(b3: B3#BWF)(implicit di1: DummyImplicit, di2: DummyImplicit): Ret_B3 = b3.asInstanceOf[Ret_B3]
  }

  trait BazContainerWF extends BazContainer with BazWithFoo {
    type Ret_B1 = B1#BWF
    type Ret_B2 = B2#BWF
    type Ret_B3 = B3#BWF
  }


  trait MyBazContainer extends BazContainer {
    type BNF = MyBazContainer
    type BWF = MyBazContainerWF

    type B1 = BazLightyear
    type B2 = BazOfABee

    val bly1 = insert(BazLightyearWF())
    val bly2 = insert(BazLightyearWF())
    val boab = insert(BazOfABeeWF())

    def doSomething(): Unit = {
      bly1.toInfinity()
      //Here we cannot invoke `bly1.andBeyond()`
    }
  }

  case class MyBazContainerWF() extends MyBazContainer with BazContainerWF {
    def doSomethingWF(): Unit = {
      bly1.toInfinity()
      bly1.andBeyond() //Here we can use `andBeyond`
      boab.foo() //Here we can use `foo`
    }
  }

}

