package sourcecode

object AnonClassName {
  def run() = {
    abstract class Foo(implicit name : sourcecode.Name) { //, ownerName: OwnerName
      def getName = name.value
//      def getOwnerName = ownerName.value
    }
    val foo1 = new Foo {}
    var foo2 = new Foo {}
    lazy val foo3 = new Foo {}
    object foo4 extends Foo
    trait Bar extends Foo
    object foo5 extends Bar
    abstract class Baz(implicit name : sourcecode.Name) extends Foo //, ownerName: OwnerName
    object foo6 extends Baz

    def foo(implicit name : sourcecode.Name) : String = name.value
    val foo7 = {
      foo
      foo
      foo
    }
    val foo8 = for (i <- 0 to 4) yield foo
    val foo9 = for (i <- 0 to 4) yield for (j <- 0 to 4) yield foo
    def args(a : String, b : String, c : String) = (a, b, c)
    val foo10 = args(foo, new Bar{}.getName, foo)
    val foo11 = for (i <- 0 to 4) yield for (j <- 0 to 4) yield new Baz{}
    trait Fooish {
      implicitly[sourcecode.Name]
    }
    val foo12 = true match {
      case false => new Foo {}
      case true => new Baz{}
    }


    //It would have been better if the name will get "$anon", but the owner is `run` def
//    assert(new Foo {}.getOwnerName == "run")
//    assert(foo1.getOwnerName == "foo1")
//    assert(foo2.getOwnerName == "foo2")
//    assert(foo3.getOwnerName == "foo3$lzy" || foo3.getOwnerName == "foo3")
  }
}
