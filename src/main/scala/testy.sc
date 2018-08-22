import DFiant._

class Foo(val name : String) {
  override def toString: String = name
}

abstract class Bar(implicit foo : Foo) {
  override def toString: String = foo.name
}

object Test {

}