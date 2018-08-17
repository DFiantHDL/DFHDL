abstract class TTT(implicit n : sourcecode.Name) {
  val a=n.value
}

object Foo extends TTT
Foo.a
