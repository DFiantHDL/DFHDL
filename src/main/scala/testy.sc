abstract class Foo(implicit enc : sourcecode.FullName) {
}

trait Foo2 {
  println(getClass.getInterfaces.head.getDeclaringClass.getName)

}
new Foo2 {}