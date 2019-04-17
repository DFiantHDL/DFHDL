import scala.reflect.runtime.universe._

trait Bar
lazy val mirror = runtimeMirror(getClass.getClassLoader)

def getConstructors(clazz: Class[_], filter: List[String] => Boolean): List[(MethodMirror, List[String])] = {
  val symbol = mirror.classSymbol(clazz) // the class symbol
  val classMirror = mirror.reflectClass(symbol) // the class mirror
  if (!symbol.isModuleClass && !symbol.isAbstractClass)
    symbol.toType.members.toList
      .collect {
        case m: MethodSymbol if m.isConstructor && m.owner == symbol =>
          (m, m.paramss.flatten.map(_.typeSignature.typeSymbol.fullName))
      }
      .collect { case (m, param) if filter(param) => (classMirror.reflectConstructor(m), param) }
  else Nil // either object only or class or trait
}

trait Baz {
  def doit() = println(getConstructors(getClass, _ => true))

}
class Foo(aaa : Int, bbb : Int)(ccc : Bar, ddd : Bar) extends Baz {
//  println(getClass.getDeclaredFields.length)
//  println(getClass.getConstructors.head.getParameters.map(e => e.getName).mkString(", "))
//  println(getClass.getConstructors.head.getParameterTypes.map(e => e.getName).mkString(", "))
//  println(getClass.getConstructors.head.getParameters.head.getDeclaringExecutable.getParameters.length)
  println(getClass.getConstructors.head.getParameters.apply(2).getDeclaringExecutable)
  println(getClass.getConstructors.head.getParameters.apply(3).getDeclaringExecutable)
//  val m = ru.runtimeMirror(getClass.getClassLoader)
//  val im=m.reflect(ccc)
  doit()
}

val z = new Bar{}
new Foo(1, 5)(z, new Bar{})

//I have a code structure similar to the following. Is this legal?
//```scala
//class Enum {
//  type Entry <: EnumEntry
//}
//object GoodEnum extends Enum {
//  sealed trait Entry
//  case object Nice extends Entry
//  case object Tasty extends Entry
//  case object Fun extends Entry
//}
//object BadEnum extends Enum {
//  sealed trait Entry
//  case object Rude extends Entry
//  case object Yucky extends Entry
//  case object Boring extends Entry
//}
//
//trait AnyBox {
//  type T <: AnyBox
//  def ===(that : T) : Boolean
//}
//trait EnumBox[E <: Enum] extends AnyToken {
//}
//object EnumBox {
//  trait Unbounded {
//    type T =  EnumToken[E]
//    def ===(that : T) : Boolean = ???
//    def == (that : E#Entry) : Boolean = ???
//
//  }
//}
//```