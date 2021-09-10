package DFiant.internals
import scala.quoted.*
extension [T](t: T)
  def debugPrint: T =
    println(t)
    t

transparent inline def showTree[T](inline arg: T): Unit = ${
  showTreeMacro[T]('arg)
}
def showTreeMacro[T](arg: Expr[T])(using Quotes, Type[T]): Expr[Unit] =
  import quotes.reflect.*
  val Inlined(_, _, term) = arg.asTerm
  println(term.show)
  println(term)
  println(TypeRepr.of[T].show)
  println(term.tpe.show)
  '{}

extension [T](t: Iterable[T])
  def mkStringBrackets: String = t.mkString("(", ", ", ")")
  def allElementsAreEqual: Boolean = t.forall(_ == t.head)

extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def asTypeOf[T]: Type[T] =
    import quotes.reflect.*
    tpe.asType.asInstanceOf[Type[T]]
  def asTypeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    tpe.asType match
      case '[t] =>
        TypeTree.of[t]

trait PrintType[T]
object PrintType:
  inline given [T]: PrintType[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[PrintType[T]] =
    import quotes.reflect.*
    println(TypeRepr.of[T])
    '{ new PrintType[T] {} }
