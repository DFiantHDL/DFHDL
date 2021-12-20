package DFiant.internals
import scala.quoted.*
import scala.annotation.tailrec
extension [T](t: T)
  def debugPrint: T =
    println(t)
    t

transparent inline def showTree[T](inline arg: T): Unit = ${
  showTreeMacro[T]('arg)
}

def errorExpr(msg: String)(using Quotes): Expr[Nothing] =
  '{ compiletime.error(${ Expr(msg) }) }

def showTreeMacro[T](arg: Expr[T])(using Quotes, Type[T]): Expr[Unit] =
  import quotes.reflect.*
  val Inlined(_, _, term) = arg.asTerm
  println(term.show)
  println(term)
  println(TypeRepr.of[T].show)
  println(term.tpe.show)
  '{}

extension [T](t: Iterable[T])(using CanEqual[T, T])
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
  def stripValueOf: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    tpe.asType match
      case '[ValueOf[t]] => TypeRepr.of[t]
      case _             => tpe
end extension

extension (using quotes: Quotes)(lhs: quotes.reflect.TypeRepr)
  def getTupleArgs: List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    @tailrec def recur(tpl: TypeRepr, args: List[TypeRepr]): List[TypeRepr] =
      tpl.asTypeOf[Any] match
        case '[head *: tail] =>
          recur(TypeRepr.of[tail], TypeRepr.of[head] :: args)
        case '[EmptyTuple] => args.reverse
        case '[t] =>
          report.errorAndAbort(s"Expecting a tuple, but found ${Type.show[t]}")
    if (lhs.show == "null")
      report.errorAndAbort(s"Expecting a tuple, but found null")
    else
      recur(lhs, Nil)
end extension

trait PrintType[T]
object PrintType:
  inline given [T]: PrintType[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[PrintType[T]] =
    import quotes.reflect.*
    println(Type.show[T])
    '{ new PrintType[T] {} }

object Error:
  transparent inline def call[T <: NonEmptyTuple]: Nothing = ${ macroImpl[T] }
  def macroImpl[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[Nothing] =
    import quotes.reflect.*
    val msg =
      TypeRepr
        .of[T]
        .getTupleArgs
        .map(_.dealias)
        .map {
          case ConstantType(StringConstant(msg)) => msg
          case t                                 => t.show
        }
        .mkString
    '{ compiletime.error(${ Expr(msg) }) }
end Error

extension (using quotes: Quotes)(sc: Expr[StringContext])
  def termWithArgs(args: Expr[Seq[Any]]): quotes.reflect.Term =
    import quotes.reflect.*
    val argsExprs = args match
      case Varargs(argsExprs) => argsExprs
    val '{ StringContext.apply($parts*) } = sc
    val partsExprs = parts match
      case Varargs(argsExprs) => argsExprs
    val fullTermParts =
      Seq(partsExprs, argsExprs)
        .flatMap(_.zipWithIndex)
        .sortBy(_._2)
        .map(_._1.asTerm)
    fullTermParts.reduce[Term] {
      case (Literal(StringConstant(l)), Literal(StringConstant(r))) =>
        Literal(StringConstant(l + r))
      case (l, r) =>
        '{ ${ l.asExpr }.toString + ${ r.asExpr }.toString }.asTerm
    }

inline implicit def fromValueOf[T](v: ValueOf[T]): T = v.value

type <:![T <: UB, UB] <: UB = T match
  case UB => T
