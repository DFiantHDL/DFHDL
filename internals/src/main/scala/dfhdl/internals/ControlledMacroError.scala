package dfhdl.internals
import scala.quoted.*

object ControlledMacroError:
  // if contains a key, it means to activate error control.
  // if the value is empty (by default), it means the implicit given is found.
  // if the value is not empty, it means the implicit given with the error message.
  private val macroAbortPositionError = collection.concurrent.TrieMap.empty[String, String]
  private val compiletimeErrorPositionError = collection.concurrent.TrieMap.empty[String, String]
  private def getKey(using Quotes): String =
    import quotes.reflect.*
    Position.ofMacroExpansion.toString
  def activate()(using Quotes): Unit = macroAbortPositionError += getKey -> ""
  def deactivate()(using Quotes): Unit =
    compiletimeErrorPositionError.remove(getKey)
    macroAbortPositionError.remove(getKey)
  def getLastMacroAbortError(using Quotes): String = macroAbortPositionError.getOrElse(getKey, "")
  def getLastCompiletimeError(using Quotes): String =
    compiletimeErrorPositionError.getOrElse(getKey, "")
  def report(msg: String)(using Quotes): Expr[Nothing] =
    import quotes.reflect.report as macroReport
    val key = getKey
    macroAbortPositionError.get(key) match
      case Some("") =>
        macroAbortPositionError += key -> msg
        macroReport.errorAndAbort(msg)
      case Some(existingMsg) =>
        macroReport.errorAndAbort(existingMsg)
      case _ =>
        compiletimeErrorPositionError += key -> msg
        '{ compiletime.error(${ Expr(msg) }) }
  def getLastErrorInExpr[T](expr: Expr[T])(using Quotes, Type[T]): Option[String] =
    import quotes.reflect.*
    def searchList(trees: List[Tree]): Option[String] =
      trees.view.flatMap(search).headOption
    def search(tree: Tree): Option[String] =
      tree match
        case Inlined(_, _, inner)       => search(inner)
        case Typed(inner, _)            => search(inner)
        case Select(inner, _)           => search(inner)
        case TypeApply(fun, _)          => search(fun)
        case Block(stats, expr)         => searchList(expr :: stats)
        case DefDef(_, _, _, Some(rhs)) => search(rhs)
        case ValDef(_, _, Some(rhs))    => search(rhs)
        case ClassDef(_, _, _, _, body) => searchList(body)
        case Apply(
              Select(Select(Ident("compiletime"), "package$package"), "error"),
              List(Inlined(_, _, Literal(StringConstant(msg))))
            ) =>
          Some(msg)
        case Apply(fun, args) => searchList(fun :: args)
        case _                => None
    if (getLastCompiletimeError.nonEmpty) search(expr.asTerm)
    else None
  end getLastErrorInExpr
end ControlledMacroError

extension [T](expr: Expr[T])(using Quotes, Type[T])
  def mapExprOrError[R](f: Expr[T] => Expr[R]): Expr[R] =
    ControlledMacroError.getLastErrorInExpr(expr) match
      case Some(msg) => '{ compiletime.error(${ Expr(msg) }) }
      case None      => f(expr)
  end mapExprOrError
