package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.whitebox


sealed class MustBeTheClassOf[T]
object MustBeTheClassOf {
  implicit def ev[T] : MustBeTheClassOf[T] = macro evMacro[T]
  def evMacro[T](c: whitebox.Context)(implicit n : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val tp = weakTypeOf[T]
    @tailrec def explore(tree : c.Tree): Boolean = {
      tree match {
        case Select(This(_), _) =>
          tree.symbol.fullName == tp.typeSymbol.fullName
        case Apply(tree, _) => explore(tree)
        case Select(tree, _) => explore(tree)
        case New(tree) => explore(tree)
        case Super(This(_), _) =>
          val SuperType(_, t) = tree.tpe
          t.typeSymbol.fullName == tp.typeSymbol.fullName
        case TypeApply(tree, _) => explore(tree)
        case t@TypeTree() => t.symbol.fullName == tp.typeSymbol.fullName
        case _ => false
      }
    }
    if (explore(c.enclosingImplicits.last.tree))  q"new DFiant.internals.MustBeTheClassOf[$tp]"
    else c.abort(c.enclosingPosition, "Wrong class symbol")
  }
}