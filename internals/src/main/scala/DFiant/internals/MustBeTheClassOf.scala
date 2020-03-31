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
        case Apply(next, _) => explore(next)
        case Select(next, _) => explore(next)
        case New(next) => explore(next)
        case Super(next@This(_), _) =>
          val SuperType(_, t) = tree.tpe
          if (t.typeSymbol.fullName == tp.typeSymbol.fullName) true
          else explore(next)
        case This(_) => tree.symbol.fullName == tp.typeSymbol.fullName
        case TypeApply(tree, _) => explore(tree)
        case t@TypeTree() => t.symbol.fullName == tp.typeSymbol.fullName
        case t@Ident(_) => t.symbol.fullName == tp.typeSymbol.fullName
        case _ =>
          false
      }
    }
    val ok =
      if (c.internal.enclosingOwner.owner.isModuleClass) true
      else explore(c.enclosingImplicits.last.tree)
//    println(showRaw(c.enclosingImplicits.last.tree), "compared to", tp, "got", ok)

    if (ok)  q"new DFiant.internals.MustBeTheClassOf[$tp]"
    else c.abort(c.enclosingPosition, "Wrong class symbol")
  }
}