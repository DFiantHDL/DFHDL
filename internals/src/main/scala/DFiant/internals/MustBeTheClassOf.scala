package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

sealed class MustBeTheClassOf[T]
object MustBeTheClassOf {
  implicit def ev[T]: MustBeTheClassOf[T] = macro evMacro[T]
  def evMacro[T](c: whitebox.Context)(implicit n: c.WeakTypeTag[T]): c.Tree = {
    import c.universe._
    val tp = weakTypeOf[T]
    val (isLiteralName, compared) = tp match {
      case ConstantType(Constant(s: String)) => (true, s)
      case _                                 => (false, tp.typeSymbol.fullName)
    }
    @tailrec def exploreTree(tree: c.Tree): Boolean = {
      tree match {
        case Select(This(_), _) =>
          tree.symbol.fullName == compared
        case Apply(Select(sup @ Super(next @ This(_), _), _), _) =>
          val SuperType(_, t) = sup.tpe
          if (t.typeSymbol.fullName == compared) true
          else exploreTree(next)
        case Apply(Select(New(t), _), _) => t.symbol.fullName == compared
        case Apply(Select(t, _), _)      => t.symbol.fullName == compared
        case Select(next, _)             => exploreTree(next)
        case This(_)                     => tree.symbol.fullName == compared
        case TypeApply(tree, _)          => exploreTree(tree)
        case t @ TypeTree()              => t.symbol.fullName == compared
        case t @ Ident(_)                => t.symbol.fullName == compared
        case _ =>
          false
      }
    }
    @tailrec def exploreOwner(owner: Symbol): Boolean = {
      if (owner.isMethod && owner.name.toString == compared) true
      else if (owner.isPackage) false
      else exploreOwner(owner.owner)
    }

    val owner = c.internal.enclosingOwner.owner
    val ownerOK = if (isLiteralName) {
      c.enclosingImplicits.last.tree match {
        case Select(_, TermName(s)) if s == compared => true
        case _                                       => exploreOwner(c.internal.enclosingOwner)
      }
    } else if (owner.isClass) {
      //           anonymous class                         object
      if (owner.name.toString.startsWith("$anon") || owner.isModuleClass)
        owner.asClass.baseClasses match {
          case _ :: parent :: _ =>
            parent.fullName == compared //get the name of the base class
          case _ => false
        }
      else owner.fullName == compared //any other class
    } else false

    val ok = {
      if (ownerOK) true
      else if (!isLiteralName) exploreTree(c.enclosingImplicits.last.tree)
      else false
    }
//    println(showRaw(c.enclosingImplicits.last.tree), owner, "compared to", tp, "got", ok)

    if (ok) q"new DFiant.internals.MustBeTheClassOf[$tp]"
    else {
      val errorMsg =
        if (isLiteralName) "Definition call doesn't match literal name"
        else "Wrong class symbol"
      c.abort(c.enclosingPosition, errorMsg)
    }
  }
}
