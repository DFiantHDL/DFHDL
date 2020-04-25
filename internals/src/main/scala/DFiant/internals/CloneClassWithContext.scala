package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

trait CloneClassWithContext[T] {
  def apply(arg : T) : Any
}
object CloneClassWithContext {
  implicit def ev[T] : CloneClassWithContext[T] = macro evMacro[T]
  def evMacro[T](c: whitebox.Context)(implicit n : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val tp = weakTypeOf[T]
    val lastTree = c.enclosingImplicits.last.tree
    val implicitArgsTpe = lastTree.symbol.asMethod.paramLists.last.map(e => e.typeSignature)
    val implicitArgsTree = implicitArgsTpe.map {
      case tpe if tp <:< tpe => q"arg"
      case tpe => q"implicitly[$tpe]"
    }
    val exploredTree = lastTree match {
      case q"${Select(Super(This(_),_),_)}(...$paramValueTrees)" => //Anonymous class
        val ClassInfoType(tpList,_,_)= c.internal.enclosingOwner.owner.typeSignature
        val classList = tpList.zipWithIndex.map {
          case (t, 0) => q"$t(...$paramValueTrees)(..$implicitArgsTree)"
          case (t, _) => q"$t"
        }
        Some(q"new ..$classList {}")
      case t @ q"${TypeApply(_,_)}(...$_)" =>
        None
      case t @ q"${Select(Ident(TermName(n)),_)}(...$_)" if n.startsWith("stabilizer$") =>
        None
      case t @ q"$_(...$_)" => Some(q"$t(..$implicitArgsTree)")
      case _ => None
    }
    val genTree = exploredTree match {
      case Some(tree) =>
        q"""
        new DFiant.internals.CloneClassWithContext[$tp] {
          def apply(arg : $tp) : Any = {
            $tree
          }
        }
       """
      case _ =>
        //        println("Unknown", showRaw(lastTree))
        q"""
        new DFiant.internals.CloneClassWithContext[$tp] {
          def apply(arg : $tp) : Any = ???
        }
       """
    }
//        println(genTree)
    genTree
  }
}