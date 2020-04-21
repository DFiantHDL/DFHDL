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
    @tailrec def explore(tree : Tree, prevArgs : List[List[Tree]]): Option[Tree] = {
      tree match {
        case Apply(Select(Super(This(_), _), _), argList) =>
          val ClassInfoType(tpList,_,_)= c.internal.enclosingOwner.owner.typeSignature
          val classList = tpList.zipWithIndex.map {
            case (t, 0) =>
              val appliedArgsTree = (argList :: prevArgs).foldLeft(q"$t")((appliedTree, args) => q"$appliedTree(..$args)")
              q"$appliedArgsTree(arg)"
            case (t, _) => q"$t"
          }
          Some(q"new ..$classList {}")
        case Apply(Select(_, _), _) =>
          val appliedArgsTree = prevArgs.foldLeft(tree)((appliedTree, args) => q"$appliedTree(..$args)")
          Some(q"$appliedArgsTree(arg)")
        case Apply(tree @ Apply(_,_), argList) => explore(tree, argList :: prevArgs)
        case _ =>
          None
      }
    }
    val genTree = explore(lastTree, List()) match {
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
    //    println(genTree)
    genTree
  }
}