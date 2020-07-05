package DFiant.internals

import scala.reflect.macros.whitebox

trait ClassArgs[T] {
  val value : List[List[(String, Any)]]
}
object ClassArgs {
  def empty : ClassArgs[Any] = new ClassArgs[Any] {
    val value : List[List[(String, Any)]] = List()
  }
  implicit def ev[T] : ClassArgs[T] = macro evMacro[T]
  def evMacro[T](c: whitebox.Context)(implicit n : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val tp = weakTypeOf[T]
    val lastTree = tp match {
      case ConstantType(Constant(_ : String)) => q""
      case _ => c.enclosingImplicits.last.tree
    }
    val genTree = lastTree match {
      case q"new $_(...$paramValueTrees)" =>
        val paramSymbols = symbolOf[T].asClass.primaryConstructor.asMethod.paramLists

        val params = (paramSymbols lazyZip paramValueTrees).map{
          case (l, r : List[_]) => (l lazyZip r).map{
            case (sym, valueTree) => q"(${sym.name.toString}, $valueTree)"
          }
        }

        q"""
        new DFiant.internals.ClassArgs[$tp] {
          val value = $params
        }
       """
      case _ =>
        q"""
        new DFiant.internals.ClassArgs[$tp] {
          val value = List()
        }
       """
    }

//        println(genTree)
    genTree
  }
}