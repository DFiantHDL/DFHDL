package DFiant.basiclib.macros

import DFiant.core.DFAny

object GenVarWithVal {
  import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  def macroImpl[Val <: DFAny : c.WeakTypeTag, Var : c.WeakTypeTag](c : Context)() : c.Expr[Val] = {
    import c.universe._
    val weakVal = weakTypeOf[Val]
    val sym = symbolOf[Var]
    val valTree = tq"$weakVal"
    val appliedTree = tq"$sym[$weakVal]"
    val list = List(appliedTree, valTree)
    val className = c.freshName()
    val classType = TypeName(className)
    val classTerm = TermName(className)
    val genTree = q"""
        case class $classType() extends ..$list {
          def newEmptyDFVar = copy().asInstanceOf[TVar]
        }
        $classTerm()
      """
    c.Expr(genTree)
  }
  def apply[Val <: DFAny, Var]() : Val = macro macroImpl[Val, Var]

}
