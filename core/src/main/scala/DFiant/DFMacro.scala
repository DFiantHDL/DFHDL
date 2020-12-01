package DFiant

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation

protected[DFiant] object DFMacro {
  def dfImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result =
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ..$iparams) extends ..$parents { $self => ..$stats }" :: tail =>
          val targs = tparams.map {
            case t : TypeDef => tq"Nothing"//Ident(t.name)
          }
          val iparamsWithCtx = iparams :+ q"private val ctx : ContextOf[$tpname[..$targs]]"
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ..$iparamsWithCtx) extends ..$parents {$self => ..$stats
          }; ..$tail"""
        case q"$mods def $tname[..$tparams](...$paramss)(implicit ..$iparams): $tpt = $expr" :: Nil =>
          val cn = c.internal.constantType(Constant(tname.toString()))
          val q"new df(...$dfParams)" = c.prefix.tree

          val fsm = tpt match {
            case tq"$_.$ident" if ident.toString() == "FSM" => true
            case tq"$ident" if ident.toString() == "FSM" => true
            case _ => false
          }
          val withScope = dfParams match {
            case List(List(Literal(Constant(withScope : Boolean)))) => withScope
            case _ => !fsm
          }
          val defdfTree = if (withScope) q"defdf{$expr}" else expr
          val ctxTree =
            if (fsm) List(q"ctx : DFBlock.Context", q"ta: FSM.HasFSMAscription")
            else List(q"ctx : ContextOf[$cn]")
          val iparamsWithCtx = iparams ++ ctxTree
          q"$mods def $tname[..$tparams](...$paramss)(implicit ..$iparamsWithCtx) : $tpt = $defdfTree"
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes or definitions")
      }
    c.Expr[Any](result)
  }
}

/**
  * Adds an implicit context declaration to the annotated design/interface/definition.
  *
  * @note Such a context is a must.
  *
  * - For `class X` it adds `implicit ctx : ContextOf[X]`
  *
  * - For `def x` it adds `implicit ctx : ContextOf["x"]`
  *
  * @param withScope When true for definitions, it creates an additional scope.
  */
class df(withScope : Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DFMacro.dfImpl
}


