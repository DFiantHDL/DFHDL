package DFiant.fsm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation

object FSMMacro {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case (x @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr") :: Nil =>
          q"$mods def $tname[..$tparams](...$paramss)(implicit ctx : DFBlock.Context) : $tpt = $expr"
        case _ => c.abort(c.enclosingPosition, "Annotation @fsm can be used only with definitions")
      }
    }
    c.Expr[Any](result)
  }
}

class fsm extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro FSMMacro.impl
}