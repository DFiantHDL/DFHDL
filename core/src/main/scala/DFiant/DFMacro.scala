package DFiant

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation

object DFMacro {
  def dfImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$stats }" :: Nil => {
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$tparams]]) extends ..$parents {$self => ..$stats
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes")
      }
    }
    c.Expr[Any](result)
  }
  def dfdsnImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends DFDesign with ..$parents { $self => ..$stats }" :: Nil => {
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$tparams]]) extends DFDesign with ..$parents{$self => ..$stats
          }"""
        }
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { $self => ..$stats }" :: Nil => {
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$tparams]]) extends DFDesign {$self => ..$stats
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes")
      }
    }
    c.Expr[Any](result)
  }
  def dfifcImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends DFInterface with ..$parents { $self => ..$stats }" :: Nil => {
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$tparams]]) extends DFInterface with ..$parents{$self => ..$stats
          }"""
        }
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { $self => ..$stats }" :: Nil => {
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$tparams]]) extends DFInterface {$self => ..$stats
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes")
      }
    }
    c.Expr[Any](result)
  }
}

class df extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DFMacro.dfImpl
}

class dfdsn extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DFMacro.dfdsnImpl
}

class dfifc extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DFMacro.dfifcImpl
}