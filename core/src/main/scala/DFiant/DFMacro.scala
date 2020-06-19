package DFiant

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation

object DFMacro {
  def dfImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$stats }" :: tail =>
          val targs = tparams.map(t => tq"Nothing") //TODO: needs to fix for the general abstract type
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$targs]]) extends ..$parents {$self => ..$stats
          }; ..$tail"""
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" :: Nil =>
          q"$mods def $tname[..$tparams](...$paramss)(implicit ctx : DFBlock.Context) : $tpt = $expr"
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes")
      }
    }
    c.Expr[Any](result)
  }
  def dfdsnImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends DFDesign with ..$parents { $self => ..$stats }" :: tail =>
          val targs = tparams.map(t => tq"Nothing")
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$targs]]) extends DFDesign with ..$parents{$self => ..$stats
          }; ..$tail"""
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { $self => ..$stats }" :: tail =>
          val targs = tparams.map(t => tq"Nothing")
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$targs]]) extends DFDesign {$self => ..$stats
          }; ..$tail"""
        case _ => c.abort(c.enclosingPosition, "Annotation @df can be used only with classes")
      }
    }
    c.Expr[Any](result)
  }
  def dfifcImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends DFInterface with ..$parents { $self => ..$stats }" :: tail =>
          val targs = tparams.map(t => tq"Nothing")
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$targs]]) extends DFInterface with ..$parents{$self => ..$stats
          }; ..$tail"""
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { $self => ..$stats }" :: tail =>
          val targs = tparams.map(t => tq"Nothing")
          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ctx : ContextOf[$tpname[..$targs]]) extends DFInterface {$self => ..$stats
          }; ..$tail"""
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