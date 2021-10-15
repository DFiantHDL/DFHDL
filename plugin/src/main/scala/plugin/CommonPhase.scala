package DFiant.plugin
import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.tpd
import StdNames.nme
import Names.*
import Types.*
import Constants.Constant
import dotty.tools.dotc.ast.tpd.Tree

import annotation.tailrec
import scala.language.implicitConversions

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived
given CanEqual[Tree, Tree] = CanEqual.derived

abstract class CommonPhase extends PluginPhase:
  import tpd._
  val debugFilter: String => Boolean = _ => false
  def pluginPrint(tree: Tree, str: Any): Unit =
    if (tree.source.path.toString.contains("DFDecimalSpec.scala"))
      println(str)
  var metaContextTpe: TypeRef = _
  extension (clsSym: Symbol)
    def inherits(parentFullName: String)(using Context): Boolean =
      if (clsSym.isClass)
        clsSym.asClass.parentSyms.exists(ps =>
          ps.fullName.toString == parentFullName || ps.inherits(parentFullName)
        )
      else false

  extension (tree: Apply)(using Context)
    def replaceArg(fromArg: Tree, toArg: Tree): Apply =
      var changed = false
      val repArgs = tree.args.map { a =>
        if (a eq fromArg)
          changed = true
          toArg
        else a
      }
      tree.fun match
        case apply: Apply if !changed =>
          Apply(apply.replaceArg(fromArg, toArg), tree.args)
        case _ =>
          Apply(tree.fun, repArgs)

  extension (tree: Apply)(using Context)
    def isContextDelegate: Boolean =
      tree.symbol.annotations.exists(
        _.symbol.name.toString == "metaContextDelegate"
      )

  object ContextArg:
    def unapply(tree: Tree)(using Context): Option[Tree] =
      tree match
        case Apply(tree, args) =>
          args
            .collectFirst {
              case a if a.tpe <:< metaContextTpe =>
                a
            }
            .orElse(unapply(tree))
        case _ => None

  object ApplyFunArgs:
    @tailrec private def recurUnapply(fun: Tree, args: List[List[Tree]])(using
        Context
    ): (Tree, List[List[Tree]]) =
      fun match
        case Apply(f, a) => recurUnapply(f, a :: args)
        case f           => (f, args)
    def unapply(tree: Apply)(using Context): Option[(Tree, List[List[Tree]])] =
      Some(recurUnapply(tree, Nil))
    def apply(fun: Tree, args: List[List[Tree]])(using Context): Apply =
      fun.appliedToArgss(args).asInstanceOf[Apply]

  override def prepareForUnit(tree: Tree)(using Context): Context =
    metaContextTpe = requiredClassRef(
      "DFiant.internals.MetaContext"
    )
    if (debugFilter(tree.source.path.toString))
      println(
        s"""===============================================================
           |Before: $phaseName
           |===============================================================
           |""".stripMargin
      )
      println(tree.showSummary(8))

    ctx
  end prepareForUnit

  override def transformUnit(tree: Tree)(using Context): Tree =
    if (debugFilter(tree.source.path.toString))
      println(
        s"""===============================================================
           |After: $phaseName
           |===============================================================
           |""".stripMargin
      )
      println(tree.showSummary(10))
    tree
end CommonPhase
