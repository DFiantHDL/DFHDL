package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.{tpd, untpd}
import StdNames.nme
import Names.*
import Types.*
import Constants.Constant
import dotty.tools.dotc.ast.tpd.Tree
import annotation.tailrec
import scala.language.implicitConversions

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived

abstract class CommonPhase extends PluginPhase:

  import tpd._

  val debugFilter: String => Boolean = _ => false
  var pluginDebugSource: String = ""

  def debug(str: => Any*): Unit =
    if (debugFilter(pluginDebugSource)) println(str.mkString(", "))

  protected def mkSome(tree: Tree)(using Context): Tree =
    ref(requiredMethod("scala.Some.apply"))
      .appliedToType(tree.tpe)
      .appliedTo(tree)

  protected def mkList(tree: List[Tree])(using Context): Tree =
    tpd.mkList(tree, TypeTree(tree.head.tpe.widen))

  protected def mkTuple(trees: List[Tree])(using Context): Tree =
    ref(requiredMethod(s"scala.Tuple${trees.length}.apply"))
      .appliedToTypes(trees.map(_.tpe.widen))
      .appliedToArgs(trees)

  var metaContextTpe: TypeRef = _
  var metaContextCls: ClassSymbol = _
  var positionCls: ClassSymbol = _
  var hasDFCTpe: TypeRef = _
  extension (clsSym: Symbol)
    def inherits(parentFullName: String)(using Context): Boolean =
      if (clsSym.isClass)
        clsSym.asClass.parentSyms.exists(ps =>
          ps.fullName.toString == parentFullName || ps.inherits(parentFullName)
        )
      else false

  extension (tpe: Type)(using Context)
    def simple: Type =
      tpe match
        case tr: TermRef        => tr.underlying.dealias
        case ann: AnnotatedType => ann.parent.simple
        case _                  => tpe.dealias

  extension (srcPos: util.SrcPos)(using Context)
    def show: String =
      val pos = srcPos.startPos
      val endPos = srcPos.endPos
      s"${pos.source.path}:${pos.line}:${pos.column}-${endPos.line}:${endPos.column}"

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

    def at(tree: DefDef | TypeDef)(using Context): Option[Tree] =
      tree match
        case tree: DefDef =>
          tree.paramss.flatten.view.reverse.collectFirst {
            case a @ ValDef(name, _, _) if a.tpe <:< metaContextTpe =>
              untpd.Ident(name).withType(a.tpe)
          }
        case TypeDef(name, _: Template) if tree.tpe <:< hasDFCTpe =>
          Some(This(tree.symbol.asClass).select("dfc".toTermName))
        case _ => None
  end ContextArg

  extension (srcPos: util.SrcPos)(using Context)
    def positionTree: Tree =
      val fileNameTree = Literal(Constant(srcPos.startPos.source.path))
      val lineStartTree = Literal(Constant(srcPos.startPos.line + 1))
      val columnStartTree = Literal(Constant(srcPos.startPos.column + 1))
      val lineEndTree = Literal(Constant(srcPos.endPos.line + 1))
      val columnEndTree = Literal(Constant(srcPos.endPos.column + 1))
      New(
        positionCls.typeRef,
        fileNameTree :: lineStartTree :: columnStartTree :: lineEndTree :: columnEndTree :: Nil
      )
  end extension

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
    pluginDebugSource = tree.source.path.toString
    metaContextTpe = requiredClassRef("dfhdl.internals.MetaContext")
    metaContextCls = requiredClass("dfhdl.internals.MetaContext")
    positionCls = requiredClass("dfhdl.internals.Position")
    hasDFCTpe = requiredClassRef("dfhdl.core.HasDFC")
    if (debugFilter(tree.source.path.toString))
      println(
        s"""===============================================================
           |Before: $phaseName
           |===============================================================
           |""".stripMargin
      )
      println(tree.showSummary(14))

    ctx
  end prepareForUnit

  override def transformUnit(tree: Tree)(using Context): Tree =
    pluginDebugSource = ""
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
