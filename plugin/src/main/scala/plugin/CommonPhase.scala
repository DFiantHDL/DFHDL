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

  protected def mkOptionString(argOpt: Option[String])(using Context): Tree =
    argOpt match
      case Some(str) =>
        New(
          defn.SomeClass.typeRef.appliedTo(defn.StringType),
          Literal(Constant(str)) :: Nil
        )
      case None =>
        mkNone

  protected def mkSome(tree: Tree)(using Context): Tree =
    ref(requiredMethod("scala.Some.apply"))
      .appliedToType(tree.tpe)
      .appliedTo(tree)

  protected def mkNone(using Context): Tree =
    ref(defn.NoneModule.termRef)

  protected def mkList(tree: List[Tree])(using Context): Tree =
    if (tree.isEmpty) ref(defn.NilModule)
    else
      val tpe = tree.view.map(_.tpe).reduce(_ | _).widenUnion
      tpd.mkList(tree, TypeTree(tpe))

  protected def mkTuple(trees: List[Tree])(using Context): Tree =
    ref(requiredMethod(s"scala.Tuple${trees.length}.apply"))
      .appliedToTypes(trees.map(_.tpe.widen))
      .appliedToArgs(trees)

  var metaContextTpe: TypeRef = _
  var metaContextCls: ClassSymbol = _
  var metaContextIgnoreAnnotSym: ClassSymbol = _
  var metaContextForwardAnnotSym: ClassSymbol = _
  var positionCls: ClassSymbol = _
  var contextFunctionSym: Symbol = _
  var hasDFCTpe: TypeRef = _
  var inlineAnnotSym: Symbol = _
  var dfValSym: Symbol = _

  extension (sym: Symbol)
    def ignoreMetaContext(using Context): Boolean =
      sym.hasAnnotation(metaContextIgnoreAnnotSym)
    def forwardMetaContext(using Context): Boolean =
      sym.hasAnnotation(metaContextForwardAnnotSym)
    def inherits(parentFullName: String)(using Context): Boolean =
      if (sym.isClass)
        sym.asClass.parentSyms.exists(ps =>
          ps.fullName.toString == parentFullName || ps.inherits(parentFullName)
        )
      else false
    def docString(using Context): Option[String] =
      import Comments.CommentsContext
      def removeLastLineWhitespace(input: String): String =
        val lines = input.split("\n")
        if (lines.length <= 1) input
        else
          val lastIndex = lines.length - 1
          val lastLineWithoutWhitespace = lines(lastIndex).trim
          lines.slice(0, lastIndex).mkString("\n") + "\n" + lastLineWithoutWhitespace
      def extract(input: String): String =
        val pattern = """(?s)/\*(.*?)\*/""".r
        val extractedText = pattern.findFirstMatchIn(input).map(_.group(1)).getOrElse("")
        removeLastLineWhitespace(extractedText).stripMargin('*')
      end extract
      ctx.docCtx.flatMap(_.docstring(sym)).map(_.raw).map(extract)
    end docString
    def staticAnnotations(using Context): List[Annotations.Annotation] =
      sym.annotations.collect {
        case a if a.tree.tpe <:< defn.StaticAnnotationClass.typeRef => a
      }
  end extension

  extension (name: String)
    def nameCheck(posTree: Tree)(using Context): String =
      val finalName =
        posTree.symbol.getAnnotation(defn.TargetNameAnnot)
          .flatMap(_.argumentConstantString(0))
          .getOrElse(name)
      if (
        !finalName.matches("^[a-zA-Z0-9_]*$") && !posTree.symbol.flags.is(
          Flags.Synthetic
        )
      )
        report.error(
          s"""Unsupported DFHDL member name $finalName.
           |Only alphanumric or underscore characters are supported.
           |You can leave the Scala name as-is and add @targetName("newName") annotation.""".stripMargin,
          posTree.srcPos
        )
      finalName

  extension (tree: ValOrDefDef)(using Context)
    def isInline: Boolean =
      val sym = tree.symbol
      (sym is Inline) || sym.hasAnnotation(inlineAnnotSym)

  extension (tpe: Type)(using Context)
    def simple: Type =
      tpe match
        case tr: TermRef        => tr.underlying.dealias
        case ann: AnnotatedType => ann.parent.simple
        case _                  => tpe.dealias
    @tailrec private def flattenConsTuple(pastArgs: List[Type]): Type = tpe.stripAnnots match
      case emptyTuple if emptyTuple.typeSymbol == defn.EmptyTupleModule =>
        AppliedType(requiredClassRef(s"scala.Tuple${pastArgs.length}"), pastArgs.reverse)
      case AppliedType(tycon, head :: next :: Nil) if tycon.typeSymbol == defn.PairClass =>
        next.flattenConsTuple(head :: pastArgs)
      case _ =>
        tpe
    def flattenConsTuple: Type = tpe.flattenConsTuple(Nil)
  end extension
  extension (tp: Type)(using Context)
    def dfcFuncTpeOptRecur: Option[Type] =
      tp.dealias match
        case defn.ContextFunctionType(ctx, res, _) if ctx.head <:< metaContextTpe => Some(res)
        case AppliedType(tycon, args) =>
          var requiresUpdate = false
          val updatedArgs = args.map { tp =>
            tp.dfcFuncTpeOptRecur match
              case Some(tp) =>
                requiresUpdate = true
                tp
              case None => tp
          }
          if (requiresUpdate) Some(AppliedType(tycon, updatedArgs))
          else None
        case _ => None
    def dfcFuncTpeOpt: Option[Type] =
      tp.dealias match
        case defn.ContextFunctionType(ctx, res, _) if ctx.head <:< metaContextTpe =>
          Some(res)
        case _ => None
  end extension

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
      tree.symbol.hasAnnotation(requiredClass("dfhdl.internals.metaContextDelegate"))

  extension (tree: ValOrDefDef)(using Context)
    def ident: Tree =
      untpd.Ident(tree.name).withType(tree.tpe)

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
              a.ident
          }
        case TypeDef(name, _: Template) if tree.tpe <:< hasDFCTpe =>
          Some(This(tree.symbol.asClass).select("dfc".toTermName))
        case _ => None
  end ContextArg

  extension (srcPos: util.SrcPos)(using Context)
    def positionTree: Tree =
      if (srcPos.span == util.Spans.NoSpan) ref(requiredMethod("dfhdl.internals.Position.unknown"))
      else
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
    dfValSym = requiredClass("dfhdl.core.DFVal")
    metaContextTpe = requiredClassRef("dfhdl.internals.MetaContext")
    metaContextCls = requiredClass("dfhdl.internals.MetaContext")
    metaContextIgnoreAnnotSym = requiredClass("dfhdl.internals.metaContextIgnore")
    metaContextForwardAnnotSym = requiredClass("dfhdl.internals.metaContextForward")
    positionCls = requiredClass("dfhdl.internals.Position")
    hasDFCTpe = requiredClassRef("dfhdl.core.HasDFC")
    inlineAnnotSym = requiredClass("scala.inline")
    contextFunctionSym = defn.FunctionSymbol(1, isContextual = true)
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
