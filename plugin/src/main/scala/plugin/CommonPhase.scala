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
import scala.compiletime.uninitialized
import scala.annotation.targetName

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

  protected def mkOption(optTree: Option[Tree])(using Context): Tree =
    optTree.map(mkSome).getOrElse(mkNone)

  protected def mkList(tree: List[Tree], tpeOpt: Option[Type] = None)(using Context): Tree =
    if (tree.isEmpty) ref(defn.NilModule)
    else
      val tpe = tpeOpt.getOrElse(tree.view.map(_.tpe).reduce(_ | _).widenUnion)
      tpd.mkList(tree, TypeTree(tpe))

  protected def mkTuple(trees: List[Tree])(using Context): Tree =
    ref(requiredMethod(s"scala.Tuple${trees.length}.apply"))
      .appliedToTypes(trees.map(_.tpe.widen))
      .appliedToArgs(trees)

  private val dropProxiesTreeMap = new TreeMap:
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      super.transform(tree) match
        case block: Block =>
          val proxyMap = block.stats.collect {
            case vd: ValDef if vd.symbol.is(Synthetic) => vd.name -> vd.rhs
          }.toMap
          block.expr match
            case Apply(fun, proxies) =>
              Apply(
                fun,
                proxies.map {
                  case Ident(n)              => proxyMap(n.toTermName)
                  case NamedArg(x, Ident(n)) => NamedArg(x, proxyMap(n.toTermName))
                  case x                     => x
                }
              )
            case tree => tree
        case tree => tree

  protected def dropProxies(tree: Tree)(using Context): Tree =
    dropProxiesTreeMap.transform(tree)

  var metaContextTpe: TypeRef = uninitialized
  var metaContextCls: ClassSymbol = uninitialized
  var metaContextIgnoreAnnotSym: ClassSymbol = uninitialized
  var metaContextForwardAnnotSym: ClassSymbol = uninitialized
  var metaGenSym: Symbol = uninitialized
  var positionGenSym: TermSymbol = uninitialized
  var contextFunctionSym: Symbol = uninitialized
  var hasDFCTpe: TypeRef = uninitialized
  var inlineAnnotSym: Symbol = uninitialized
  var dfValSym: Symbol = uninitialized
  var constModTpe: Type = uninitialized
  var genDesignParamSym: TermSymbol = uninitialized

  extension (tree: TypeDef)
    def hasDFC(using Context): Boolean =
      (tree.tpe <:< hasDFCTpe) // && (dfSpecTpe == NoType || !(tree.tpe <:< dfSpecTpe))

  // replacing the old arg references according to the argument map
  def replaceArgs(expr: Tree, argMap: Map[Symbol, Tree])(using Context): Tree =
    val replacer = new TreeMap():
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case id: (Select | Ident) if argMap.contains(id.symbol) =>
            argMap(id.symbol)
          case _ =>
            super.transform(tree)
    replacer.transform(expr)
  end replaceArgs

  extension (tpe: Type)(using Context)
    def dfValTpeOpt: Option[Type] =
      tpe.dealias match
        case res if res.dealias.typeSymbol == dfValSym => Some(res)
        case _                                         => None
    def isDFConst: Boolean =
      tpe.widenDealias match
        case AppliedType(_, _ :: modifierTpe :: Nil) =>
          modifierTpe.dealias match
            case AppliedType(_, _ :: _ :: _ :: pTpe :: Nil) =>
              pTpe =:= constModTpe
            case _ => false
        case _ =>
          false
  end extension

  extension (tree: ValOrDefDef)(using Context)
    def dfValTpeOpt: Option[Type] =
      tree.tpt.tpe.dfValTpeOpt

  extension (tree: ValOrDefDef)(using Context)
    def genMeta: Tree =
      val nameOptTree = mkOptionString(Some(tree.name.toString.nameCheck(tree)))
      val positionTree = tree.srcPos.positionTree
      val docOptTree = mkOptionString(tree.symbol.docString)
      val annotTree = mkList(tree.symbol.annotations.map(_.tree))
      ref(metaGenSym).appliedToArgs(
        nameOptTree :: positionTree :: docOptTree :: annotTree :: Nil
      )
  end extension

  extension (v: ValDef)(using Context)
    def genDesignParamValDef(dfcTree: Tree): ValDef =
      val meta = v.genMeta
      val paramGen =
        ref(genDesignParamSym)
          .appliedToType(v.tpt.tpe)
          .appliedToArgs(List(ref(v.symbol), meta))
          .appliedTo(dfcTree)
      val uniqueName = NameKinds.UniqueName.fresh(s"${v.name}_plugin".toTermName)
      val flags: FlagSet = if (ctx.owner.isConstructor) Private else EmptyFlags
      SyntheticValDef(uniqueName, paramGen, flags)

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
      extension (c: Context)
        def docCtx: Option[Comments.ContextDocstrings] = c.property(Comments.ContextDoc)
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
      def extractParamDescription(docstring: String, paramName: String): Option[String] =
        val pattern = (s"@param\\s+$paramName\\s+([^@]*)").r
        pattern.findFirstMatchIn(docstring) match
          case Some(m) => Some(m.group(1).trim)
          case None    => None
      if (sym.is(Param))
        sym.owner.docString.flatMap(d => extractParamDescription(d, sym.name.toString))
      else if (sym.isConstructor)
        sym.owner.docString
      else ctx.docCtx.flatMap(_.docstring(sym)).map(_.raw).map(extract)
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
  end extension

  // custom replacement for compiler defn.ContextFunctionType
  object ContextFunctionType:
    def unapply(tp: Type)(using Context): Option[(List[Type], Type)] =
      defn.asContextFunctionType(tp) match
        case tp1 if tp1.exists =>
          val args = tp1.dropDependentRefinement.argInfos
          Some((args.init, args.last))
        case _ => None

  extension (tree: ValOrDefDef)(using Context)
    def isInline: Boolean =
      val sym = tree.symbol
      sym.is(Inline) || sym.hasAnnotation(inlineAnnotSym)

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
        case ContextFunctionType(ctx, res) if ctx.head <:< metaContextTpe => Some(res)
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
        case ContextFunctionType(ctx, res) if ctx.head <:< metaContextTpe =>
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
        ref(positionGenSym).appliedTo(
          fileNameTree, lineStartTree, columnStartTree, lineEndTree, columnEndTree
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
    metaGenSym = requiredMethod("dfhdl.compiler.ir.Meta.gen")
    positionGenSym = requiredMethod("dfhdl.internals.Position.fromAbsPath")
    hasDFCTpe = requiredClassRef("dfhdl.core.HasDFC")
    inlineAnnotSym = requiredClass("scala.inline")
    constModTpe = requiredClassRef("dfhdl.core.ISCONST").appliedTo(ConstantType(Constant(true)))
    contextFunctionSym = defn.FunctionSymbol(1, isContextual = true)
    genDesignParamSym = requiredMethod("dfhdl.core.r__For_Plugin.genDesignParam")
    if (debugFilter(tree.source.path.toString))
      println(
        s"""===============================================================
           |Before: $phaseName
           |===============================================================
           |""".stripMargin
      )
      println(tree.show)

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
      println(tree.show)
    tree
end CommonPhase
