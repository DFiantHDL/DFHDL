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
import ast.tpd
import StdNames.nme
import Names.*
import Constants.Constant
import Types.*

import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable
import annotation.tailrec

class MetaContextGenPhase(setting: Setting) extends CommonPhase:
  import tpd._

  // override val debugFilter: String => Boolean = _.contains("Example.scala")
  val phaseName = "MetaContextGen"

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextDelegate")
  var setMetaSym: Symbol = uninitialized
  var setMetaAnonSym: Symbol = uninitialized
  var treeOwnerApplyMap = Map.empty[Apply, (MemberDef, util.SrcPos)]
  var treeOwnerApplyMapStack = List.empty[Map[Apply, (MemberDef, util.SrcPos)]]
  val treeOwnerOverrideMap = mutable.Map.empty[DefDef, (Tree, util.SrcPos)]
  val contextDefs = mutable.Map.empty[String, Tree]
  var clsStack = List.empty[TypeDef]
  var applyStack = List.empty[Apply]

  extension (tree: ValOrDefDef)(using Context)
    def needsNewContext: Boolean =
      tree match
        case _: ValDef => true // valdefs always generate new context
        case dd: DefDef =>
          val sym = tree.symbol
          // defdefs generate new context if they are not inline
          // and when they are not synthetic, indicating that they
          // are actually constructor definitions (other synthetics
          // should not have context, anyways), when they are not exported,
          // and when they don't have a context argument
          !tree.isInline && !sym.is(Synthetic) && !sym.is(Exported) &&
          ContextArg.at(dd).isEmpty

  class MetaInfo(
      val nameOpt: Option[String],
      val srcPos: util.SrcPos,
      val docOpt: Option[String],
      val annotations: List[Annotations.Annotation]
  )
  extension (tree: Tree)(using Context)
    def setMeta(metaInfo: MetaInfo): Tree =
      import metaInfo.*
      setMeta(nameOpt, srcPos, docOpt, annotations)
    def setMeta(
        nameOpt: Option[String],
        srcPos: util.SrcPos,
        docOpt: Option[String],
        annotations: List[Annotations.Annotation]
    ): Tree =
      if (nameOpt.nonEmpty)
        val nameOptTree = mkOptionString(nameOpt)
        val positionTree = srcPos.positionTree
        val docOptTree = mkOptionString(docOpt)
        val annotTree = mkList(annotations.map(_.tree))
        tree
          .select(setMetaSym)
          .appliedToArgs(
            nameOptTree :: positionTree :: docOptTree :: annotTree :: Nil
          )
          .withType(TermRef(tree.tpe, setMetaSym))
      else
        val positionTree = srcPos.positionTree
        tree
          .select(setMetaAnonSym)
          .appliedTo(positionTree)
          .withType(TermRef(tree.tpe, setMetaAnonSym))
    end setMeta
  end extension

  extension (sym: Symbol)
    def fixedFullName(using Context): String =
      sym.fullName.toString.replace("._$", ".")
  private def ignoreValDef(tree: ValDef)(using Context): Boolean =
    tree.name.toString match
      case inlinedName(prefix) =>
        tree.tpe match
          case x: TermRef =>
            x.underlying.dealias.typeSymbol.name.toString == prefix ||
            x.parents.exists(_.typeSymbol.name.toString == prefix)
          case _ => false
      case _ => false

  def getMetaInfo(ownerTree: Tree, srcPos: util.SrcPos)(using Context): Option[MetaInfo] =
    ownerTree match
      case t: ValOrDefDef if t.needsNewContext =>
        if (t.symbol.flags.is(Flags.Mutable))
          report.warning(
            "Scala `var` modifier for DFHDL values/classes is highly discouraged!\nConsider changing to `val`.",
            t.srcPos
          )
        val (nameOpt, docOpt, annots) =
          t match
            case vd: ValDef if vd.isEmpty || ignoreValDef(vd) => (None, None, Nil)
            case dd: DefDef                                   => (None, None, Nil)
            case _ =>
              (
                Some(t.name.toString.nameCheck(t)),
                t.symbol.docString,
                t.symbol.staticAnnotations
              )
        Some(MetaInfo(nameOpt, srcPos, docOpt, annots))
      case t: TypeDef if t.name.toString.endsWith("$") =>
        Some(
          MetaInfo(
            Some(t.name.toString.dropRight(1).nameCheck(t)),
            srcPos,
            t.symbol.docString,
            t.symbol.staticAnnotations
          )
        )
      case _ =>
        // no meta information
        None
    end match
  end getMetaInfo

  override def transformApply(tree: Apply)(using Context): Tree =
    val origApply = applyStack.head
    applyStack = applyStack.drop(1)
    if (
      tree.tpe.isParameterless && !tree.fun.symbol.ignoreMetaContext && !tree.fun.symbol.forwardMetaContext
    )
      tree match
        // found a context argument
        case ContextArg(argTree) =>
          treeOwnerApplyMap.get(origApply) match
            case Some(ownerTree, srcPos) =>
              getMetaInfo(ownerTree, srcPos) match
                case Some(metaInfo) =>
                  tree.replaceArg(argTree, argTree.setMeta(metaInfo))
                case None =>
                  val sym = argTree.symbol
                  contextDefs.get(sym.fixedFullName) match
                    case Some(ct) if !(ct.sameTree(ownerTree)) =>
                      report.error(
                        s"${ownerTree.symbol} is missing an implicit Context parameter",
                        ownerTree.symbol
                      )
                    case _ =>
                  // do nothing
                  tree
            // No owner tree found, so it's anonymous. Yet we may want to apply no new context
            // at all and just keep the propagated context.
            case None =>
              // keeping the propagated context
              if (tree.fun.symbol.name.toString.contains("$")) tree
              // generating a new anonymous context
              else tree.replaceArg(argTree, argTree.setMeta(None, origApply.srcPos, None, Nil))
          end match
        case _ => tree
    else tree
  end transformApply

  override def prepareForBlock(tree: Block)(using Context): Context =
    tree.stats match
      case _ :+ (cls @ TypeDef(_, template: Template)) if cls.symbol.isAnonymousClass =>
        template.parents.collectFirst { case p: Apply =>
          template.body.collectFirst {
            case dd: DefDef
                if dd.symbol.is(Override) && dd.symbol.name.toString == "__dfc" &&
                  dd.tpt.tpe <:< metaContextTpe =>
              if (!treeOwnerOverrideMap.contains(dd))
                treeOwnerOverrideMap += (dd -> (EmptyTree, p.srcPos))
          }
        }
      case _ =>
    ctx
  end prepareForBlock

  override def transformDefDef(tree: DefDef)(using Context): tpd.Tree =
    val sym = tree.symbol
    if (sym.is(Override) && sym.name.toString == "__dfc" && tree.tpt.tpe <:< metaContextTpe)
      treeOwnerOverrideMap.get(tree) match
        case Some(ownerTree, srcPos) =>
          getMetaInfo(ownerTree, srcPos) match
            case Some(metaInfo) =>
              cpy.DefDef(tree)(rhs = tree.rhs.setMeta(metaInfo))
            case None =>
              if (ownerTree.isEmpty)
                cpy.DefDef(tree)(rhs = tree.rhs.setMeta(None, srcPos, None, Nil))
              else tree
        case None => tree
    else tree
  end transformDefDef

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case template: Template =>
        if (!tree.symbol.isAnonymousClass)
          template.parents.foreach {
            case p: Apply => addToTreeOwnerMap(p, tree, None)
            case _        =>
          }
          addContextDef(tree)
        clsStack = tree :: clsStack
      case _ =>
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    tree.rhs match
      case template: Template =>
        clsStack = clsStack.drop(1)
      case _ =>
    tree

  private def addToTreeOwnerMap(
      apply: Apply,
      ownerTree: MemberDef,
      inlinedSrcPos: Option[util.SrcPos],
      dfcOverrideDef: Option[DefDef] = None
  )(using
      Context
  ): Unit =
    // debug("~~~~~~~~~~~~~~~~~~~~")
    // debug(s"Adding under ${ownerTree.name}, ${inlinedSrcPos.map(_.show)}:\n${apply.show}")
    if (!treeOwnerApplyMap.contains(apply))
      val srcPos = inlinedSrcPos.getOrElse(apply.srcPos)
      treeOwnerApplyMap = treeOwnerApplyMap + (apply -> (ownerTree, srcPos))
      dfcOverrideDef.foreach: dd =>
        treeOwnerOverrideMap += (dd -> (ownerTree, srcPos))
  end addToTreeOwnerMap

  object ApplyArgForward:
    def unapply(tree: Apply)(using Context): Option[Tree] =
      // checking for meta forwarding
      tree.fun.symbol.getAnnotation(metaContextForwardAnnotSym).flatMap { annot =>
        annot.tree match
          case Apply(_, List(Literal(Constant(argIdx: Int)))) =>
            val ApplyFunArgs(_, args) = tree: @unchecked
            Some(args.flatten.toList(argIdx))
          case _ => None
      }

  private def nameValOrDef(
      tree: Tree,
      ownerTree: ValOrDefDef,
      typeFocus: Type,
      inlinedSrcPos: Option[util.SrcPos]
  )(using Context): Boolean =
    // debug("------------------------------")
    // debug("pos--->  ", tree.srcPos.show)
    // debug(tree.showSummary(10))
    // in case this returns an iterable of type T, then the arguments that
    // have the type T will also be candidates to get the name
    lazy val iterableType: Option[Type] = typeFocus match
      case AppliedType(tycon, typeArg :: Nil)
          if tycon.typeSymbol.inherits("scala.collection.Iterable") =>
        Some(typeArg)
      case _ => None
    tree match
      // any `map` or `for` would yield a block of anonymous function and closure
      case Apply(_, List(Block(List(defdef: DefDef), _: Closure))) =>
        // debug("Map/For")
        nameValOrDef(defdef.rhs, ownerTree, defdef.rhs.tpe.simple, inlinedSrcPos)
      case ApplyArgForward(tree) =>
        nameValOrDef(tree, ownerTree, typeFocus, inlinedSrcPos)
      case apply: Apply =>
        // debug("Apply done!")
        // ignoring anonymous method unless it has a context argument
        val add =
          if (ownerTree.symbol.isAnonymousFunction)
            ownerTree match
              // this case is for functions like `def foo(block : DFC ?=> Unit) : Unit`
              case DefDef(_, List(List(arg)), _, _) => arg.tpe <:< metaContextTpe
              case _                                => false
          else true
        if (add) iterableType match
          case Some(typeArg) =>
            apply.args.collectFirst {
              case termArg if termArg.tpe <:< typeArg => termArg
            } match
              case Some(termArg) => nameValOrDef(termArg, ownerTree, typeArg, inlinedSrcPos)
              case None          => false
          case _ =>
            apply match
              case ContextArg(_) =>
                addToTreeOwnerMap(apply, ownerTree, inlinedSrcPos)
                true
              case _ => false
        else false
      case Typed(tree, _) =>
        // debug("Typed")
        nameValOrDef(tree, ownerTree, typeFocus, inlinedSrcPos)
      case TypeApply(Select(tree, _), _) =>
        // debug("TypeApply")
        nameValOrDef(tree, ownerTree, typeFocus, inlinedSrcPos)
      case inlined @ Inlined(_, bindings, tree) =>
        // debug("Inlined")
        val updatedInlineSrcPos = inlinedSrcPos orElse Some(inlined.srcPos)
        if (!nameValOrDef(tree, ownerTree, typeFocus, updatedInlineSrcPos))
          val ownerTreeSym = ownerTree.symbol
          bindings.view.reverse.collectFirst {
            case vd @ ValDef(_, _, apply: Apply)
                // we ignore the binding if the owner (tree) is a method.
                // this looks like:
                // ```
                // def apply(arg: Int)(using $x1: DFC): ...
                //    val some$proxy1 = foo(arg)($x1)
                //     some$proxy1.asInstanceOf
                // ```
                if !(vd.symbol.owner == ownerTreeSym && ownerTreeSym.is(Method)) =>
              nameValOrDef(apply, ownerTree, typeFocus, updatedInlineSrcPos)
          }.getOrElse(false)
        else true
        end if
      case Block(List(anonDef: DefDef), _: Closure) if anonDef.symbol.isAnonymousFunction =>
        nameValOrDef(anonDef.rhs, ownerTree, typeFocus, inlinedSrcPos)
      case Block(_ :+ (cls @ TypeDef(_, template: Template)), _) if cls.symbol.isAnonymousClass =>
        // debug("Block done!")
        template.body.lastOption match
          case Some(defDef: DefDef) if template.parents.exists(_.symbol.forwardMetaContext) =>
            nameValOrDef(defDef.rhs, ownerTree, typeFocus, inlinedSrcPos)
          case _ =>
            var named = false
            template.parents.collectFirst { case p: Apply =>
              val dfcOverrideDef = template.body.collectFirst {
                case dd: DefDef
                    if dd.symbol.is(
                      Override
                    ) && dd.symbol.name.toString == "__dfc" && dd.tpt.tpe <:< metaContextTpe =>
                  dd
              }
              addToTreeOwnerMap(p, ownerTree, inlinedSrcPos, dfcOverrideDef)
              named = true
            }
            named
        end match
      case block: Block =>
        // debug("Block expr")
        nameValOrDef(block.expr, ownerTree, typeFocus, inlinedSrcPos)
      case tryBlock: Try =>
        // debug("Try block")
        nameValOrDef(tryBlock.expr, ownerTree, typeFocus, inlinedSrcPos)
      case _ => false
    end match
  end nameValOrDef

  def addContextDef(tree: Tree)(using Context): Unit =
    val defdefTree = tree match
      case tree: DefDef  => tree
      case tree: TypeDef =>
        // debug(tree.symbol)
        tree.rhs.asInstanceOf[Template].constr
    defdefTree.paramss.flatten.view.reverse.collectFirst {
      case a if a.tpe <:< metaContextTpe =>
        val fixedName = a.symbol.fixedFullName
        // debug(s"Def   ${fixedName}, ${tree.show}")
        contextDefs += (fixedName -> tree)
    }
  private def rejectBadPrimitiveOps(tree: Apply, pos: util.SrcPos)(using
      Context
  ): Unit =
    tree match
      case Apply(Select(lhs, fun), List(rhs))
          if (fun == nme.EQ || fun == nme.NE) &&
            (lhs.tpe <:< defn.IntType || lhs.tpe <:< defn.BooleanType || lhs.tpe <:< defn
              .TupleTypeRef) =>
        val rhsSym = rhs.tpe.dealias.typeSymbol
        if (rhsSym == dfValSym)
          report.error(
            s"Unsupported Scala primitive at the LHS of `$fun` with a DFHDL value.\nConsider switching positions of the arguments.",
            pos
          )
      case Apply(Select(lhs, fun), List(Apply(Apply(Ident(hackName), _), _)))
          if (fun == nme.ZOR || fun == nme.ZAND || fun == nme.XOR) && hackName.toString == "BooleanHack" =>
        report.error(
          s"Unsupported Scala Boolean primitive at the LHS of `$fun` with a DFHDL value.\nConsider switching positions of the arguments.",
          pos
        )
      case Apply(Apply(Ident(hackName), _), _) if hackName.toString == "BooleanHack" =>
        report.error(
          s"Found unexpected DFHDL boolean to Scala boolean conversion.",
          pos
        )
      case _ =>

  override def prepareForApply(tree: Apply)(using Context): Context =
    val srcPos = treeOwnerApplyMap.get(tree) match
      case Some(_, srcPos) => srcPos
      case _               => tree.srcPos

    rejectBadPrimitiveOps(tree, srcPos)
    applyStack = tree :: applyStack
    ctx

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    tree.rhs match
      // for inline defs that result from a context function, we forward to
      // the anonDef's RHS, just for the purpose of ignoring the Apply later on
      case Block(List(anonDef: DefDef), closure: Closure) if tree.isInline =>
        nameValOrDef(anonDef.rhs, tree, tree.tpe.simple, None)
      case _ =>
        if (
          !tree.symbol.isClassConstructor && !tree.symbol.isAnonymousFunction &&
          !tree.name.toString.contains("$proxy")
        )
          addContextDef(tree)
          nameValOrDef(tree.rhs, tree, tree.tpe.simple, None)
    ctx
  end prepareForDefDef

  override def prepareForInlined(tree: Inlined)(using Context): Context =
    // skipping over redundant inlines that should not be used for positioning
    if (!tree.call.symbol.is(Permanent))
      nameValOrDef(tree.expansion, EmptyValDef, tree.expansion.tpe, Some(tree.srcPos))
    ctx

  // This is requires for situations like:
  // val (a, b) = (foo(using DFC), foo(using DFC))
  // It is desugared into:
  // val $num$ = (foo(using DFC), foo(using DFC))
  // val a = $num$._1
  // val b = $num$._2
  // Since we need the `a` and `b` trees to be the name owners,
  // we go over the desugared Stats and run `nameValOrDef` accordingly.
  override def prepareForStats(trees: List[Tree])(using Context): Context =
    var tupleArgs: List[Tree] = Nil
    var idx = 0
    def isSyntheticTuple(sym: Symbol) =
      val symName = sym.name.toString
      symName.startsWith("$") && symName.endsWith("$")
    object TupleArgs:
      def unapply(tree: Tree): Option[List[Tree]] =
        tree match
          case Apply(_: TypeApply, args) => Some(args)
          case Match(Typed(tree, _), _)  => unapply(tree)
          case Inlined(_, _, tree)       => unapply(tree)
          case _                         => None
    trees.foreach {
      case vd @ ValDef(_, _, TupleArgs(args))
          if vd.tpe <:< defn.TupleTypeRef && isSyntheticTuple(vd.symbol) =>
        tupleArgs = args
      case vd @ ValDef(_, _, Select(x, sel))
          if tupleArgs.nonEmpty && x.tpe <:< defn.TupleTypeRef &&
            isSyntheticTuple(x.symbol) && sel.toString.startsWith("_") =>
        nameValOrDef(tupleArgs(idx), vd, vd.tpt.tpe.simple, None)
        idx = idx + 1
        if (idx == tupleArgs.length)
          idx = 0
          tupleArgs = Nil
      case _ =>
    }
    ctx
  end prepareForStats

  private val inlinedName = "(.*)_this".r
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    tree.name.toString match
      case n if n.contains("$")     => // do nothing
      case _ if tree.mods.is(Param) => // do nothing
      case _ if tree.rhs.isEmpty    => // do nothing
      case _                        =>
        // debug("================================================")
        // debug(s"prepareForValDef: ${tree.name}")
        treeOwnerApplyMapStack = treeOwnerApplyMap :: treeOwnerApplyMapStack
        nameValOrDef(tree.rhs, tree, tree.tpe.simple, None)
    end match
    ctx
  end prepareForValDef

  override def transformValDef(tree: ValDef)(using Context): Tree =
    tree.name.toString match
      case n if n.contains("$")     => // do nothing
      case _ if tree.mods.is(Param) => // do nothing
      case _ if tree.rhs.isEmpty    => // do nothing
      case _                        =>
        // debug(s"transformValDef: ${tree.name}")
        treeOwnerApplyMap = treeOwnerApplyMapStack.head
        treeOwnerApplyMapStack = treeOwnerApplyMapStack.drop(1)
    end match
    tree

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    setMetaAnonSym = metaContextCls.requiredMethod("setMetaAnon")
    treeOwnerOverrideMap.clear()
    contextDefs.clear()
    ctx
  end prepareForUnit
end MetaContextGenPhase
