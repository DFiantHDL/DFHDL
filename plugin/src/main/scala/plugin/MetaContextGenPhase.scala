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
import collection.mutable
import annotation.tailrec

class MetaContextGenPhase(setting: Setting) extends CommonPhase:
  import tpd._

  // override val debugFilter: String => Boolean = _.contains("DFOpaqueSpec.scala")
  val phaseName = "MetaContextGen"

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextDelegate")
  var setMetaSym: Symbol = _
  var dfTokenSym: Symbol = _
  var metaContextForwardAnnotSym: ClassSymbol = _
  var metaContextIgnoreAnnotSym: ClassSymbol = _
  val treeOwnerMap = mutable.Map.empty[Apply, Tree]
  val contextDefs = mutable.Map.empty[String, Tree]
  var clsStack = List.empty[TypeDef]
  val inlinedOwnerMap = mutable.Map.empty[Apply, Inlined]
  var applyPosStack = List.empty[util.SrcPos]
  var applyStack = List.empty[Apply]

  extension (tree: ValOrDefDef)(using Context)
    def needsNewContext: Boolean =
      tree match
        case _: ValDef => true // valdefs always generate new context
        case _         =>
          // defdefs generate new context if they are not inline
          // and when they are not synthetic, indicating that they
          // are actually constructor definitions (other synthetics
          // should not have context, anyways)
          !tree.isInline && !tree.symbol.is(Synthetic)

  extension (tree: Tree)(using Context)
    def setMeta(
        nameOpt: Option[String],
        srcPos: util.SrcPos,
        docOpt: Option[String],
        annotations: List[Annotations.Annotation]
    ): Tree =
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
    end setMeta
  end extension

  extension (sym: Symbol)
    def fixedFullName(using Context): String =
      sym.fullName.toString.replace("._$", ".")
    def ignoreMetaContext(using Context): Boolean =
      sym.hasAnnotation(metaContextIgnoreAnnotSym)
  private def ignoreValDef(tree: ValDef)(using Context): Boolean =
    tree.name.toString match
      case inlinedName(prefix) =>
        tree.tpe match
          case x: TermRef =>
            x.underlying.dealias.typeSymbol.name.toString == prefix ||
            x.parents.exists(_.typeSymbol.name.toString == prefix)
          case _ => false
      case _ => false
  override def transformApply(tree: Apply)(using Context): Tree =
    val srcPos = applyPosStack.head
    val origApply = applyStack.head
    applyPosStack = applyPosStack.drop(1)
    applyStack = applyStack.drop(1)
    if (tree.tpe.isParameterless && !tree.fun.symbol.ignoreMetaContext)
      tree match
        case ContextArg(argTree) =>
          val sym = argTree.symbol
          treeOwnerMap.get(origApply) match
            case Some(t: ValOrDefDef) if t.needsNewContext =>
              if (t.symbol.flags.is(Flags.Mutable))
                report.warning(
                  "Scala `var` modifier for DFHDL values/classes is highly discouraged!\nConsider changing to `val`.",
                  t.srcPos
                )
              val (nameOpt, docOpt, annots) =
                t match
                  case vd: ValDef if (ignoreValDef(vd)) => (None, None, Nil)
                  case dd: DefDef                       => (None, None, Nil)
                  case _ =>
                    (
                      Some(t.name.toString.nameCheck(t)),
                      t.symbol.docString,
                      t.symbol.staticAnnotations
                    )
              tree.replaceArg(argTree, argTree.setMeta(nameOpt, srcPos, docOpt, annots))
            case Some(t: TypeDef) if t.name.toString.endsWith("$") =>
              tree.replaceArg(
                argTree,
                argTree.setMeta(
                  Some(t.name.toString.dropRight(1).nameCheck(t)),
                  srcPos,
                  t.symbol.docString,
                  t.symbol.staticAnnotations
                )
              )
            case Some(t) => // Def or Class
              contextDefs.get(sym.fixedFullName) match
                case Some(ct) if !(ct sameTree t) =>
                  report.error(
                    s"${t.symbol} is missing an implicit Context parameter",
                    t.symbol
                  )
                case _ =>
              // do nothing
              tree
            case _ => // Anonymous
              if (tree.fun.symbol.name.toString.contains("$")) tree
              else tree.replaceArg(argTree, argTree.setMeta(None, srcPos, None, Nil))
          end match
        case _ => tree
    else tree
  end transformApply

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case template: Template =>
        if (!tree.symbol.isAnonymousClass)
          template.parents.foreach {
            case p: Apply => addToTreeOwnerMap(p, tree)
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

  private def addToTreeOwnerMap(apply: Apply, ownerTree: Tree)(using Context): Unit =
    // debug("~~~~~~~~~~~~~~~~~~~~")
    // debug(s"Adding: ${apply.show}")
    // debug(ownerTree.show)
    treeOwnerMap += (apply -> ownerTree)

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

  @tailrec private def nameValOrDef(
      tree: Tree,
      ownerTree: Tree,
      typeFocus: Type
  )(using Context): Unit =
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
        nameValOrDef(defdef.rhs, ownerTree, defdef.rhs.tpe.simple)
      case ApplyArgForward(tree) =>
        nameValOrDef(tree, ownerTree, typeFocus)
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
              case Some(termArg) => nameValOrDef(termArg, ownerTree, typeArg)
              case None          =>
          case _ => addToTreeOwnerMap(apply, ownerTree)
      case Typed(tree, _) =>
        // debug("Typed")
        nameValOrDef(tree, ownerTree, typeFocus)
      case TypeApply(Select(tree, _), _) =>
        // debug("TypeApply")
        nameValOrDef(tree, ownerTree, typeFocus)
      case inlined @ Inlined(_, bindings, tree) =>
        // debug("Inlined")
        bindings.view.reverse.collectFirst {
          case vd @ ValDef(_, _, apply: Apply) if !vd.symbol.owner.is(Method) =>
            addToTreeOwnerMap(apply, ownerTree)
        }
        nameValOrDef(tree, ownerTree, typeFocus)
      case Block((cls @ TypeDef(_, template: Template)) :: _, _) if cls.symbol.isAnonymousClass =>
        // debug("Block done!")
        template.parents.foreach {
          case p: Apply => addToTreeOwnerMap(p, ownerTree)
          case _        =>
        }
      case block: Block =>
        // debug("Block expr")
        nameValOrDef(block.expr, ownerTree, typeFocus)
      case tryBlock: Try =>
        // debug("Try block")
        nameValOrDef(tryBlock.expr, ownerTree, typeFocus)
      case _ =>
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
            (lhs.tpe <:< defn.IntType || lhs.tpe <:< defn.BooleanType || lhs.tpe <:< defn.TupleTypeRef) =>
        val rhsSym = rhs.tpe.dealias.typeSymbol
        if (rhsSym == dfValSym || rhsSym == dfTokenSym)
          report.error(
            s"Unsupported Scala primitive at the LHS of `$fun` with a DFHDL value or token.\nConsider switching positions of the arguments.",
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
    val srcPos = inlinedOwnerMap.get(tree) match
      case Some(inlined) =>
        inlinedOwnerMap.remove(tree)
        inlined.srcPos
      case _ => tree.srcPos

    rejectBadPrimitiveOps(tree, srcPos)
    applyPosStack = srcPos :: applyPosStack
    applyStack = tree :: applyStack
    ctx

  override def prepareForInlined(inlined: Inlined)(using Context): Context =
    inlinePos(inlined.expansion, inlined)
    ctx

  @tailrec private def inlinePos(
      tree: Tree,
      inlinedTree: Inlined
  )(using Context): Unit =
//    debug("pos--->  ", tree.srcPos.show)
//    debug(tree.show)
    tree match
      case ApplyArgForward(tree) => inlinePos(tree, inlinedTree)
      case apply: Apply =>
        if (!inlinedOwnerMap.contains(apply) && !inlinedTree.call.isEmpty)
          // debug("INLINE:")
          // debug("from", apply.srcPos.show)
          // debug("to  ", inlinedTree.srcPos.show)
          inlinedOwnerMap += (apply -> inlinedTree)
      case Typed(tree, _)                => inlinePos(tree, inlinedTree)
      case TypeApply(Select(tree, _), _) => inlinePos(tree, inlinedTree)
      case Inlined(_, bindings, tree) =>
        bindings.view.reverse.collectFirst {
          case vd @ ValDef(_, _, apply: Apply) if !vd.symbol.owner.is(Method) =>
            if (!inlinedOwnerMap.contains(apply) && !inlinedTree.call.isEmpty)
              inlinedOwnerMap += (apply -> inlinedTree)
        }
        inlinePos(tree, inlinedTree)
      case block: Block => inlinePos(block.expr, inlinedTree)
      case _            =>
    end match
  end inlinePos

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    if (
      !tree.symbol.isClassConstructor && !tree.symbol.isAnonymousFunction &&
      !tree.name.toString.contains("$proxy") && !(tree.symbol is Exported)
    )
      addContextDef(tree)
      nameValOrDef(tree.rhs, tree, tree.tpe.simple)
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
        nameValOrDef(tupleArgs(idx), vd, vd.tpt.tpe.simple)
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
        nameValOrDef(tree.rhs, tree, tree.tpe.simple)
    end match
    ctx
  end prepareForValDef

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    dfTokenSym = requiredClass("dfhdl.core.DFToken")
    metaContextForwardAnnotSym = requiredClass("dfhdl.internals.metaContextForward")
    metaContextIgnoreAnnotSym = requiredClass("dfhdl.internals.metaContextIgnore")
    treeOwnerMap.clear()
    contextDefs.clear()
    inlinedOwnerMap.clear()
    ctx
  end prepareForUnit
end MetaContextGenPhase
