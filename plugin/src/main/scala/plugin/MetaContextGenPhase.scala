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
import Constants.Constant
import Types.*

import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class MetaContextGenPhase(setting: Setting) extends CommonPhase:
  import tpd._

//  override val debugFilter: String => Boolean =
//    _.contains("PluginSpec.scala")
  val phaseName = "MetaContextGen"

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextDelegate")
  var setMetaSym: Symbol = _
  var lateConstructionTpe: TypeRef = _
  var dfTokenSym: Symbol = _
  var dfValSym: Symbol = _
  val treeOwnerMap = mutable.Map.empty[String, Tree]
  val contextDefs = mutable.Map.empty[String, Tree]
  var clsStack = List.empty[TypeDef]
  val inlinedOwnerStack = mutable.Map.empty[Apply, Inlined]
  var applyPosStack = List.empty[util.SrcPos]

  extension (tree: Tree)(using Context)
    def setMeta(
        nameOpt: Option[String],
        srcTree: Tree,
        srcPos: util.SrcPos
    ): Tree =
      val nameOptTree = nameOpt match
        case Some(str) =>
          New(
            defn.SomeClass.typeRef.appliedTo(defn.StringType),
            Literal(Constant(str)) :: Nil
          )
        case None =>
          ref(defn.NoneModule.termRef)
      val clsTree = clsStack.head
      val lateConstruction =
        clsTree.name.toString.contains("$") &&
          clsTree.tpe <:< lateConstructionTpe &&
          clsTree.rhs
            .asInstanceOf[Template]
            .parents
            .forall(p => !(p sameTree srcTree))
      val lateConstructionTree = Literal(Constant(lateConstruction))
      val positionTree = srcPos.positionTree
      tree
        .select(setMetaSym)
        .appliedToArgs(
          nameOptTree :: positionTree :: lateConstructionTree :: Nil
        )
        .withType(TermRef(tree.tpe, setMetaSym))

  extension (sym: Symbol)
    def fixedFullName(using Context): String =
      sym.fullName.toString.replace("._$", ".")

  extension (name: String)
    def nameCheck(posTree: Tree)(using Context): String =
      val finalName = posTree.symbol.annotations
        .collectFirst {
          case ann if ann.symbol.fullName.toString == "scala.annotation.targetName" =>
            ann.argumentConstantString(0).get
        }
        .getOrElse(name)
      if (
        !finalName.matches("^[a-zA-Z0-9_]*$") && !posTree.symbol.flags.is(
          Flags.Synthetic
        )
      )
        report.error(
          s"""Unsupported DSL member name $finalName.
           |Only alphanumric or underscore characters are supported.
           |You can leave the Scala name as-is and add @targetName("newName") annotation.""".stripMargin,
          posTree.srcPos
        )
      finalName

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
    applyPosStack = applyPosStack.drop(1)
    if (tree.tpe.isParameterless)
      tree match
        case ContextArg(argTree) =>
          val sym = argTree.symbol
          treeOwnerMap.get(srcPos.show) match
            case Some(t: ValDef) =>
              if (t.symbol.flags.is(Flags.Mutable))
                report.warning(
                  "Scala `var` modifier for dataflow values/classes is highly discouraged!\nConsider changing to `val`.",
                  t.srcPos
                )
              val nameOpt =
                if (ignoreValDef(t)) None
                else Some(t.name.toString.nameCheck(t))
              tree.replaceArg(argTree, argTree.setMeta(nameOpt, tree, srcPos))
            case Some(t: TypeDef) if t.name.toString.endsWith("$") =>
              tree.replaceArg(
                argTree,
                argTree.setMeta(
                  Some(t.name.toString.dropRight(1).nameCheck(t)),
                  tree,
                  srcPos
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
              tree.replaceArg(argTree, argTree.setMeta(None, tree, srcPos))
          end match
        case _ => tree
    else tree
  end transformApply

  val localPattern = "\\<local (.*)\\$\\>".r
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case template: Template =>
        if (!tree.symbol.isAnonymousClass)
          template.parents.foreach(p => addToTreeOwnerMap(p, tree)(using None))
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

  private def addToTreeOwnerMap(tree: Tree, ownerTree: Tree)(using
      inlinedPosOpt: Option[util.SrcPos]
  )(using Context): Unit =
    debug("Adding!!!")
    val srcPos = inlinedPosOpt.getOrElse(tree.srcPos)
    debug(srcPos.show)
    treeOwnerMap += (srcPos.show -> ownerTree)

  @tailrec private def nameValOrDef(
      tree: Tree,
      ownerTree: Tree,
      inlinedPosOpt: Option[util.SrcPos]
  )(using Context): Unit =
    debug("------------------------------")
    debug("pos--->  ", tree.srcPos.show)
    debug(tree.showSummary(10))
    tree match
      case apply: Apply =>
        debug("Apply done!")
        addToTreeOwnerMap(apply, ownerTree)(using inlinedPosOpt)
      case Typed(tree, _) =>
        debug("Typed")
        nameValOrDef(tree, ownerTree, inlinedPosOpt)
      case TypeApply(Select(tree, _), _) =>
        debug("TypeApply")
        nameValOrDef(tree, ownerTree, inlinedPosOpt)
      case inlined @ Inlined(_, _, tree) =>
        debug("Inlined")
        nameValOrDef(
          tree,
          ownerTree,
          Some(inlinedPosOpt.getOrElse(inlined.srcPos))
        )
      case Block((cls @ TypeDef(_, template: Template)) :: _, _) if cls.symbol.isAnonymousClass =>
        debug("Block done!")
        template.parents.foreach(p => addToTreeOwnerMap(p, ownerTree)(using inlinedPosOpt))
      case block: Block =>
        debug("Block expr")
        nameValOrDef(block.expr, ownerTree, inlinedPosOpt)
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
            s"Unsupported Scala primitive at the LHS of `$fun` with a dataflow value or token.\nConsider switching positions of the arguments.",
            pos
          )
      case Apply(Select(lhs, fun), List(Apply(Ident(hackName), _)))
          if (fun == nme.ZOR || fun == nme.ZAND || fun == nme.XOR) && hackName.toString == "BooleanHack" =>
        report.error(
          s"Unsupported Scala Boolean primitive at the LHS of `$fun` with a dataflow value.\nConsider switching positions of the arguments.",
          pos
        )
      case _ =>

  override def prepareForApply(tree: Apply)(using Context): Context =
    val srcPos = inlinedOwnerStack.get(tree) match
      case Some(inlined) =>
        inlinedOwnerStack.remove(tree)
        inlined.srcPos
      case _ => tree.srcPos

    rejectBadPrimitiveOps(tree, srcPos)
    applyPosStack = srcPos :: applyPosStack
    ctx

  @tailrec private def inlinePos(
      tree: Tree,
      inlinedTree: Inlined
  )(using Context): Unit =
//    debug("pos--->  ", tree.srcPos.show)
//    debug(tree.show)
    tree match
      case apply: Apply =>
        if (!inlinedOwnerStack.contains(apply))
          debug("INLINE:")
          debug("from", apply.srcPos.show)
          debug("to  ", inlinedTree.srcPos.show)
          inlinedOwnerStack += (apply -> inlinedTree)
      case Typed(tree, _) =>
        inlinePos(tree, inlinedTree)
      case TypeApply(Select(tree, _), _) =>
        inlinePos(tree, inlinedTree)
      case Inlined(_, _, tree) =>
        inlinePos(tree, inlinedTree)
      case block: Block =>
        inlinePos(block.expr, inlinedTree)
      case _ =>
    end match
  end inlinePos

  override def prepareForInlined(inlined: Inlined)(using Context): Context =
    inlinePos(inlined.expansion, inlined)
    ctx

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    if (!tree.symbol.isClassConstructor && !tree.name.toString.contains("$proxy"))
      addContextDef(tree)
      nameValOrDef(tree.rhs, tree, None)
    ctx

  private val inlinedName = "(.*)_this".r
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    tree.name.toString match
      case n if n.contains("$proxy") => // do nothing
      case _ if tree.mods.is(Param)  => // do nothing
      case _ if tree.rhs.isEmpty     => // do nothing
      case _ =>
        debug("================================================")
        debug(s"prepareForValDef: ${tree.name}")
        nameValOrDef(tree.rhs, tree, None)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    lateConstructionTpe = requiredClassRef("DFiant.internals.LateConstruction")
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    dfValSym = requiredClass("DFiant.core.DFVal")
    dfTokenSym = requiredClass("DFiant.core.DFToken")
    treeOwnerMap.clear()
    contextDefs.clear()
    inlinedOwnerStack.clear()
    ctx
end MetaContextGenPhase
