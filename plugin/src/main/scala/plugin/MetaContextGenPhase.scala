package DFiant.plugin

import dotty.tools.dotc._

import plugins._

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import ast.tpd
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class MetaContextGenPhase(setting: Setting) extends CommonPhase {
  import tpd._

  override val show: Boolean = true
  val phaseName = "MetaContextGen"

  override val runsAfter = Set("MetaContextDelegate")
  override val runsBefore = Set(transform.FirstTransform.name)
  var positionCls: ClassSymbol = _
  var metaContextCls: ClassSymbol = _
  var setMetaSym: Symbol = _
  var lateConstructionTpe: TypeRef = _
  val treeOwnerMap = mutable.Map.empty[String, Tree]
  val contextDefs = mutable.Map.empty[String, Tree]
  val ignore = mutable.Set.empty[String]
  var clsStack = List.empty[TypeDef]

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

  extension (tree: Tree)(using Context)
    def setMeta(nameOpt: Option[String], srcTree: Tree): Tree =
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
      val positionTree = srcTree.srcPos.positionTree
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
    def nameCheck(posTree: Tree)(using Context): String = {
      val finalName = posTree.symbol.annotations
        .collectFirst {
          case ann
              if ann.symbol.fullName.toString == "scala.annotation.targetName" =>
            ann.argumentConstantString(0).get
        }
        .getOrElse(name)
      if (!finalName.matches("^[a-zA-Z0-9_]*$"))
        report.error(
          s"""Unsupported DSL member name $finalName.
           |Only alphanumric or underscore characters are supported.
           |You can leave the Scala name as-is and add @targetName("newName") annotation""".stripMargin,
          posTree.srcPos
        )
      finalName
    }

  override def transformApply(tree: Apply)(using Context): Tree =
    if (tree.tpe.isParameterless && !ignore.contains(tree.unique))
      tree match
        case ContextArg(argTree) =>
          val sym = argTree.symbol
          treeOwnerMap.get(tree.unique) match
            case Some(t: ValDef) =>
              if (t.mods.is(Flags.Mutable))
                report.warning(
                  "Variable modifier for DSL constructed values is highly discouraged!\nConsider changing to `val`.",
                  t.srcPos
                )
              tree.replaceArg(
                argTree,
                argTree.setMeta(Some(t.name.toString.nameCheck(t)), tree)
              )
            case Some(t: TypeDef) if t.name.toString.endsWith("$") =>
              tree.replaceArg(
                argTree,
                argTree.setMeta(
                  Some(t.name.toString.dropRight(1).nameCheck(t)),
                  tree
                )
              )
            case Some(t) => //Def or Class
              contextDefs.get(sym.fixedFullName) match
                case Some(ct) if ct != t =>
                  report.error(
                    s"${t.symbol} is missing an implicit Context parameter",
                    t.symbol
                  )
                case _ =>
              //do nothing
              tree
            case _ => //Anonymous
              tree.replaceArg(argTree, argTree.setMeta(None, tree))
        case _ => tree
    else tree

  val localPattern = "\\<local (.*)\\$\\>".r
  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    tree.rhs match
      case template: Template =>
        if (!tree.symbol.isAnonymousClass)
          template.parents.foreach(p => treeOwnerMap += (p.unique -> tree))
          addContextDef(tree)
        clsStack = tree :: clsStack
      case _ =>
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    clsStack = clsStack.drop(1)
    tree

  @tailrec private def nameValOrDef(tree: Tree, ownerTree: Tree)(using
      Context
  ): Unit =
    tree match
      case apply: Apply =>
        treeOwnerMap += (apply.unique -> ownerTree)
      case Typed(tree, _) =>
        nameValOrDef(tree, ownerTree)
      case TypeApply(Select(tree, _), _) =>
        nameValOrDef(tree, ownerTree)
      case Inlined(_, _, tree) =>
        nameValOrDef(tree, ownerTree)
      case Block((cls @ TypeDef(tpn, template: Template)) :: _, expr)
          if cls.symbol.isAnonymousClass =>
        template.parents.foreach(p => treeOwnerMap += (p.unique -> ownerTree))
      case block: Block => nameValOrDef(block.expr, ownerTree)
      case _            =>

  def addContextDef(tree: Tree)(using Context): Unit =
    val defdefTree = tree match
      case tree: DefDef  => tree
      case tree: TypeDef =>
        // println(tree.symbol)
        tree.rhs.asInstanceOf[Template].constr
    defdefTree.paramss.flatten.view.reverse.collectFirst {
      case a if a.tpe <:< metaContextTpe =>
        val fixedName = a.symbol.fixedFullName
        // println(s"Def   ${fixedName}, ${tree.show}")
        contextDefs += (fixedName -> tree)
    }

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    if (
      !tree.symbol.isClassConstructor && !tree.name.toString.contains("$proxy")
    )
      addContextDef(tree)
      nameValOrDef(tree.rhs, tree)
    ctx

  @tailrec private def ignoreInternalApplies(tree: Apply)(using Context): Unit =
    tree.fun match
      case apply: Apply =>
        ignore += apply.unique
        ignoreInternalApplies(apply)
      case _ =>

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if (!tree.name.toString.contains("$proxy")) nameValOrDef(tree.rhs, tree)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    positionCls = requiredClass("DFiant.internals.Position")
    metaContextCls = requiredClass("DFiant.internals.MetaContext")
    lateConstructionTpe = requiredClassRef("DFiant.internals.LateConstruction")
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    ctx

}
