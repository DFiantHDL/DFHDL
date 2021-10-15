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
  var positionCls: ClassSymbol = _
  var metaContextCls: ClassSymbol = _
  var setMetaSym: Symbol = _
  var lateConstructionTpe: TypeRef = _
  val treeOwnerMap = mutable.Map.empty[String, Tree]
  val contextDefs = mutable.Map.empty[String, Tree]
  var clsStack = List.empty[TypeDef]
  var inlinedOwnerStack = List.empty[(Tree, Inlined)]

  extension (tree: Tree)(using Context)
    def inlinedPos: util.SrcPos =
      inlinedOwnerStack.headOption match
        case Some(t, inlined) if t.toString == tree.toString => inlined.srcPos
        case _                                               => tree.srcPos
    def unique(using inlinedPosOpt: Option[util.SrcPos] = None): String =
      inlinedPosOpt.getOrElse(tree.inlinedPos).show

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
    def show: String =
      val pos = srcPos.startPos
      val endPos = srcPos.endPos
      s"${pos.source.path}:${pos.line}:${pos.column}-${endPos.line}:${endPos.column}"
  end extension

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
      val srcPos = srcTree.inlinedPos
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
          case ann
              if ann.symbol.fullName.toString == "scala.annotation.targetName" =>
            ann.argumentConstantString(0).get
        }
        .getOrElse(name)
      if (!finalName.matches("^[a-zA-Z0-9_]*$"))
        report.error(
          s"""Unsupported DSL member name $finalName.
           |Only alphanumric or underscore characters are supported.
           |You can leave the Scala name as-is and add @targetName("newName") annotation.""".stripMargin,
          posTree.inlinedPos
        )
      finalName

  private def ignoreValDef(tree: ValDef)(using Context): Boolean =
    tree.name.toString match
      case inlinedName(prefix) =>
        tree.tpe.typeSymbol.name.toString == prefix ||
          tree.tpe.parents.exists(_.typeSymbol.name.toString == prefix)
      case _ => false
  override def transformApply(tree: Apply)(using Context): Tree =
    if (tree.tpe.isParameterless)
      tree match
        case ContextArg(argTree) =>
          val sym = argTree.symbol
          treeOwnerMap.get(tree.unique) match
            case Some(t: ValDef) =>
              if (t.mods.is(Flags.Mutable))
                report.warning(
                  "Variable modifier for DSL constructed values is highly discouraged!\nConsider changing to `val`.",
                  t.inlinedPos
                )
              val nameOpt =
                if (ignoreValDef(t)) None
                else Some(t.name.toString.nameCheck(t))
              tree.replaceArg(argTree, argTree.setMeta(nameOpt, tree))
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
    treeOwnerMap += (tree.unique -> ownerTree)

  @tailrec private def nameValOrDef(
      tree: Tree,
      ownerTree: Tree,
      inlinedPosOpt: Option[util.SrcPos]
  )(using Context): Unit =
    tree match
      case apply: Apply =>
        addToTreeOwnerMap(apply, ownerTree)(using inlinedPosOpt)
      case Typed(tree, _) =>
        nameValOrDef(tree, ownerTree, inlinedPosOpt)
      case TypeApply(Select(tree, _), _) =>
        nameValOrDef(tree, ownerTree, inlinedPosOpt)
      case inlined @ Inlined(_, _, tree) =>
        nameValOrDef(tree, ownerTree, Some(inlined.srcPos))
      case Block((cls @ TypeDef(_, template: Template)) :: _, _)
          if cls.symbol.isAnonymousClass =>
        template.parents.foreach(p =>
          addToTreeOwnerMap(p, ownerTree)(using inlinedPosOpt)
        )
      case block: Block =>
        nameValOrDef(block.expr, ownerTree, inlinedPosOpt)
      case _ =>
    end match
  end nameValOrDef

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

  override def prepareForInlined(tree: Inlined)(using Context): Context =
    tree match
      case Inlined(_, _, Typed(actualTree, _)) =>
        inlinedOwnerStack = (actualTree, tree) :: inlinedOwnerStack
      case _ =>
    ctx

  override def transformInlined(tree: Inlined)(using Context): Tree =
    tree match
      case Inlined(_, _, Typed(actualTree, _)) =>
        inlinedOwnerStack = inlinedOwnerStack.drop(1)
      case _ =>
    tree

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    if (
      !tree.symbol.isClassConstructor && !tree.name.toString.contains("$proxy")
    )
      addContextDef(tree)
      nameValOrDef(tree.rhs, tree, None)
    ctx

  private val inlinedName = "(.*)_this".r
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    tree.name.toString match
      case n if n.contains("$proxy") => //do nothing
      case _ if tree.mods.is(Param)  => //do nothing
      case _ if tree.rhs.isEmpty     => //do nothing
      case _ =>
        nameValOrDef(tree.rhs, tree, None)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    positionCls = requiredClass("DFiant.internals.Position")
    metaContextCls = requiredClass("DFiant.internals.MetaContext")
    lateConstructionTpe = requiredClassRef("DFiant.internals.LateConstruction")
    setMetaSym = metaContextCls.requiredMethod("setMeta")
    treeOwnerMap.clear()
    contextDefs.clear()
    ctx
end MetaContextGenPhase
