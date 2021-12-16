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
import dotty.tools.dotc.semanticdb.ConstantMessage.SealedValue.UnitConstant

import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec

class CustomIfPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "CustomIf"
//  override val debugFilter: String => Boolean =
//    _.contains("DFIfSpec.scala")
  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")
  val ignore = mutable.Set.empty[String]
  val replace = mutable.Set.empty[String]
  var fromBooleanSym: Symbol = _
  var toFunc1Sym: Symbol = _
  var toTuple2Sym: Symbol = _
  var fromBranchesSym: Symbol = _

  override def transformApply(tree: Apply)(using Context): Tree =
    tree

  private def isHackedIf(tree: If)(using Context): Boolean =
    tree.cond match
      case Apply(Apply(Ident(n), List(dfCond)), List(dfc))
          if n.toString == "BooleanHack" =>
        true
      case _ => false

  @tailrec private def isHackedIfRecur(tree: If)(using Context): Boolean =
    if (isHackedIf(tree)) true
    else
      tree.elsep match
        case tree: If => isHackedIfRecur(tree)
        case _        => false

  @tailrec private def ignoreElseIfRecur(tree: If)(using Context): Unit =
    ignore += tree.srcPos.show
    tree.elsep match
      case tree: If => ignoreElseIfRecur(tree)
      case _        => // done

  @tailrec private def getDFC(tree: If)(using Context): Tree =
    if (isHackedIf(tree))
      val Apply(_, List(dfcTree)) = tree.cond
      dfcTree
    else
      tree.elsep match
        case tree: If => getDFC(tree)
        case _ =>
          report.error("Did not manage to find a DFC")
          ???

  override def prepareForIf(tree: If)(using Context): Context =
    if (!ignore.contains(tree.srcPos.show) && isHackedIfRecur(tree))
      tree.elsep match
        case tree: If => ignoreElseIfRecur(tree)
        case _        => // do nothing
      replace += tree.srcPos.show
    ctx

  private def transformCond(condTree: Tree, dfcTree: Tree)(using
      Context
  ): Tree =
    condTree match
      case Apply(Apply(_, List(dfCondTree)), _) => dfCondTree
      case _ =>
        ref(fromBooleanSym)
          .appliedTo(condTree)
          .appliedTo(dfcTree)

  private def transformBlock(tree: Tree, combinedTpe: Type)(using
      Context
  ): Tree =
    ref(toFunc1Sym)
      .appliedToType(combinedTpe)
      .appliedTo(tree)

  @tailrec private def transformIfRecur(
      tree: If,
      combinedTpe: Type,
      dfcTree: Tree,
      prevPairs: List[Tree]
  )(using
      Context
  ): (List[Tree], Tree) =
    val condTree = transformCond(tree.cond, dfcTree)
    val blockTree = transformBlock(tree.thenp, combinedTpe)
    val pairs =
      ref(toTuple2Sym)
        .appliedToTypes(List(condTree.tpe, blockTree.tpe))
        .appliedToArgs(List(condTree, blockTree)) :: prevPairs
    tree.elsep match
      case tree: If =>
        transformIfRecur(tree, combinedTpe, dfcTree, pairs)
      case Literal(Constant(_: Unit)) =>
        (pairs.reverse, ref(defn.NoneModule))
      case elseBlockTree =>
        val block = transformBlock(elseBlockTree, combinedTpe)
        val someBlock = New(
          defn.SomeClass.typeRef.appliedTo(block.tpe),
          block :: Nil
        )
        (pairs.reverse, someBlock)
  end transformIfRecur

  override def transformIf(tree: If)(using Context): Tree =
    if (replace.contains(tree.srcPos.show))
      debug("=======================")
      val dfcTree = getDFC(tree)
      val combinedTpe = tree.tpe
      debug("DFC", dfcTree)
      debug(tree.show)
      debug(tree.srcPos.show)
      debug(tree)
      val (branchesVarArgs, elseOption) =
        transformIfRecur(tree, combinedTpe, dfcTree, Nil)
      val branches = mkList(branchesVarArgs, TypeTree(defn.AnyType))
      ref(fromBranchesSym)
        .appliedToType(combinedTpe)
        .appliedTo(branches, elseOption)
        .appliedTo(dfcTree)
    else tree
  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ignore.empty
    replace.empty
    fromBooleanSym = requiredMethod("DFiant.core.__For_Plugin.fromBoolean")
    toFunc1Sym = requiredMethod("DFiant.core.__For_Plugin.toFunc1")
    toTuple2Sym = requiredMethod("DFiant.core.__For_Plugin.toTuple2")
    fromBranchesSym = requiredMethod("DFiant.core.ifdf.fromBranches")
    ctx
end CustomIfPhase
