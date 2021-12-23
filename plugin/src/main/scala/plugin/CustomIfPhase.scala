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
  override val debugFilter: String => Boolean =
    _.contains("DFMatchSpec.scala")
  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")
  val ignore = mutable.Set.empty[String]
  val replace = mutable.Set.empty[String]
  var fromBooleanSym: Symbol = _
  var toFunc1Sym: Symbol = _
  var toTuple2Sym: Symbol = _
  var fromBranchesSym: Symbol = _
  var dfValClsRef: TypeRef = _
  var enumHackedUnapply: Symbol = _

  override def transformApply(tree: Apply)(using Context): Tree =
    tree

  private def isHackedGuard(tree: Tree)(using Context): Boolean =
    tree match
      case Apply(Apply(Ident(n), List(dfCond)), List(dfc))
          if n.toString == "BooleanHack" =>
        true
      case _ => false

  private def isHackedIf(tree: If)(using Context): Boolean =
    isHackedGuard(tree.cond)

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
  private def getDFC(tree: UnApply)(using Context): Tree =
    val UnApply(TypeApply(Apply(_, List(entry)), _), List(dfc), _) = tree
    dfc

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

  private def transformCasePattern(tree: Tree)(using Context): Tree =
    tree match
      case literal: Literal =>
      // hacked unapply for enum enumerations
      case unapply: UnApply if unapply.fun.symbol == enumHackedUnapply =>
      // catch all
      case Ident(i) if i.toString == "_" =>
      // catch all with name bind
      case Bind(n, Ident(i)) if i.toString == "_" =>
      // named and typed bind
      case Bind(n, Typed(Ident(i), tpt)) if i.toString == "_" =>
      // union of alternatives
      case Alternative(list) =>
      // unknown pattern
      case _ =>
        debug(s"Unknown pattern: ${tree.show}")
    end match
    ???
  end transformCasePattern

  override def prepareForMatch(tree: Match)(using Context): Context =
    tree.selector.tpe.underlyingIfProxy match
      case AppliedType(tycon, _) if tycon <:< dfValClsRef =>
        debug("The entire match tree")
        debug(tree.show)
        debug("Case pattern")
        debug(tree.cases.head.pat)
        tree.cases.head.pat match
          // hacked unapply for enum enumerations
          case tree: UnApply if tree.fun.symbol == enumHackedUnapply =>
            debug("DFC")
            debug(getDFC(tree))
          case _ =>
        debug("Case guard")
        debug(isHackedGuard(tree.cases.head.guard))
        debug("Case RHS")
        debug(tree.cases.head.body)
      case _ => // do nothing
    ctx
  end prepareForMatch
  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ignore.empty
    replace.empty
    fromBooleanSym = requiredMethod("DFiant.core.__For_Plugin.fromBoolean")
    toFunc1Sym = requiredMethod("DFiant.core.__For_Plugin.toFunc1")
    toTuple2Sym = requiredMethod("DFiant.core.__For_Plugin.toTuple2")
    fromBranchesSym = requiredMethod("DFiant.core.DFIf.fromBranches")
    dfValClsRef = requiredClassRef("DFiant.core.DFVal")
    enumHackedUnapply = requiredMethod("DFiant.unapply")
    ctx
end CustomIfPhase
