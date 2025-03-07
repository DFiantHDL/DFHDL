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

class LoopFSMPhase(setting: Setting) extends CommonPhase:
  import tpd._

  // override val debugFilter: String => Boolean = _.contains("Example.scala")
  val phaseName = "LoopFSM"
  var stepRef: TypeRef = uninitialized
  var addStepSym: Symbol = uninitialized
  var customForSym: Symbol = uninitialized
  var getLoopIterSym: Symbol = uninitialized
  var toFunc1Sym: Symbol = uninitialized
  var fromBooleanSym: Symbol = uninitialized
  var customWhileSym: Symbol = uninitialized
  var dfcStack: List[Tree] = Nil

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    ContextArg.at(tree).foreach { t =>
      dfcStack = t :: dfcStack
    }
    ctx

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    ContextArg.at(tree).foreach { t =>
      dfcStack = dfcStack.drop(1)
    }
    tree

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    ContextArg.at(tree).foreach { t =>
      dfcStack = t :: dfcStack
    }
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    ContextArg.at(tree).foreach { t =>
      dfcStack = dfcStack.drop(1)
    }
    tree

  object Step:
    def unapply(tree: Tree)(using Context): Option[Tree] =
      if (tree.tpe <:< stepRef && !(tree.tpe =:= defn.NothingType))
        tree match
          case Apply(Ident(stepName), dfcTree :: _) if stepName.toString == "step" =>
            Some(dfcTree)
          case _ =>
            report.error("Unexpected step pattern. Must be `def xyz = step`", tree.srcPos)
            None
      else None

  case class FilteredRange(range: Tree, filters: List[(ValDef, Tree)])
  object FilteredRange:
    def unapply(tree: Tree)(using Context): Option[FilteredRange] =
      tree match
        case Apply(
              Select(range, withFilter),
              List(
                Block(
                  List(
                    dd @ DefDef(
                      anonfun,
                      List(List(iter: ValDef)),
                      _,
                      _
                    )
                  ),
                  _: Closure
                )
              )
            ) if anonfun.toString.startsWith("$anonfun") && withFilter.toString == "withFilter" =>
          val updatedRHS = dd.rhs.changeOwner(dd.symbol, ctx.owner)
          range match
            case FilteredRange(range, filters) =>
              Some(FilteredRange(range, filters :+ (iter, updatedRHS)))
            case _ => Some(FilteredRange(range, List((iter, updatedRHS))))
        case _ => Some(FilteredRange(tree, List()))
  end FilteredRange

  case class Foreach(
      iter: ValDef,
      range: Tree,
      filters: List[(ValDef, Tree)],
      body: Tree,
      dfc: Tree
  )
  object Foreach:
    def unapply(tree: Tree)(using Context): Option[Foreach] =
      tree match
        case Apply(
              Apply(
                TypeApply(Select(FilteredRange(range, filters), foreach), List(_)),
                List(
                  Block(
                    List(
                      dd @ DefDef(
                        anonfun,
                        List(List(iter: ValDef)),
                        _,
                        _
                      )
                    ),
                    _: Closure
                  )
                )
              ),
              List(dfcTree)
            ) if anonfun.toString.startsWith("$anonfun") && foreach.toString == "foreach" =>
          val updatedRHS = dd.rhs.changeOwner(dd.symbol, ctx.owner)
          Some(Foreach(iter, range, filters, updatedRHS, dfcTree))
        case _ =>
          None
  end Foreach

  override def transformWhileDo(tree: WhileDo)(using Context): Tree =
    dfcStack.headOption.map { dfc =>
      val guard = tree.cond match
        case Apply(Apply(Ident(n), List(dfCond)), List(_)) if n.toString == "BooleanHack" =>
          dfCond
        case cond => ref(fromBooleanSym).appliedTo(cond).appliedTo(dfc)
      ref(customWhileSym).appliedTo(guard).appliedTo(tree.body).appliedTo(dfc)
    }.getOrElse(tree)

  override def transformApply(tree: Apply)(using Context): Tree =
    tree match
      case fe @ Foreach(iter, range, filters, body, dfc) =>
        val loopIter =
          ref(getLoopIterSym)
            .appliedToType(iter.dfValTpeOpt.get.widen)
            .appliedTo(iter.genMeta)
            .appliedTo(dfc)
        val ifGuards = mkList(filters.map { case (iter, filter) =>
          val guard = replaceArgs(filter.underlying, Map(iter.symbol -> loopIter))
          ref(toFunc1Sym)
            .appliedToType(guard.tpe)
            .appliedTo(guard)
        })
        val updatedBody = replaceArgs(body, Map(iter.symbol -> loopIter))
        ref(customForSym)
          .appliedTo(iter.genMeta, range, ifGuards)
          .appliedTo(updatedBody)
          .appliedTo(dfc)
      case _ =>
        tree

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    trees.view.map {
      case dd: DefDef =>
        dd.rhs match
          case Step(dfcTree) =>
            val addStepRunTree =
              ref(addStepSym).appliedTo(ref(dd.symbol)).appliedTo(dfcTree)
            List(dd, addStepRunTree)
          case _ =>
            Some(dd)
      // disallow anonymous steps
      case stepTree @ Step(_) =>
        report.error(
          "Unexpected anonymous step pattern. Must be `def xyz = step`",
          stepTree.srcPos
        )
        Nil
      case tree => Some(tree)
    }.flatten.toList

  // disallow step to be a val
  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if (tree.tpe <:< stepRef)
      report.error("A step must have a `def` modifier", tree.srcPos)
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    stepRef = requiredClassRef("dfhdl.core.Step")
    addStepSym = requiredMethod("dfhdl.core.r__For_Plugin.addStep")
    customForSym = requiredMethod("dfhdl.core.DFFor.plugin")
    getLoopIterSym = requiredMethod("dfhdl.core.DFFor.pluginGetLoopIter")
    toFunc1Sym = requiredMethod("dfhdl.core.r__For_Plugin.toFunc1")
    fromBooleanSym = requiredMethod("dfhdl.core.r__For_Plugin.fromBoolean")
    customWhileSym = requiredMethod("dfhdl.core.DFWhile.plugin")
    ctx
  end prepareForUnit
end LoopFSMPhase
