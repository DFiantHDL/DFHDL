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
  var registerStepSym: Symbol = uninitialized
  var addStepSym: Symbol = uninitialized
  var gotoStepSym: Symbol = uninitialized
  var customForSym: Symbol = uninitialized
  var getLoopIterSym: Symbol = uninitialized
  var toFunc1Sym: Symbol = uninitialized
  var fromBooleanSym: Symbol = uninitialized
  var customWhileSym: Symbol = uninitialized
  var processAnonDefSym: Symbol = uninitialized
  var processScopeCtxSym: Symbol = uninitialized
  var dfcStack: List[Tree] = Nil
  val processStepDefs = mutable.LinkedHashMap.empty[Symbol, DefDef]

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("CustomControl")

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    ContextArg.at(tree).foreach { t =>
      dfcStack = t :: dfcStack
    }
    ctx

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val updatedDefDef =
      if (tree.symbol == processAnonDefSym)
        val registeredSteps = processStepDefs.view.map { (sym, dd) =>
          ref(registerStepSym)
            .appliedTo(dd.genMeta)
            .appliedTo(dfcStack.head, ref(processScopeCtxSym))
        }.toList
        val updatedRHS = tree.rhs match
          case Block(stats, expr) => Block(registeredSteps ++ stats, expr)
          case expr               => Block(registeredSteps, expr)
        cpy.DefDef(tree)(rhs = updatedRHS)
      else tree
    ContextArg.at(tree).foreach { t =>
      dfcStack = dfcStack.drop(1)
    }
    updatedDefDef
  end transformDefDef

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

  def allDefsErrMsg(srcPos: util.SrcPos)(using Context): Unit =
    report.error("Process blocks must only declare `def`s or no `def`s at all.", srcPos)

  def processStatCheck(tree: Tree, returnCheck: Boolean)(using Context): Unit =
    tree match
      case Block(stats, expr) =>
        processStatCheck(stats, tree.srcPos)
        expr match
          case Literal(Constant(_: Unit)) =>
          case _ =>
            stats.headOption match
              case Some(dd: DefDef) =>
                allDefsErrMsg(expr.srcPos)
              case _ =>
                processStatCheck(expr, returnCheck)
      case If(cond, thenp, elsep) =>
        processStatCheck(thenp, returnCheck)
        processStatCheck(elsep, returnCheck)
      case Match(scrut, cases) =>
        cases.foreach { case CaseDef(pat, guard, body) =>
          processStatCheck(body, returnCheck)
        }
      case _ if returnCheck =>
        tree match
          case x @ Ident(stepName) if processStepDefs.contains(x.symbol)           =>
          case Apply(x @ Ident(stepName), _) if processStepDefs.contains(x.symbol) =>
          case _ =>
            report.error(
              "Process `def`s must end with a call to a process `def` (it could be the same `def`).",
              tree.srcPos
            )
      case Foreach(_, _, _, body, _) =>
        processStatCheck(body, returnCheck = false)
      case WhileDo(_, body) =>
        processStatCheck(body, returnCheck = false)
      case _ =>

  def processStatCheck(trees: List[Tree], srcPos: util.SrcPos)(using Context): Unit =
    val (allDefs: List[DefDef] @unchecked, allStepBlocks: List[Tree]) = (trees.partition {
      case _: DefDef => true
      case _         => false
    }): @unchecked
    if (allDefs.nonEmpty && allStepBlocks.nonEmpty) allDefsErrMsg(srcPos)

    var errFound = false
    // checking process defs syntax and caching the process def symbols
    allDefs.foreach {
      case dd @ DefDef(_, Nil, retTypeTree, _) if retTypeTree.tpe =:= defn.UnitType =>
        processStepDefs += (dd.symbol -> dd)
      case dd =>
        report.error("Unexpected process `def` syntax. Must be `def xyz: Unit = ...`", dd.srcPos)
        errFound = true
    }
    if (!errFound)
      allDefs.foreach { dd => processStatCheck(dd.rhs, returnCheck = true) }
      allStepBlocks.foreach { step => processStatCheck(step, returnCheck = false) }
  end processStatCheck

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

  case class ProcessForever(scopeCtx: ValDef, block: Tree)
  object ProcessForever:
    def unapply(tree: Tree)(using Context): Option[ProcessForever] =
      tree match
        case Apply(
              Apply(
                Select(Ident(process), forever),
                List(
                  Block(
                    List(dd @ DefDef(anonfun, List(List(scopeCtx: ValDef)), _, _)),
                    _: Closure
                  )
                )
              ),
              _
            )
            if anonfun.toString.startsWith("$anonfun") && process.toString == "process" && forever
              .toString == "forever" =>
          processAnonDefSym = dd.symbol
          processScopeCtxSym = scopeCtx.symbol
          Some(ProcessForever(scopeCtx, dd.rhs))
        case _ =>
          None
  end ProcessForever

  override def prepareForApply(tree: Apply)(using Context): Context =
    tree match
      case ProcessForever(scopeCtx, block) =>
        processStatCheck(block, returnCheck = false)
        processStepDefs.view.groupBy(_._2.name.toString).foreach { case (name, defs) =>
          if (defs.size > 1)
            report.error(
              s"Process step `def`s must be unique. Found multiple `def $name: Unit = ...`s.",
              defs.head._2.srcPos
            )
        }
        ctx
      case _ =>
    ctx

  override def transformIdent(tree: Ident)(using Context): Tree =
    if (processStepDefs.contains(tree.symbol))
      ref(gotoStepSym)
        .appliedTo(Literal(Constant(tree.name.toString)))
        .appliedTo(dfcStack.head, ref(processScopeCtxSym))
    else tree

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
      case ProcessForever(scopeCtx, block) =>
        processStepDefs.clear()
        tree
      case _ =>
        tree

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    trees.map {
      case dd: DefDef if processStepDefs.contains(dd.symbol) =>
        ref(addStepSym)
          .appliedTo(Literal(Constant(dd.name.toString)))
          .appliedTo(dd.rhs.changeOwner(dd.symbol, ctx.owner))
          .appliedTo(dfcStack.head, ref(processScopeCtxSym))
      case tree => tree
    }

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    registerStepSym = requiredMethod("dfhdl.core.Step.pluginRegisterStep")
    addStepSym = requiredMethod("dfhdl.core.Step.pluginAddStep")
    gotoStepSym = requiredMethod("dfhdl.core.Step.pluginGotoStep")
    customForSym = requiredMethod("dfhdl.core.DFFor.plugin")
    getLoopIterSym = requiredMethod("dfhdl.core.DFFor.pluginGetLoopIter")
    toFunc1Sym = requiredMethod("dfhdl.core.r__For_Plugin.toFunc1")
    fromBooleanSym = requiredMethod("dfhdl.core.r__For_Plugin.fromBoolean")
    customWhileSym = requiredMethod("dfhdl.core.DFWhile.plugin")
    processStepDefs.clear()
    ctx
  end prepareForUnit
end LoopFSMPhase
