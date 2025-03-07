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

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

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

  case class Foreach(iter: ValDef, range: Tree, body: Tree, dfc: Tree)
  object Foreach:
    def unapply(tree: Tree)(using Context): Option[Foreach] =
      tree match
        case Apply(
              Apply(
                TypeApply(Select(rangeTree, foreach), List(_)),
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
          Some(Foreach(iter, rangeTree, dd.rhs, dfcTree))
        case _ =>
          None
  end Foreach

  override def transformApply(tree: Apply)(using Context): Tree =
    tree match
      case fe @ Foreach(iter, range, body, dfc) =>
        val loopIter =
          ref(getLoopIterSym)
            .appliedToType(iter.dfValTpeOpt.get.widen)
            .appliedTo(iter.genMeta)
            .appliedTo(dfc)
        val updatedBody = replaceArgs(body, Map(iter.symbol -> loopIter))
        ref(customForSym).appliedTo(iter.genMeta, range).appliedTo(updatedBody).appliedTo(dfc)
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
    ctx
  end prepareForUnit
end LoopFSMPhase
