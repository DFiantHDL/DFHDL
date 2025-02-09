package dfhdl.plugin

import dotty.tools.dotc._

import plugins._

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import ast.{tpd, untpd, TreeTypeMap}
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable
import annotation.tailrec

class DesignDefsPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "DesignDefs"
  // override val debugFilter: String => Boolean = _.contains("Example.scala")

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  var designFromDefSym: Symbol = uninitialized
  var designFromDefGetInputSym: Symbol = uninitialized

  // DFHDL design construction from definitions transformation.
  // Such transformation rely on code like `def foo(arg: Bit <> VAL): Bit <> VAL`
  // The `Bit <> VAL` type is a match type that manifests as `DFC ?=> DFValOf[Bit]`.
  override def transformDefDef(tree: DefDef)(using Context): tpd.Tree =
    val sym = tree.symbol
    lazy val dfValArgs = tree.paramss.view.flatten.collect {
      case vd: ValDef if vd.dfValTpeOpt.nonEmpty && !vd.tpt.tpe.isDFConst => vd
    }.toList
    lazy val dfConstValArgs = tree.paramss.view.flatten.collect {
      case vd: ValDef if vd.dfValTpeOpt.nonEmpty && vd.tpt.tpe.isDFConst => vd
    }.toList
    tree.rhs match
      case Block(List(anonDef: DefDef), closure: Closure)
          if (
            // We ignore inline method, since these should not be transformed into
            // design hierarchies.
            // We also ignore exported methods, to prevent transforming a method that
            // was already transformed at its origin.
            !tree.isInline && !sym.is(Exported) &&
              // transform only methods that return a DFHDL value and
              // have at least one DFHDL parameter and
              // have a context argument
              anonDef.dfValTpeOpt.nonEmpty && dfValArgs.nonEmpty
          ) =>
        debug("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        debug(tree.show)
        val dfc = ContextArg.at(anonDef).get

        val updatedAnonRHS: Tree =
          // list of tuples of the old arguments and their meta data
          val args = mkList(dfValArgs.map(a => mkTuple(List(a.ident, a.genMeta))))
          // input map to replace old arg references with new input references
          val inputMap = mutable.Map.empty[Symbol, Tree]
          dfValArgs.view.zipWithIndex.foreach((a, i) =>
            inputMap +=
              a.symbol -> ref(designFromDefGetInputSym)
                .appliedToType(a.dfValTpeOpt.get.widen)
                .appliedTo(Literal(Constant(i)))
                .appliedTo(dfc)
          )
          // constant parameter generation
          val designParamGenValDefs: List[ValDef] = inContext(ctx.withOwner(anonDef.symbol)) {
            dfConstValArgs.map { v =>
              val valDef = v.genDesignParamValDef(None, dfc)
              inputMap += v.symbol -> ref(valDef.symbol)
              valDef
            }
          }
          // updated body with the extra design parameter definition after replacing parameter references
          val updatedBody = replaceArgs(anonDef.rhs, inputMap.toMap) match
            case Block(stats, expr) => Block(designParamGenValDefs ++ stats, expr)
            case simpleTree         => Block(designParamGenValDefs, simpleTree)
          // calling the runtime method that constructs the design from the definition
          ref(designFromDefSym)
            .appliedToType(anonDef.dfValTpeOpt.get.widen)
            .appliedToArgs(List(args, tree.genMeta)) // meta represents the transformed tree
            .appliedTo(updatedBody)
            .appliedTo(dfc)
        end updatedAnonRHS
        val updatedAnonDef = cpy.DefDef(anonDef)(rhs = updatedAnonRHS)
        val updatedRHS = Block(List(updatedAnonDef), closure)
        cpy.DefDef(tree)(rhs = updatedRHS)
      case _ =>
        if (
          // ignoring anonymous functions (since they are not transformed into design hierarchies)
          // and ignoring exported methods (to prevent transforming a method that was already transformed at its origin)
          // and ignoring mutable methods (that are just a reference to a mutable variable)
          // and ignoring constructors definitions (since they are not transformed into design hierarchies)
          !sym.isAnonymousFunction && !sym.is(Exported) && !sym.is(Mutable) &&
          !sym.isConstructor && !sym.owner.isAnonymousClass
        )
          if (
            (tree.dfValTpeOpt.nonEmpty || tree.tpt.tpe =:= defn.UnitType) && dfValArgs.nonEmpty &&
            !sym.ignoreMetaContext
          )
            report.error(
              "Must use a `<> DFRET` modifier for a DFHDL function return type.",
              tree.tpt.srcPos
            )
        tree
    end match
  end transformDefDef

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if (tree.tpt.tpe.dfcFuncTpeOpt.flatMap(_.dfValTpeOpt).nonEmpty)
      report.error(
        "A DFHDL value/argument must have a `<> VAL` modifier.",
        tree.tpt.srcPos
      )
    ctx

  override def prepareForUnit(tree: Tree)(using Context): Context =
    designFromDefSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDef")
    designFromDefGetInputSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefGetInput")
    super.prepareForUnit(tree)
    ctx
end DesignDefsPhase
