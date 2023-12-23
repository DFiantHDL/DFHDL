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
  var metaGenSym: Symbol = uninitialized

  extension (tpe: Type)(using Context)
    def dfValTpeOpt: Option[Type] =
      tpe.dealias match
        case res if res.dealias.typeSymbol == dfValSym => Some(res)
        case _                                         => None

  extension (tree: ValOrDefDef)(using Context)
    def dfValTpeOpt: Option[Type] =
      tree.tpt.tpe.dfValTpeOpt

  extension (tree: ValOrDefDef)(using Context)
    def genMeta: Tree =
      val nameOptTree = mkOptionString(Some(tree.name.toString.nameCheck(tree)))
      val positionTree = tree.srcPos.positionTree
      val docOptTree = mkOptionString(tree.symbol.docString)
      val annotTree = mkList(tree.symbol.annotations.map(_.tree))
      ref(metaGenSym).appliedToArgs(
        nameOptTree :: positionTree :: docOptTree :: annotTree :: Nil
      )
  end extension

  // DFHDL design construction from definitions transformation.
  // Such transformation rely on code like `def foo(arg: Bit <> VAL): Bit <> VAL`
  // The `Bit <> VAL` type is a match type that is manifests as `DFC ?=> DFValOf[Bit]`.
  override def transformDefDef(tree: DefDef)(using Context): tpd.Tree =
    val sym = tree.symbol
    lazy val dfValArgs = tree.paramss.view.flatten.collect {
      case vd: ValDef if vd.dfValTpeOpt.nonEmpty => vd
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

        // replacing the old arg references according to the argument map
        def replaceArgs(expr: Tree, argMap: Map[Symbol, Tree]): Tree =
          val replacer = new TreeMap():
            override def transform(tree: Tree)(using Context): Tree =
              tree match
                case id @ Ident(_) if argMap.contains(id.symbol) =>
                  argMap(id.symbol)
                case _ => super.transform(tree)
          replacer.transform(expr)

        val updatedAnonRHS: Tree =
          // list of tuples of the old arguments and their meta data
          val args = mkList(dfValArgs.map(a => mkTuple(List(a.ident, a.genMeta))))
          // input map to replace old arg references with new input references
          val inputMap = dfValArgs.view.zipWithIndex.map((a, i) =>
            a.symbol -> ref(designFromDefGetInputSym)
              .appliedToType(a.dfValTpeOpt.get.widen)
              .appliedTo(Literal(Constant(i)))
              .appliedTo(dfc)
          ).toMap
          // calling the runtime method that constructs the design from the definition
          ref(designFromDefSym)
            .appliedToType(anonDef.dfValTpeOpt.get.widen)
            .appliedToArgs(List(args, tree.genMeta)) // meta represents the transformed tree
            .appliedTo(replaceArgs(anonDef.rhs, inputMap))
            .appliedTo(dfc)
        end updatedAnonRHS
        val updatedAnonDef = cpy.DefDef(anonDef)(rhs = updatedAnonRHS)
        val updatedRHS = Block(List(updatedAnonDef), closure)
        cpy.DefDef(tree)(rhs = updatedRHS)
      case _ =>
        if (!sym.isAnonymousFunction && !sym.is(Exported) && !sym.isConstructor)
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
    metaGenSym = requiredMethod("dfhdl.compiler.ir.Meta.gen")
    designFromDefSym = requiredMethod("dfhdl.core.__For_Plugin.designFromDef")
    designFromDefGetInputSym = requiredMethod("dfhdl.core.__For_Plugin.designFromDefGetInput")
    super.prepareForUnit(tree)
    ctx
end DesignDefsPhase
