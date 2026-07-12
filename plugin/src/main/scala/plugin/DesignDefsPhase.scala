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
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  var designFromDefSym: Symbol = uninitialized
  var designFromDefGetInputSym: Symbol = uninitialized
  var designFromDefGetParamSym: Symbol = uninitialized

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
    lazy val scalaValArgs = tree.paramss.view.flatten.collect {
      case vd: ValDef if vd.dfValTpeOpt.isEmpty && !vd.tpt.tpe.isMetaContext => vd
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
        // out-of-scope value references become explicit: DFHDL constants as phantom design
        // parameters, DFHDL values as phantom input ports (both tagged so the design-def
        // view form hides them), and plain Scala values as cache key extensions. All are
        // evaluated in the def's rhs scope at every call, so a pure cache hit (which skips
        // the body) still binds this call's captured values.
        val captures = discoverDesignDefCaptures(sym, anonDef.symbol, anonDef.rhs)
        // the runtime names phantoms after the captured values, so a name clash would
        // misbind the design's parameter map
        locally:
          val explicitNames = (dfValArgs.view ++ dfConstValArgs.view).map(_.name.toString).toSet
          val seen = mutable.Set.empty[String]
          (captures.phantomConsts.view ++ captures.phantomVals.view).foreach { (path, t) =>
            val name = captureName(path)
            if (explicitNames.contains(name) || !seen.add(name))
              report.error(
                s"""Ambiguous captured value name `$name` in a DFHDL method.
                   |Every captured external value must have a name distinct from the method's arguments and from other captured values.""".stripMargin,
                t.srcPos
              )
          }

        val updatedAnonRHS: Tree =
          // the plugin-side fallback meta of a captured value: its (leaf) name and
          // DECLARATION position (a reference position may originate from inlined library
          // code). The runtime prefers the applied value's own meta and uses this fallback
          // only for anonymous applied values.
          def genCapturedMeta(path: List[Symbol], t: Tree): Tree =
            ref(metaGenSym).appliedToArgs(
              mkOptionString(Some(captureName(path))) :: t.symbol.srcPos.positionTree ::
                mkOptionString(None) :: mkList(Nil) :: Nil
            )
          // list of tuples of the old arguments and their meta data
          val args = mkList(dfValArgs.map(a => mkTuple(List(a.ident, a.genMeta))))
          // list of (name, applied value, meta) tuples of the `<> CONST` arguments — the
          // design parameters as applied at the call site. The harness (`designFromDef`)
          // creates the design parameters from these, outside the body, so a pure cache hit
          // that skips the body still binds fresh parameters to this call's applied values.
          val constArgs = mkList(dfConstValArgs.map(v =>
            mkTuple(List(Literal(Constant(v.name.toString.nameCheck(v))), v.ident, v.genMeta))
          ))
          // list of the plain Scala (non-DFHDL) argument values and captured Scala values.
          // A pure body may legitimately depend on them, so they are part of the pure
          // cache key.
          val scalaArgs = mkList(
            scalaValArgs.map(_.ident) ++ captures.scalaCaptures.map(_._2),
            Some(defn.AnyType)
          )
          // (value, fallback meta) tuples of the phantom captures
          val phantomArgs = mkList(
            captures.phantomVals.map((path, t) => mkTuple(List(t, genCapturedMeta(path, t))))
          )
          val phantomConstArgs = mkList(
            captures.phantomConsts.map((path, t) => mkTuple(List(t, genCapturedMeta(path, t))))
          )
          // input map to replace old arg references with new input references
          val inputMap = mutable.Map.empty[Symbol, Tree]
          dfValArgs.view.zipWithIndex.foreach((a, i) =>
            inputMap +=
              a.symbol -> ref(designFromDefGetInputSym)
                .appliedToType(a.dfValTpeOpt.get.widen)
                .appliedTo(Literal(Constant(i)))
                .appliedTo(dfc)
          )
          // constant parameter references are rewired to fetch the harness-created design
          // parameters by index
          dfConstValArgs.view.zipWithIndex.foreach((v, i) =>
            inputMap +=
              v.symbol -> ref(designFromDefGetParamSym)
                .appliedToType(v.tpt.tpe)
                .appliedTo(Literal(Constant(i)))
                .appliedTo(dfc)
          )
          // phantom capture rewiring (path-keyed, since captures are keyed by their full
          // stable access path): captured values become inputs appended after the explicit
          // arguments and captured constants become design parameters appended after the
          // explicit const parameters, sharing the harness accessors' index spaces
          val phantomReplaceMap = mutable.Map.empty[List[Symbol], Tree]
          captures.phantomVals.view.zipWithIndex.foreach { case ((path, t), i) =>
            phantomReplaceMap += path -> ref(designFromDefGetInputSym)
              .appliedToType(t.tpe.widen.dfValTpeOpt.get)
              .appliedTo(Literal(Constant(dfValArgs.length + i)))
              .appliedTo(dfc)
          }
          captures.phantomConsts.view.zipWithIndex.foreach { case ((path, t), i) =>
            phantomReplaceMap += path -> ref(designFromDefGetParamSym)
              .appliedToType(t.tpe.widen.dfValTpeOpt.get)
              .appliedTo(Literal(Constant(dfConstValArgs.length + i)))
              .appliedTo(dfc)
          }
          object phantomReplacer extends TreeMap:
            override def transform(t: Tree)(using Context): Tree = t match
              case _: (Ident | Select) =>
                stablePathKey(t).flatMap(phantomReplaceMap.get) match
                  case Some(replacement) => replacement
                  case None              => super.transform(t)
              case _ => super.transform(t)
          val bodyAfterPhantoms =
            if (phantomReplaceMap.isEmpty) anonDef.rhs
            else phantomReplacer.transform(anonDef.rhs)
          // updated body after replacing parameter references
          val updatedBody = replaceArgs(bodyAfterPhantoms, inputMap.toMap)
          // calling the runtime method that constructs the design from the definition
          ref(designFromDefSym)
            .appliedToType(anonDef.dfValTpeOpt.get.widen)
            .appliedToArgs(List(
              args,
              constArgs,
              tree.genMeta, // meta represents the transformed tree
              scalaArgs,
              phantomArgs,
              phantomConstArgs
            ))
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
    designFromDefGetParamSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefGetParam")
    super.prepareForUnit(tree)
    ctx
end DesignDefsPhase
