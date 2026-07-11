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
  var designFromDefEDSym: Symbol = uninitialized
  var designFromDefGetInputSym: Symbol = uninitialized
  var scopeFunctionSym: Symbol = uninitialized
  var scopeProceduralSym: Symbol = uninitialized
  var pureAnnotSym: Symbol = uninitialized

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
    // ED methods (HDL functions/tasks) are detected by the scope evidence parameter that
    // the `<> EDRET` match type injects into the context lambda: `Scope.Function` for
    // functions (non-Unit return) and `Scope.Procedural` for procedural methods (Unit).
    def edScopeKindOf(anonDef: DefDef): Option[Boolean] =
      anonDef.paramss.flatten.collectFirst {
        case vd: ValDef if vd.tpe <:< scopeFunctionSym.typeRef   => true
        case vd: ValDef if vd.tpe <:< scopeProceduralSym.typeRef => false
      }
    tree.rhs match
      case Block(List(anonDef: DefDef), closure: Closure)
          if (
            // We ignore inline method, since these should not be transformed into
            // design hierarchies.
            // We also ignore exported methods, to prevent transforming a method that
            // was already transformed at its origin.
            !tree.isInline && !sym.is(Exported) &&
              // transform only methods that return a DFHDL value and
              // have a context argument and
              // have at least one DFHDL parameter (ED methods may have none)
              anonDef.dfValTpeOpt.nonEmpty &&
              (dfValArgs.nonEmpty || edScopeKindOf(anonDef).nonEmpty)
          ) =>
        debug("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        debug(tree.show)
        val edScopeKind = edScopeKindOf(anonDef)
        val isED = edScopeKind.nonEmpty
        var hasEDErrors = false
        def edError(msg: String): Unit =
          report.error(msg, tree.srcPos)
          hasEDErrors = true
        if (isED)
          // an explicit — possibly empty `()` — term parameter block is required, so that
          // ED method call sites always read as calls
          val hasTermParamBlock = tree.paramss.exists { clause =>
            clause.isEmpty || clause.headOption.exists {
              case vd: ValDef => !vd.symbol.is(Given)
              case _          => false
            }
          }
          if (!hasTermParamBlock)
            edError(
              "An ED method must declare an explicit parameter block. Use an empty `()` parameter block if the method has no arguments."
            )
          // direct recursion cannot be modeled (an ED method is a self-contained design
          // hierarchy that cannot contain itself)
          var hasRecursion = false
          object recursionFinder extends TreeTraverser:
            def traverse(t: Tree)(using Context): Unit = t match
              case _: (Ident | Select) if t.symbol == sym => hasRecursion = true
              case _                                      => traverseChildren(t)
          recursionFinder.traverse(anonDef.rhs)
          if (hasRecursion)
            edError("Recursion is not allowed for ED methods.")
        end if
        if (hasEDErrors) tree
        else
          val dfc = ContextArg.at(anonDef).get
          // --- ED phantom capture (see the ed-methods plan) ---
          // Free DFHDL-value stable references in the body become phantom input arguments:
          // they are evaluated inside the def at call time (so call sites are never
          // transformed), connected in the caller's scope like explicit arguments, and
          // hidden by the HDL printers. Keyed by the full stable path — the same member
          // symbol reached through different instance paths must not unify.
          def stablePathKey(t: Tree): Option[List[Symbol]] = t match
            case id: Ident if id.symbol.exists && id.symbol.isTerm => Some(List(id.symbol))
            case th: This                                          => Some(List(th.symbol))
            case sel @ Select(qual, _) if sel.symbol.exists && sel.symbol.isTerm =>
              stablePathKey(qual).map(sel.symbol :: _)
            case _ => None
          // both non-const values (phantom input ports) and constants (phantom design
          // parameters) are captured. NOTE: the type must be widened before the DFHDL-value
          // test — a member with an explicit `<> ...` type annotation carries the unreduced
          // match-type alias on its TermRef (unlike inferred-type members)
          def isCapturable(t: Tree): Boolean =
            t.tpe.isStable && t.tpe.widen.dfValTpeOpt.nonEmpty &&
              stablePathKey(t).exists { path =>
                val root = path.last
                // rooted at `this` — an enclosing container member is capturable;
                // static (global) values are reachable everywhere and never captured;
                // the def's own parameters and body locals are not captures
                if (root.isClass) true
                else
                  !root.isStatic &&
                  !root.ownersIterator.exists(o => o == sym || o == anonDef.symbol)
              }
          val capturedPaths = mutable.LinkedHashMap.empty[List[Symbol], Tree]
          if (isED)
            object captureFinder extends TreeTraverser:
              def traverse(t: Tree)(using Context): Unit = t match
                case _: (Ident | Select) if isCapturable(t) =>
                  capturedPaths.getOrElseUpdate(stablePathKey(t).get, t)
                case _ => traverseChildren(t)
            captureFinder.traverse(anonDef.rhs)
          val updatedAnonRHS: Tree =
            def genCapturedMeta(t: Tree): Tree =
              // the captured symbol's declaration position is used — a reference position
              // may originate from inlined library code (e.g. Exact conversions)
              ref(metaGenSym).appliedToArgs(
                mkOptionString(Some(t.symbol.name.toString)) :: t.symbol.srcPos.positionTree ::
                  mkOptionString(None) :: mkList(Nil) :: Nil
              )
            // captured constants become phantom design parameters; captured values become
            // phantom input ports
            val (phantomConstTrees, phantomArgTrees) =
              capturedPaths.values.toList.partition(_.tpe.isDFConst)
            // list of tuples of the old arguments and their meta data
            val args = mkList(dfValArgs.map(a => mkTuple(List(a.ident, a.genMeta))))
            // list of (name, applied value) tuples of the `<> CONST` arguments and the
            // phantom-captured constants — the design parameters as applied at the call
            // site, used for the design's `paramMap` (explicit application keeps `@hw.pure`
            // memoization sound: applied values flow per call, never recovered implicitly)
            val constArgs = mkList(
              dfConstValArgs.map(v =>
                mkTuple(List(Literal(Constant(v.name.toString.nameCheck(v))), v.ident))
              ) ++
                phantomConstTrees.map(t =>
                  mkTuple(List(Literal(Constant(t.symbol.name.toString)), t))
                )
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
            // constant parameter generation
            val designParamGenValDefs: List[ValDef] = inContext(ctx.withOwner(anonDef.symbol)) {
              dfConstValArgs.map { v =>
                val valDef = v.genContainerParamValDef(None, dfc)
                inputMap += v.symbol -> ref(valDef.symbol)
                valDef
              }
            }
            // phantom capture rewrite: captured values become extra inputs appended after
            // the explicit arguments; captured constants become explicit (tagged) phantom
            // design parameters; body occurrences are replaced (path-keyed) by the
            // corresponding accessors
            val phantomReplaceMap = mutable.Map.empty[List[Symbol], Tree]
            phantomArgTrees.view.zipWithIndex.foreach { (t, j) =>
              phantomReplaceMap += stablePathKey(t).get -> ref(designFromDefGetInputSym)
                .appliedToType(t.tpe.widen.dfValTpeOpt.get)
                .appliedTo(Literal(Constant(dfValArgs.length + j)))
                .appliedTo(dfc)
            }
            val phantomParamGenValDefs: List[ValDef] =
              inContext(ctx.withOwner(anonDef.symbol)) {
                phantomConstTrees.map { t =>
                  val valDef = genContainerParamValDefImpl(
                    t, t.tpe.widen.dfValTpeOpt.get, t.symbol.name.toString, genCapturedMeta(t),
                    None, dfc, phantom = true
                  )
                  phantomReplaceMap += stablePathKey(t).get -> ref(valDef.symbol)
                  valDef
                }
              }
            val phantomArgs =
              mkList(phantomArgTrees.map(t => mkTuple(List(t, genCapturedMeta(t)))))
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
            // updated body with the extra design parameter definitions after replacing
            // parameter references
            val allParamGenValDefs = designParamGenValDefs ++ phantomParamGenValDefs
            val updatedBody = replaceArgs(bodyAfterPhantoms, inputMap.toMap) match
              case Block(stats, expr) => Block(allParamGenValDefs ++ stats, expr)
              case simpleTree         => Block(allParamGenValDefs, simpleTree)
            // calling the runtime method that constructs the design from the definition.
            // ED methods are `@hw.pure` by default; the plugin checks for an explicit
            // `@hw.pure` annotation (active or not) to let the user opt out.
            val fromDefApply =
              if (isED)
                ref(designFromDefEDSym)
                  .appliedToType(anonDef.dfValTpeOpt.get.widen)
                  .appliedToArgs(List(
                    args,
                    phantomArgs,
                    constArgs,
                    Literal(Constant(!sym.hasAnnotation(pureAnnotSym))),
                    tree.genMeta
                  )) // meta represents the transformed tree
              else
                ref(designFromDefSym)
                  .appliedToType(anonDef.dfValTpeOpt.get.widen)
                  .appliedToArgs(List(
                    args,
                    constArgs,
                    tree.genMeta
                  )) // meta represents the transformed tree
            fromDefApply
              .appliedTo(updatedBody)
              .appliedTo(dfc)
          end updatedAnonRHS
          val updatedAnonDef = cpy.DefDef(anonDef)(rhs = updatedAnonRHS)
          val updatedRHS = Block(List(updatedAnonDef), closure)
          cpy.DefDef(tree)(rhs = updatedRHS)
        end if
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
    designFromDefEDSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefED")
    designFromDefGetInputSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefGetInput")
    scopeFunctionSym = requiredClass("dfhdl.core.DFC.Scope.Function")
    scopeProceduralSym = requiredClass("dfhdl.core.DFC.Scope.Procedural")
    pureAnnotSym = requiredClass("dfhdl.hw.annotation.pure")
    super.prepareForUnit(tree)
    ctx
end DesignDefsPhase
