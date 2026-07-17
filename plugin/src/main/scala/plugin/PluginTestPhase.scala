package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Decorators.*
import Constants.Constant
import Types.*
import Phases.*
import ast.{tpd, untpd}
import parsing.Parsers.Parser
import typer.{ConstFold, Typer}
import transform.MegaPhase
import transform.MegaPhase.MiniPhase
import util.SourceFile

/** Replaces `dfhdl.internals.PluginErrCheck.pluginCheckErrors(code)` calls with the literal list of
  * error messages that compiling `code` produces once the DFHDL plugin phases have run on it. This
  * is the plugin-phase counterpart of the `compiletime.testing.typeCheckErrors` intrinsic, whose
  * nested-compilation pipeline it mirrors (`Inlines.Intrinsics.compileForErrors`), and it exists so
  * munit specs can assert on diagnostics that only plugin phases emit. It is instantiated only
  * under `-P:dfhdl.plugin:testing` (see devdocs/plugin-error-testing.md).
  */
class PluginTestPhase(setting: Setting) extends CommonPhase:
  import tpd.*

  val phaseName = "PluginErrCheck"

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  private var markerClass: Symbol = NoSymbol
  private val preTyperRewriter = new PreTyperPhase(setting)

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    // The marker lives in internals TEST sources, so it exists only in DFHDL's own test
    // compilations; in any other compilation that enables this phase it stays inert.
    markerClass = getModuleIfDefined("dfhdl.internals.PluginErrCheck").moduleClass
    ctx

  override def transformApply(tree: Apply)(using Context): Tree =
    val sym = tree.symbol
    if (
      markerClass.exists && sym.name.toString == "pluginCheckErrors" &&
      sym.maybeOwner == markerClass
    ) replaceMarker(tree)
    else tree

  private def stripTyped(t: Tree): Tree = t match
    case Typed(t2, _)        => stripTyped(t2)
    case Block(Nil, t2)      => stripTyped(t2)
    case Inlined(_, Nil, t2) => stripTyped(t2)
    case _                   => t

  private def constString(arg: Tree)(using Context): Option[String] =
    ConstFold(stripTyped(arg.underlying)).tpe.widenTermRefExpr.dealias match
      case ConstantType(Constant(str: String)) => Some(str)
      case _                                   => None

  private def replaceMarker(tree: Apply)(using Context): Tree =
    tree.args match
      case codeArg :: Nil =>
        constString(codeArg) match
          case Some(code) =>
            val errors = snippetErrors(code)
            mkList(errors.map(msg => Literal(Constant(msg))), Some(defn.StringType))
              .withSpan(tree.span)
          case None =>
            // The retained rhs of an inline helper (e.g. `assertPluginError`) contains a
            // marker call whose argument is the inline parameter rather than a constant. That
            // copy is never executed (each call site is replaced on its inlined,
            // constant-argument copy), so it is left as is.
            if (!ctx.owner.ownersIterator.exists(_.is(Inline)))
              report.error(
                "The `code` argument of `pluginCheckErrors` must be a statically known String.",
                codeArg.srcPos
              )
            tree
      case _ => tree

  // The installed (scheduled) phases from the typer onward, by name. `ContextBase.phases`
  // is not accessible outside the compiler, so the chain is walked via the public
  // `Phase.next` instead.
  private def installedPhaseMap(using Context): Map[String, Phase] =
    val b = Map.newBuilder[String, Phase]
    var p: Phase = ctx.base.typerPhase
    while (p.exists)
      b += p.phaseName -> p
      p = p.next
    b.result()

  // Fresh nested instances of this plugin's typed phases (their per-unit mutable state
  // forbids reusing the installed ones). PreTyper is applied separately on the parse tree,
  // CodeDigest is elaboration-caching bookkeeping with no diagnostics, and this phase itself
  // must not recurse into snippets.
  private def freshPluginPhases: List[PluginPhase] = List(
    FlattenInlinedPhase(setting),
    TopAnnotPhase(setting),
    PureCheckPhase(setting),
    MetaContextPlacerPhase(setting),
    LoopFSMPhase(setting),
    CustomControlPhase(setting),
    MethodsPhase(setting),
    MetaContextGenPhase(setting),
    MetaContextDelegatePhase(setting),
    OnCreateEventsPhase(setting),
    DesignClsSkipPhase(setting)
  )

  private def snippetErrors(code: String)(using Context): List[String] =
    val unitName = "plugin-err-check"
    // the wildcard import every DFHDL test file opens with; beyond it, snippets are
    // self-contained blocks that see the classpath but not the call site's lexical scope
    val fullCode = "import dfhdl.*\n" + code
    val source2 = SourceFile.virtual(unitName, fullCode)

    // tested strings must not be rewritten by `-rewrite`
    val noRewriteSettings =
      ctx.settings.rewrite.updateIn(ctx.settingsState.reinitializedCopy(), None)

    class MegaPhaseWithCustomPhaseId(miniPhases: Array[MiniPhase], startId: Int, endId: Int)
        extends MegaPhase(miniPhases):
      override def start: Int = startId
      override def end: Int = endId

    def compilationUnits(untpdTree: untpd.Tree, tpdTree: Tree): List[CompilationUnit] =
      val unit = CompilationUnit(unitName, fullCode)
      unit.tpdTree = tpdTree
      unit.untpdTree = untpdTree
      List(unit)

    // The nested context is built and the snippet parsed and typed at the TYPER phase's
    // period: this phase's own period does not allow implicit search
    // (`Phase.allowsImplicitSearch`), and symbols created at typer stay valid for the
    // forward phase runs below (which re-pin their own periods via `atPhase`). This matches
    // the upstream intrinsic, which evaluates during typer.
    atPhase(ctx.base.typerPhase) {
      // a dummy owner, as in the upstream intrinsic: the actual owner might be inspected by
      // a transform phase, leading to cyclic errors
      val dummyOwner =
        newSymbol(ctx.owner, "$dummySymbol$".toTermName, Private, defn.AnyType, NoSymbol)
      val newContext = ctx.fresh
        .setSettings(noRewriteSettings)
        .setNewTyperState()
        .setTyper(new Typer(ctx.nestingLevel + 1))
        .setSource(source2)
        .withOwner(dummyOwner)

      inContext(newContext) {
        def noErrors = ctx.reporter.allErrors.isEmpty
        val parsed = new Parser(source2).block()
        if (noErrors)
          val untpdTree = preTyperRewriter.rewriteParsed(parsed)
          val tpdTree = ctx.typer.typed(untpdTree)
          if (noErrors)
            // Every run below is constructed INSIDE this nested context on purpose: the
            // closures capture the given Context, and capturing the enclosing real one
            // would leak the snippet's diagnostics into the real compilation.
            //
            // The standard runs are those the real pipeline interleaves with the plugin
            // phases. Pickler, SetRootTree (present only under -Yretain-trees), and the
            // InlineVals/ElimRepeated/RefChecks group the upstream intrinsic reconstructs
            // are all irrelevant to plugin diagnostics and skipped.
            val standardRuns: List[(Int, Tree => Tree)] = List(
              ctx.base.postTyperPhase,
              ctx.base.inliningPhase
            ).collect {
              case p if p.exists =>
                (
                  p.id,
                  (t: Tree) => atPhase(p)(p.runOn(compilationUnits(untpdTree, t)).head.tpdTree)
                )
            }
            // Each fresh plugin phase is pinned to its installed counterpart's phase id, so
            // denotation lookups match the real pipeline, and the whole nested pipeline is
            // ordered by those ids, i.e. by the real schedule's order.
            val installed = installedPhaseMap
            val pluginRuns: List[(Int, Tree => Tree)] =
              freshPluginPhases.flatMap { fresh =>
                installed.get(fresh.phaseName).map { real =>
                  val mp = MegaPhaseWithCustomPhaseId(Array(fresh), real.id, real.id)
                  val run: Tree => Tree = fresh match
                    // PureCheck does its whole-run analysis (and its static-impurity error
                    // reporting) in `runOn`, which `transformUnit` never reaches
                    case pureCheck: PureCheckPhase =>
                      (t: Tree) =>
                        atPhase(mp.end + 1) {
                          val res = mp.transformUnit(t)
                          pureCheck.analyzeNested(compilationUnits(untpdTree, res))
                          res
                        }
                    case _ => (t: Tree) => atPhase(mp.end + 1)(mp.transformUnit(t))
                  (real.id, run)
                }
              }
            var transformTree = tpdTree
            for ((_, run) <- (standardRuns ++ pluginRuns).sortBy(_._1))
              if (noErrors) transformTree = run(transformTree)
          end if
        end if
        ctx.reporter.allErrors.map(_.message)
      }
    }
  end snippetErrors
end PluginTestPhase
