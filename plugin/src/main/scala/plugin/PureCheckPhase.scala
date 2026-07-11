package dfhdl.plugin

import dotty.tools.dotc.*

import plugins.*

import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Annotations.Annotation
import SymDenotations.*

import Decorators.*
import ast.Trees.*
import ast.tpd
import Names.*
import Constants.Constant
import Types.*
import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable

/** Purity analysis. DFHDL elaboration is PURE BY DEFAULT: a design's (or design def's) elaboration
  * is assumed to be a function of its code, applied parameters, input types, and plain Scala
  * arguments, which allows elaboration caching. This phase transitively synthesizes
  * `@hw.annotation.pure(false)` (impure marking) on defs, classes, and vals whose bodies detectably
  * depend on effects:
  *   - references to symbols already marked `@pure(false)`, whether explicitly by the user,
  *     internally by the library (the `toScalaXYZ` family), or synthesized by this phase in a
  *     dependency (annotations persist to TASTy, so the transitive check only ever consults direct
  *     dependencies' saved summaries; no deep exploration is needed)
  *   - references to a blacklist of known-impure code we cannot annotate (randomness, IO, time,
  *     system state)
  *   - reads or writes of `var`s declared outside the analyzed definition
  *
  * An explicit `@pure` (or `@pure(true)`) is the user's trust override: such definitions are not
  * analyzed and never marked. DFHDL's own packages are trusted as pure unless explicitly annotated,
  * since the library's member-creation effects are exactly what elaboration caching reproduces
  * deterministically.
  *
  * The analysis over-approximates reachability (a reference anywhere in a body counts) and cannot
  * detect all effects (e.g. mutation behind unannotated third-party calls); undetected effects must
  * be marked `@pure(false)` manually and are otherwise the user's responsibility.
  *
  * The phase must run before `MetaContextPlacer` (which captures class annotations into
  * `__clsMeta`) and thereby also before `pickler` (which persists the synthesized annotations to
  * TASTy for dependent compilations).
  */
class PureCheckPhase(setting: Setting) extends CommonPhase:
  import tpd.*

  val phaseName = "PureCheck"

  override val runsAfter = Set("TopAnnot")
  override val runsBefore = Set("MetaContextPlacer")

  private var pureAnnotSym: Symbol = uninitialized

  // known-impure code we cannot annotate ourselves; each entry matches itself and all its
  // members (prefix with a `.` boundary)
  private val blacklisted = List(
    "scala.util.Random",
    "scala.sys",
    "scala.io",
    "java.util.Random",
    "java.util.concurrent.ThreadLocalRandom",
    "java.io",
    "java.nio.file",
    "java.net",
    "java.time",
    "java.lang.System.currentTimeMillis",
    "java.lang.System.nanoTime",
    "java.lang.System.getenv",
    "java.lang.System.getProperty",
    "java.lang.System.getProperties"
  )
  // DFHDL's own implementation is imperative by design (mutable elaboration DB); its effects
  // are exactly what elaboration caching reproduces, so it is trusted as pure unless a symbol
  // is explicitly annotated (e.g. the `toScalaXYZ` family)
  private val trusted = List(
    "dfhdl.core",
    "dfhdl.internals",
    "dfhdl.compiler",
    "dfhdl.hw",
    "dfhdl.options",
    "dfhdl.app",
    "dfhdl.tools",
    "dfhdl.plugin"
  )

  // None = unannotated, Some(true) = explicitly pure (trust override),
  // Some(false) = impure (explicit or previously synthesized)
  private def pureMarking(sym: Symbol)(using Context): Option[Boolean] =
    sym.getAnnotation(pureAnnotSym).map { annot =>
      annot.argumentConstant(0) match
        case Some(c) if c.tag == Constants.BooleanTag => c.booleanValue
        case _                                        => true
    }

  // classification codes: 0 = none, 1 = trusted, 2 = blacklisted
  private val classificationCache = mutable.Map.empty[Symbol, Int]
  private def matchesEntry(fqn: String, entry: String): Boolean =
    fqn == entry || fqn.startsWith(entry + ".")
  private def classify(sym: Symbol)(using Context): Int =
    classificationCache.getOrElseUpdate(
      sym, {
        val fqn = sym.fullName.toString
        if (trusted.exists(matchesEntry(fqn, _))) 1
        else if (blacklisted.exists(matchesEntry(fqn, _))) 2
        else 0
      }
    )

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val res = super.runOn(units)
    pureAnnotSym = getClassIfDefined("dfhdl.hw.annotation.pure")
    if (pureAnnotSym.exists)
      analyze(res)
    res

  private def analyze(units: List[CompilationUnit])(using Context): Unit =
    // analyzable definitions of this run, in discovery order
    val roots = mutable.LinkedHashMap.empty[Symbol, (Tree, Boolean)] // sym -> (tree, isClass)
    val verdictImpure = mutable.Set.empty[Symbol]
    val rdeps = mutable.Map.empty[Symbol, mutable.Set[Symbol]]
    // the common base of all DFHDL containers (designs/interfaces/domains); the inherited
    // purity walk of a design class stops here (exclusive)
    val containerCls = getClassIfDefined("dfhdl.core.Container")

    object rootCollector extends TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        tree match
          case dd: DefDef if dd.symbol.exists && !dd.symbol.isConstructor && !dd.rhs.isEmpty =>
            roots += dd.symbol -> (dd.rhs, false)
          case td @ TypeDef(_, tmpl: Template) if td.symbol.exists =>
            roots += td.symbol -> (tmpl, true)
          case vd: ValDef
              if vd.symbol.exists && vd.symbol.owner.isClass && !vd.rhs.isEmpty &&
                // container (design/interface/domain) instance vals are not roots: their
                // rhs impurity is the CHILD's elaboration, carried by the child class
                // marking and by the owner's template scan; referencing the instance
                // afterwards is pure
                !(containerCls.exists && vd.tpt.tpe.derivesFrom(containerCls.asClass)) =>
            roots += vd.symbol -> (vd.rhs, false)
          case _ =>
        traverseChildren(tree)
      end traverse
    end rootCollector
    units.foreach { cu =>
      if (!cu.tpdTree.isEmpty) rootCollector.traverse(cu.tpdTree)
    }

    def markImpure(sym: Symbol): Unit =
      if (verdictImpure.add(sym))
        rdeps.getOrElse(sym, Set.empty).foreach(markImpure)

    def scanRoot(rootSym: Symbol, tree: Tree, isClass: Boolean): Unit =
      if (classify(rootSym) != 1)
        pureMarking(rootSym) match
          case Some(true)  => // trusted by the user, not analyzed
          case Some(false) => markImpure(rootSym)
          case None        =>
            var impure = false
            val localDeps = mutable.Set.empty[Symbol]
            def visitRef(sym: Symbol): Unit =
              if (sym.exists && !impure)
                // a constructor's purity is its class's purity
                val target = if (sym.isConstructor) sym.owner else sym
                // accessing an object may run its initializer
                val moduleCls =
                  if (target.is(Module) && !target.isClass) target.moduleClass else NoSymbol
                check(target)
                if (moduleCls.exists) check(moduleCls)
            def check(sym: Symbol): Unit =
              if (!impure)
                pureMarking(sym) match
                  case Some(false) => impure = true
                  case Some(true)  => // trusted by the user
                  case None        =>
                    classify(sym) match
                      case 1 => // trusted library code
                      case 2 => impure = true // blacklisted
                      case _ =>
                        if (sym.is(Mutable) && !sym.isContainedIn(rootSym)) impure = true
                        else if (verdictImpure.contains(sym)) impure = true
                        else if (roots.contains(sym) && sym != rootSym) localDeps += sym
            object bodyScanner extends TreeTraverser:
              def traverse(tree: Tree)(using Context): Unit =
                if (!impure)
                  tree match
                    // nested definitions are their own analysis roots; only actual
                    // references to them (calls, closures, instantiations) create edges
                    case _: DefDef             =>
                    case _: TypeDef            =>
                    case _: Import | _: Export =>
                    // structural references to enclosing owners (e.g. the implicit
                    // `this.dfc` in every member) must not create impurity edges; the
                    // enclosing class's own marking carries its poison
                    case _: This | _: Super                                        =>
                    case vd: ValDef if vd.symbol.exists && vd.symbol.owner.isClass =>
                      // a class-owned val is usually its own root, so the edge suffices;
                      // container instance vals are not roots (see the root collector), so
                      // their rhs is scanned directly here as part of the owner's template
                      if (roots.contains(vd.symbol)) visitRef(vd.symbol)
                      else traverseChildren(vd)
                    case _ =>
                      visitRef(tree.symbol)
                      traverseChildren(tree)
            end bodyScanner
            val treesToScan: List[Tree] =
              if (isClass)
                val tmpl = tree.asInstanceOf[Template]
                // the constructor runs the parent constructors, the member val
                // initializers, and the plain template statements; member methods and
                // nested classes only run when referenced
                tmpl.parents ++ tmpl.body.filter {
                  case _: DefDef | _: TypeDef => false
                  case _                      => true
                }
              else List(tree)
            treesToScan.foreach(bodyScanner.traverse)
            // a class inherits its IMMEDIATE parents' purity MARKING; no deeper walk is
            // needed because this phase guarantees transitivity by induction: each parent's
            // own compilation already folded its parents' impurity into its own marking.
            // Only markings (and same-run pending roots) are consulted here; the general
            // blacklist/trusted rules must not apply to parents, since symbol-level parents
            // include compiler-added ones (e.g. `java.io.Serializable` on case classes,
            // which would falsely match the IO blacklist).
            def checkParent(sym: Symbol): Unit =
              if (!impure && sym.exists)
                pureMarking(sym) match
                  case Some(false) => impure = true
                  case Some(true)  =>
                  case None        =>
                    if (verdictImpure.contains(sym)) impure = true
                    else if (roots.contains(sym) && sym != rootSym) localDeps += sym
            if (!impure && isClass)
              rootSym.asClass.parentSyms.foreach(checkParent)
            if (impure) markImpure(rootSym)
            else if (localDeps.nonEmpty)
              localDeps.foreach { d =>
                rdeps.getOrElseUpdate(d, mutable.Set.empty) += rootSym
                // the dep may have been resolved impure after we checked it
                if (verdictImpure.contains(d)) markImpure(rootSym)
              }
        end match
    end scanRoot

    roots.foreach { case (sym, (tree, isClass)) => scanRoot(sym, tree, isClass) }

    // synthesize `@pure(false)` so the marking is visible to the runtime (design metas), to
    // later phases, and, once pickled, to dependent compilations
    verdictImpure.foreach { sym =>
      if (pureMarking(sym).isEmpty)
        sym.addAnnotation(
          Annotation(New(pureAnnotSym.typeRef, List(Literal(Constant(false)))).withSpan(sym.span))
        )
    }
  end analyze
end PureCheckPhase
