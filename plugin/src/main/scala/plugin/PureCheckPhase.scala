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
import annotation.tailrec

/** Purity analysis. DFHDL elaboration is PURE BY DEFAULT: a design's (or design def's) elaboration
  * is assumed to be a function of its code, applied parameters, input types, and plain Scala
  * arguments, which allows elaboration caching. This phase transitively synthesizes
  * `@hw.annotation.pure(false)` (impure marking) on defs, classes, and vals whose bodies detectably
  * depend on effects:
  *   - references to symbols already marked `@pure(false)`, whether explicitly by the user or
  *     synthesized by this phase in a dependency (annotations persist to TASTy, so the transitive
  *     check only ever consults direct dependencies' saved summaries; no deep exploration is
  *     needed)
  *   - references to a blacklist of known-impure code we cannot annotate (randomness, IO, time,
  *     system state)
  *   - reads or writes of `var`s declared outside the analyzed definition
  *
  * Constant-data forcing (the `toScalaXYZ` family, marked `pure(true, "*")` in the library: pure
  * given its receiver's data) is attributed to the forced value's dataflow root rather than
  * poisoning the referencing design. Data-impure parameters are recorded BY NAME on their def's own
  * annotation, `pure(true, impureParams*)` (`"*"` matches all parameters), so the marking prints at
  * the declaration, covers phantom parameters that have no source symbol, and makes the
  * `toScalaXYZ` family simply the BASE CASE of the marked-parameter propagation: every application
  * of a marked parameter re-attributes its applied argument at the call site:
  *   - rooted at a design def's `<> CONST` parameter: the def gets `pure(true, <param name>)` and
  *     stays pure; the runtime elaboration cache keys the named parameters' applied type and data
  *   - rooted at a captured constant of the design def (an out-of-scope value that the DesignDefs
  *     rigging turns into a phantom design parameter): the def gets `pure(true, <phantom name>)`
  *     and the phantom's applied data is keyed exactly like an explicit parameter's
  *   - rooted at a `<> VAL` input or a plain Scala argument of a design def: fully pure, since the
  *     cache key already covers input types and Scala argument values (forcing a non-constant's
  *     data is impossible, so a `<> VAL` root implies a type-derived constant, e.g. its width)
  *   - rooted at a parameter of any other (non-design-def) method compiled in this run: that
  *     parameter is recorded the same way, and every application of it re-attributes the applied
  *     argument at the call site, so the forcing propagates to its true root across helper and
  *     inline methods
  *   - rooted at a code-determined static (a global constant, an object member): fully pure
  *   - anything else (per-instance data, mutable state, opaque flows): falls back to design-level
  *     impurity, exactly as if the forcer itself were referenced impurely
  *
  * An explicit `@pure` (or `@pure(true)`) is the user's trust override: such definitions are not
  * analyzed and never marked (nor are their parameters). DFHDL's own packages are trusted as pure
  * unless explicitly annotated, since the library's member-creation effects are exactly what
  * elaboration caching reproduces deterministically.
  *
  * The analysis over-approximates reachability (a reference anywhere in a body counts) and cannot
  * detect all effects (e.g. mutation behind unannotated third-party calls); undetected effects must
  * be marked `@pure(false)` manually and are otherwise the user's responsibility.
  *
  * The phase must run before `MetaContextPlacer` (which captures class annotations into
  * `__clsMeta`) and thereby also before `pickler` (which persists the synthesized annotations to
  * TASTy for dependent compilations).
  */
class PureCheckPhase(setting: Setting) extends CapturePhase:
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

  // the data-impure parameter NAMES recorded on a def's `pure(isPure, impureParams*)`
  // annotation (synthesized by this phase in a previous compilation, or user/library
  // declared; `"*"` matches every parameter, e.g. on the `toScalaXYZ` data forcers)
  private val impureParamsCache = mutable.Map.empty[Symbol, Set[String]]
  private def markedImpureParams(sym: Symbol)(using Context): Set[String] =
    impureParamsCache.getOrElseUpdate(
      sym,
      sym.getAnnotation(pureAnnotSym) match
        case Some(annot) =>
          annot.tree match
            case Apply(_, args) =>
              args.collect {
                case Typed(SeqLiteral(elems, _), _) => elems
                case sl: SeqLiteral                 => sl.elems
              }.flatten.collect { case Literal(Constant(s: String)) => s }.toSet
            case _ => Set.empty
        case None => Set.empty
    )

  // a class parameter accessor and its constructor parameter are the same logical
  // parameter; the constructor parameter symbol is the canonical representative
  // (instantiation-site attribution and annotation-name synthesis meet it there)
  private def normalizedParam(s: Symbol)(using Context): Symbol =
    if (s.is(ParamAccessor))
      s.owner.primaryConstructor.paramSymss.flatten
        .find(p => p.isTerm && p.name == s.name).getOrElse(s)
    else s

  // markings and roots of a parameter's logical owner: a constructor parameter belongs
  // to its class
  private def paramOwnerOf(p: Symbol)(using Context): Symbol =
    if (p.owner.isConstructor) p.owner.owner else p.owner
  private def isMarkedParam(owner: Symbol, p: Symbol)(using Context): Boolean =
    // a class records its data-impure constructor parameters on the CLASS annotation
    // (which is what reaches the runtime through the design meta), so a constructor
    // callee consults its class
    val annotOwner = if (owner.isConstructor) owner.owner else owner
    val names = markedImpureParams(annotOwner)
    names.contains("*") || names.contains(p.name.toString)

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

  // members of the `dfhdl` root package itself (the `hdl` object and the package-level export
  // forwarders of the core ops); library code like `dfhdl.lib` lives in SUB-packages and is
  // deliberately not covered
  private val dfhdlRootPkgCache = mutable.Map.empty[Symbol, Boolean]
  private def isDfhdlRootPkgMember(sym: Symbol)(using Context): Boolean =
    dfhdlRootPkgCache.getOrElseUpdate(
      sym,
      sym.ownersIterator.find(_.is(Package)).exists(_.fullName.toString == "dfhdl")
    )

  // Scala core value operations are data-transparent for attribution purposes: their result
  // derives only from their (scanned) arguments
  private def isScalaCoreOp(sym: Symbol)(using Context): Boolean =
    val owner = sym.owner
    defn.ScalaValueClasses().contains(owner) || {
      val fqn = owner.fullName.toString
      fqn.startsWith("scala.runtime.") || fqn.startsWith("scala.math.") ||
      fqn == "java.lang.String"
    }

  // the method's DFHDL-value-typed term parameters (the only ones that can carry forcing)
  private val dfhdlParamsCache = mutable.Map.empty[Symbol, List[Symbol]]
  private def dfhdlParams(sym: Symbol)(using Context): List[Symbol] =
    dfhdlParamsCache.getOrElseUpdate(
      sym,
      sym.paramSymss.flatten.filter(p => p.isTerm && p.info.dfValTpeOpt.nonEmpty)
    )

  // strips a call tree down to its method part, collecting term argument lists and type
  // argument trees
  @tailrec private def decomposeCall(
      t: Tree,
      argss: List[List[Tree]] = Nil,
      targs: List[Tree] = Nil
  ): (Tree, List[List[Tree]], List[Tree]) =
    t match
      case Apply(fun, args)   => decomposeCall(fun, args :: argss, targs)
      case TypeApply(fun, ts) => decomposeCall(fun, argss, ts ::: targs)
      case _                  => (t, argss, targs)

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    // the unit's design defs, for the capture discovery `analyze` runs once the whole run's
    // units have been walked (see `collectDesignDefs`)
    collectDesignDefs(tree)
    ctx

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val res = super.runOn(units)
    pureAnnotSym = getClassIfDefined("dfhdl.hw.annotation.pure")
    if (pureAnnotSym.exists)
      analyze(res)
    res

  // Nested-compilation entry point (see PluginTestPhase): the analysis normally runs in `runOn`
  // once the whole run's units are walked, but the nested snippet pipeline drives phases through
  // `MegaPhase.transformUnit`, which never invokes `runOn`.
  def analyzeNested(units: List[CompilationUnit])(using Context): Unit =
    pureAnnotSym = getClassIfDefined("dfhdl.hw.annotation.pure")
    if (pureAnnotSym.exists)
      analyze(units)

  // the attribution verdict for a forced (or forced-param-applied) expression
  private enum ForcedRes derives CanEqual:
    case Pure // the forced data is covered by the elaboration cache key (or code-determined)
    case Marks(params: Set[Symbol]) // the forced data roots at these parameters
    case Escalate // unattributable: fall back to design-level impurity

  private def combineRes(a: ForcedRes, b: ForcedRes): ForcedRes = (a, b) match
    case (ForcedRes.Escalate, _) | (_, ForcedRes.Escalate) => ForcedRes.Escalate
    case (ForcedRes.Marks(x), ForcedRes.Marks(y))          => ForcedRes.Marks(x ++ y)
    case (m: ForcedRes.Marks, _)                           => m
    case (_, r)                                            => r
  private def combineAll(rs: List[ForcedRes]): ForcedRes =
    rs.foldLeft(ForcedRes.Pure: ForcedRes)(combineRes)

  private def analyze(units: List[CompilationUnit])(using Context): Unit =
    // analyzable definitions of this run, in discovery order
    val roots = mutable.LinkedHashMap.empty[Symbol, (Tree, Boolean)] // sym -> (tree, isClass)
    val verdictImpure = mutable.Set.empty[Symbol]
    val rdeps = mutable.Map.empty[Symbol, mutable.Set[Symbol]]
    // parameters whose applied data is forced into elaboration (data-impure params)
    val paramForced = mutable.Set.empty[Symbol]
    // phantom parameter NAMES (design-def captures) whose applied data is forced into
    // elaboration, per design def; insertion-ordered for deterministic annotations
    val phantomForced = mutable.Map.empty[Symbol, mutable.LinkedHashSet[String]]
    // the design-def's captured constants that the DesignDefs rigging will turn into
    // phantom design parameters, keyed by the capture's stable access path
    val phantomConstCaptures = mutable.Map.empty[Symbol, Map[List[Symbol], String]]
    def phantomConstsOf(defSym: Symbol): Map[List[Symbol], String] =
      phantomConstCaptures.getOrElseUpdate(
        defSym,
        designDefAnon(defSym) match
          case Some(anonDef) =>
            discoverDesignDefCaptures(defSym, anonDef.symbol, anonDef.rhs)
              .phantomConsts.map((path, _) => path -> captureName(path)).toMap
          case None => Map.empty
      )
    def markPhantomForced(owner: Symbol, names: Set[String]): Unit =
      if (names.nonEmpty)
        phantomForced.getOrElseUpdate(owner, mutable.LinkedHashSet.empty) ++= names
    // pending applications of a possibly-forced parameter: when the param gets marked, each
    // recorded application re-attributes its argument in the caller's context
    val paramEdges = mutable.Map.empty[Symbol, mutable.ListBuffer[(Symbol, () => ForcedRes)]]
    // the common base of all DFHDL containers (designs/interfaces/domains); the inherited
    // purity walk of a design class stops here (exclusive)
    val containerCls = getClassIfDefined("dfhdl.core.Container")
    // design classes route their instantiation through the design load gate, which keys
    // their `<> CONST` parameters (applied data, once marked), plain Scala constructor
    // parameters and template captures (`__clsScalaArgs`), and captured-constant
    // auto-parameters, so forcing rooted at any of those is attributable rather than a
    // design-level escalation (mirroring design defs)
    val designCls = getClassIfDefined("dfhdl.core.Design")
    def isDesignCls(sym: Symbol): Boolean =
      designCls.exists && sym.isClass && !sym.isAnonymousClass &&
        sym.typeRef.derivesFrom(designCls.asClass)
    // the class-template captured constants that materialize as auto-created design
    // parameters at runtime (`cloneUnreachable`), keyed by the capture's stable access
    // path (the class-design counterpart of a design def's phantom constants)
    val clsConstCaptureMap = mutable.Map.empty[Symbol, Map[List[Symbol], String]]
    def clsConstCapturesOf(clsSym: Symbol): Map[List[Symbol], String] =
      clsConstCaptureMap.getOrElseUpdate(
        clsSym,
        roots.get(clsSym) match
          case Some((tmpl: Template, true)) =>
            discoverClsCaptures(clsSym.asClass, tmpl)
              .phantomConsts.map((path, _) => path -> captureName(path)).toMap
          case _ => Map.empty
      )

    // every immutable value definition of this run, for attribution tracing: a forced
    // value rooted at a val resolves to that val's definition, wherever it is defined
    // (local, sibling class member, global)
    val valRhs = mutable.Map.empty[Symbol, Tree]

    // A static function (`<> CONSTRET`) is PURE BY DEFINITION, so for it this phase's verdict is
    // fatal rather than advisory: an impure static def is an error, not a cache opt-out.
    def isStaticDef(sym: Symbol): Boolean = designDefAnon(sym).exists(isStaticAnonDef)
    def staticImpureError(sym: Symbol): Unit =
      report.error(
        """|A static function (`<> CONSTRET`) must be pure, and this one's elaboration depends on an effect.
           |Effects are randomness, IO, time, system state, a `var` declared outside the function, or a call to an impure definition.
           |Note that CAPTURED CONSTANTS are pure: they become phantom design parameters and only enter the elaboration cache key.""".stripMargin,
        sym.srcPos
      )
    object rootCollector extends TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        tree match
          case vd: ValDef if vd.symbol.exists && !vd.rhs.isEmpty && !vd.symbol.is(Mutable) =>
            valRhs += vd.symbol -> vd.rhs
          case _ =>
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
        end match
        traverseChildren(tree)
      end traverse
    end rootCollector
    units.foreach { cu =>
      if (!cu.tpdTree.isEmpty) rootCollector.traverse(cu.tpdTree)
    }

    def markImpure(sym: Symbol): Unit =
      if (verdictImpure.add(sym))
        rdeps.getOrElse(sym, Set.empty).foreach(markImpure)

    def markForcedParam(p: Symbol): Unit =
      if (paramForced.add(p))
        paramEdges.remove(p).getOrElse(mutable.ListBuffer.empty).foreach { (caller, resOf) =>
          resOf() match
            case ForcedRes.Marks(qs) => qs.foreach(markForcedParam)
            case ForcedRes.Escalate  => markImpure(caller)
            case ForcedRes.Pure      =>
        }

    def scanRoot(rootSym: Symbol, tree: Tree, isClass: Boolean): Unit =
      if (classify(rootSym) != 1)
        pureMarking(rootSym) match
          case Some(true)  => // trusted by the user, not analyzed
          case Some(false) => markImpure(rootSym)
          case None        =>
            var impure = false
            val localDeps = mutable.Set.empty[Symbol]
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
            // the nearest enclosing design def or design class of the analyzed root:
            // the design whose load key covers data landing in the analyzed body
            lazy val nearestDesignBoundary = rootSym.ownersIterator
              .find(o => isDesignDef(o) || (isDesignCls(o) && roots.contains(o)))
            // ~~~ constant-data forcing attribution ~~~
            // resolves a forced expression to its dataflow roots; see the phase doc
            def attributeSym(s: Symbol, visited: Set[Symbol]): ForcedRes =
              if (!s.exists) ForcedRes.Escalate
              // trusted library code (fields included, e.g. the implicit `dfc` context) and
              // user `@pure`-marked code is data-transparent: results derive from the
              // (also scanned) arguments
              else if (classify(s) == 1 || isDfhdlRootPkgMember(s) || pureMarking(s).contains(true))
                ForcedRes.Pure
              else if (pureMarking(s).contains(false)) ForcedRes.Escalate
              else if (s.is(ParamAccessor) || (s.is(Param) && s.isTerm && s.owner.isConstructor))
                // a design class's constructor parameter, referenced through its
                // in-template parameter accessor or the constructor parameter symbol
                // itself: `<> CONST` parameters get keyed by applied type+data once
                // marked (recorded on the CLASS annotation, like a design def's), and
                // plain Scala parameters are keyed by value (`__clsScalaArgs`), so both
                // are attributable. This resolution is only sound when the forced data
                // LANDS in the class's own body (the class key covers it): inside a
                // nested design def the def's OWN key must cover the data instead, so
                // the parameter escalates and the def-boundary capture path records a
                // phantom parameter name when applicable. Non-design class parameters
                // stay design-level (escalate).
                val cls = if (s.is(ParamAccessor)) s.owner else s.owner.owner
                if (
                  isDesignCls(cls) && rootSym.isContainedIn(cls) &&
                  nearestDesignBoundary.contains(cls)
                )
                  if (pureMarking(cls).isDefined) ForcedRes.Pure
                  else if (s.info.isDFConst) ForcedRes.Marks(Set(normalizedParam(s)))
                  else ForcedRes.Pure
                else ForcedRes.Escalate
              else if (s.is(Param) && s.isTerm)
                val m = s.owner
                if (!m.is(Method) || !rootSym.isContainedIn(m)) ForcedRes.Escalate
                // the def's explicit marking governs: `@pure` trusts, `@pure(false)` never
                // caches, so param attribution is moot either way
                else if (pureMarking(m).isDefined) ForcedRes.Pure
                else if (isDesignDef(m))
                  // `<> CONST` params get keyed by applied type+data once marked; `<> VAL`
                  // inputs are keyed by their DFTypes (forcing can only derive type-level
                  // constants from them) and plain Scala args are keyed by value
                  if (s.info.isDFConst) ForcedRes.Marks(Set(s)) else ForcedRes.Pure
                else if (roots.contains(m) && !m.isAnonymousFunction)
                  // helper/inline method compiled this run: mark the param so every
                  // application re-attributes its argument at the call site
                  if (s.info.dfValTpeOpt.nonEmpty) ForcedRes.Marks(Set(s))
                  else ForcedRes.Escalate
                else ForcedRes.Escalate
              else if (s.is(Mutable)) ForcedRes.Escalate
              else if (s.is(Method))
                // transparent combinators: the result derives from the (also scanned)
                // qualifier and arguments
                if (isScalaCoreOp(s)) ForcedRes.Pure
                else ForcedRes.Escalate
              else if (s.isTerm && !s.isClass && valRhs.contains(s))
                // an immutable value (local, sibling member, global): trace its definition.
                // NOTE: like the rest of this phase, resolution is STATIC; a subclass
                // overriding a traced member is not modeled
                if (visited.contains(s)) ForcedRes.Pure
                else attributeTree(valRhs(s), visited + s)
              else if (s.is(Module) || s.isStatic) ForcedRes.Pure // code-determined
              else ForcedRes.Escalate
            end attributeSym
            // a capture of the enclosing design def that becomes a phantom design
            // parameter (see the DesignDefs rigging): an otherwise-unattributable forcing
            // rooted at it is recorded by the phantom's predicted name on that design def
            // and keyed by the phantom's applied data, exactly like an explicit data-impure
            // parameter. The recording happens here directly (and the verdict turns Pure)
            // because attribution may run under any root NESTED in the design def, most
            // commonly the def's own context lambda, whose rhs is the actual design body.
            // Attribution that already resolved (Pure or explicit-param Marks) is kept, so
            // code-determined captures stay pure and do not fatten the key.
            def phantomCaptureRes(t: Tree, res: ForcedRes): ForcedRes = res match
              case ForcedRes.Escalate =>
                // the nearest enclosing design def or design class governs: a captured
                // constant materializes as the def's phantom parameter or the class's
                // auto-created parameter, keyed by applied data under the recorded name
                nearestDesignBoundary match
                  case Some(owner) =>
                    val captureMap =
                      if (isDesignDef(owner)) phantomConstsOf(owner)
                      else clsConstCapturesOf(owner)
                    stablePathKey(t).flatMap(captureMap.get) match
                      case Some(name) =>
                        markPhantomForced(owner, Set(name))
                        ForcedRes.Pure
                      case None => res
                  case None => res
              case _ => res
            def attributeTree(t: Tree, visited: Set[Symbol]): ForcedRes =
              t match
                case _ if t.isEmpty     => ForcedRes.Pure
                case _: Literal         => ForcedRes.Pure
                case _: TypeTree        => ForcedRes.Pure
                case _: This | _: Super => ForcedRes.Escalate // instance-rooted data
                case _: Closure | _: DefDef | _: TypeDef => ForcedRes.Escalate
                case NamedArg(_, arg)                    => attributeTree(arg, visited)
                case Typed(e, _)                         => attributeTree(e, visited)
                case Block(_, expr)                      => attributeTree(expr, visited)
                case If(c, tp, ep)                       =>
                  combineAll(List(c, tp, ep).map(attributeTree(_, visited)))
                case m: Match =>
                  combineAll((m.selector :: m.cases.flatMap(c => List(c.guard, c.body)))
                    .map(attributeTree(_, visited)))
                case i: Inlined =>
                  // the semantics live in the expansion (whose proxy bindings resolve
                  // through the local-vals trace); the residual call is metadata only
                  attributeTree(i.expansion, visited)
                case sl: SeqLiteral =>
                  combineAll(sl.elems.map(attributeTree(_, visited)))
                case app @ Apply(fun, args) =>
                  attributeSym(app.symbol, visited) match
                    case ForcedRes.Escalate => ForcedRes.Escalate
                    case symRes             =>
                      // implicit/contextual arguments cannot carry forced data unless they
                      // are DFHDL values themselves
                      val isImplicitList = fun.tpe.widen match
                        case mt: MethodType => mt.isImplicitMethod || mt.isContextualMethod
                        case _              => false
                      val relevantArgs =
                        if (isImplicitList) args.filter(_.tpe.dfValTpeOpt.nonEmpty) else args
                      combineAll(
                        symRes :: attributeTree(fun, visited) ::
                          relevantArgs.map(attributeTree(_, visited))
                      )
                case TypeApply(fun, _) =>
                  attributeSym(t.symbol, visited) match
                    case ForcedRes.Escalate => ForcedRes.Escalate
                    case symRes             => combineRes(symRes, attributeTree(fun, visited))
                case Select(qual, _) =>
                  val res = attributeSym(t.symbol, visited) match
                    case ForcedRes.Escalate => ForcedRes.Escalate
                    case symRes             =>
                      qual match
                        // the selected member's own rule governs; the instance link itself
                        // (e.g. `this.dfc`) is not data
                        case _: This | _: Super => symRes
                        case _                  => combineRes(symRes, attributeTree(qual, visited))
                  phantomCaptureRes(t, res)
                case _: Ident => phantomCaptureRes(t, attributeSym(t.symbol, visited))
                case _        => ForcedRes.Escalate // unknown shape: unattributable
            end attributeTree

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
            def applyRes(res: ForcedRes): Unit = res match
              case ForcedRes.Escalate  => impure = true
              case ForcedRes.Marks(ps) => ps.foreach(markForcedParam)
              case ForcedRes.Pure      =>
            // when a parameter that may carry forcing is applied opaquely (bare method
            // reference, partial application), no call-site attribution exists: a current
            // marking escalates now, a future one escalates through the recorded edge
            def unappliedParam(p: Symbol): Unit =
              if (paramForced.contains(p) || isMarkedParam(p.owner, p)) impure = true
              else
                val pOwner = paramOwnerOf(p)
                if (roots.contains(pOwner) && pureMarking(pOwner).isEmpty)
                  paramEdges.getOrElseUpdate(p, mutable.ListBuffer.empty) +=
                    ((rootSym, () => ForcedRes.Escalate))
            def visitUnappliedRef(sym: Symbol): Unit =
              visitRef(sym)
              if (!impure && sym.exists && sym.is(Method))
                dfhdlParams(sym).foreach(unappliedParam)
            // pairs a callee's parameter list with the term arguments of one application,
            // resolving named arguments by name
            def pairArgs(ps: List[Symbol], args: List[Tree]): List[(Symbol, Tree)] =
              args.zipWithIndex.flatMap {
                case (NamedArg(name, a), _) => ps.find(_.name == name).map(_ -> a)
                case (a, i)                 => ps.lift(i).map(_ -> a)
              }
            // propagates existing (or future, via edges) forced-param markings of the
            // callee onto this call's applied arguments
            def handleAppliedParams(callee: Symbol, argss: List[List[Tree]]): Unit =
              if (callee.exists && callee.is(Method) && dfhdlParams(callee).nonEmpty)
                // a constructor callee resolves to its class for roots/markings (a
                // design class instantiation attributes its applied `<> CONST`
                // arguments exactly like a design def call)
                val calleeRoot = if (callee.isConstructor) callee.owner else callee
                val recordable = roots.contains(calleeRoot) && pureMarking(calleeRoot).isEmpty
                val termParamss =
                  callee.paramSymss.filter(ps => ps.isEmpty || ps.head.isTerm)
                termParamss.zipAll(argss, Nil, Nil).foreach { (ps, args) =>
                  if (args.isEmpty)
                    // partial application: the remaining params escape attribution
                    ps.filter(p => p.info.dfValTpeOpt.nonEmpty).foreach(unappliedParam)
                  else
                    pairArgs(ps, args).foreach { (p, a) =>
                      if (p.info.dfValTpeOpt.nonEmpty)
                        if (paramForced.contains(p) || isMarkedParam(callee, p))
                          applyRes(attributeTree(a, Set.empty))
                        else if (recordable)
                          paramEdges.getOrElseUpdate(p, mutable.ListBuffer.empty) +=
                            ((rootSym, () => attributeTree(a, Set.empty)))
                    }
                }
            end handleAppliedParams

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
                    case app: Apply =>
                      val (meth, argss, targs) = decomposeCall(app)
                      val callee = meth.symbol
                      // data forcers (toScalaXYZ, `pure(true, "*")`) need no special case:
                      // the generic marked-param handling attributes their receiver (the
                      // extension's first term argument) like any other marked application
                      handleAppliedParams(callee, argss)
                      visitRef(callee)
                      // manual traversal: the method part was handled above; the receiver,
                      // type arguments, and all argument trees are scanned normally
                      meth match
                        case Select(qual, _) => traverse(qual)
                        case _               =>
                      targs.foreach(traverse)
                      argss.foreach(_.foreach(traverse))
                    case _ =>
                      visitUnappliedRef(tree.symbol)
                      traverseChildren(tree)
            end bodyScanner
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

    // synthesize `pure(isPure, impureParams*)` so the marking is visible to the runtime
    // (design metas), to later phases, and, once pickled, to dependent compilations
    def pureAnnotTree(isPure: Boolean, impureParams: List[String], sym: Symbol): tpd.Tree =
      New(
        pureAnnotSym.typeRef,
        List(
          Literal(Constant(isPure)),
          Typed(
            SeqLiteral(impureParams.map(n => Literal(Constant(n))), TypeTree(defn.StringType)),
            TypeTree(defn.RepeatedParamClass.typeRef.appliedTo(defn.StringType))
          )
        )
      ).withSpan(sym.span)
    verdictImpure.foreach { sym =>
      // For a static function the verdict is FATAL rather than advisory. `verdictImpure` covers
      // all three ways to earn it: a detected effect, an explicit `@pure(false)` (which `scanRoot`
      // routes through `markImpure`), and transitive impurity through a called definition.
      // `@pure`/`@pure(true)` stays the trust override: such defs are never analyzed and never
      // become rdep targets, so they never reach here.
      if (isStaticDef(sym)) staticImpureError(sym)
      else if (pureMarking(sym).isEmpty)
        sym.addAnnotation(Annotation(pureAnnotTree(false, Nil, sym)))
    }
    // forced params of a design-level-pure owner are recorded BY NAME on the owner itself
    // (`pure(true, names*)`): the runtime keys their applied type+data, the marking prints
    // at the declaration, and phantom params (with no source symbol) fit the same scheme
    // through their predicted capture names. A design-level-impure owner never caches, so
    // its param markings would only be noise.
    val paramForcedByOwner = paramForced.groupBy(paramOwnerOf)
    (paramForcedByOwner.keySet ++ phantomForced.keySet).foreach { owner =>
      if (pureMarking(owner).isEmpty && !verdictImpure.contains(owner))
        val ps = paramForcedByOwner.getOrElse(owner, mutable.Set.empty[Symbol])
        // a class's parameters live on its primary constructor
        val ownerParams =
          if (owner.isClass) owner.asClass.primaryConstructor.paramSymss.flatten
          else owner.paramSymss.flatten
        val names = ownerParams.collect {
          case p if ps.contains(p) => p.name.toString
        } ++ phantomForced.get(owner).fold(Nil)(_.toList)
        owner.addAnnotation(Annotation(pureAnnotTree(true, names, owner)))
    }
  end analyze
end PureCheckPhase
