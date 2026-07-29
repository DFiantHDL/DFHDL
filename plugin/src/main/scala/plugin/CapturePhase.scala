package dfhdl.plugin

import dotty.tools.dotc.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.tpd
import Types.*
import scala.language.implicitConversions
import scala.compiletime.uninitialized

/** Capture discovery: which values a method (or a design class template) references from OUTSIDE
  * its own scope.
  *
  * The Methods rigging makes the generated design self-contained by materializing a def's captures
  * explicitly, evaluated in the def's rhs scope at every call:
  *   - captured DFHDL constants become PHANTOM design parameters
  *   - captured non-constant DFHDL values become PHANTOM input ports
  *   - captured plain Scala values join the elaboration cache key (`scalaArgs`)
  *
  * Phantom members are tagged (`ir.PhantomTag`) so the DFHDL printer hides them in the method view
  * form.
  *
  * Discovery lives here, apart from the phases that act on it, because it is a CONTRACT between
  * them rather than any one phase's business: Methods creates the phantom members, while PureCheck
  * must predict the very same set (down to the phantom parameter NAMES, recorded on the def's
  * `pure(true, impureParams*)` annotation) before those members exist. A set that differed between
  * the two would silently drop a captured constant out of the design load key.
  */
trait CapturePhase extends CommonPhase:
  import tpd.*

  // ~~~ the methods of the run ~~~
  // The defs that `Methods` turns into method harnesses, by symbol, each mapped to its
  // context lambda (the anon def whose rhs is the actual design body). The capture discovery
  // needs them: a call to a method does not run its body in the caller's scope, but it DOES
  // evaluate the callee's captures there (see `capturesOfMethod`).
  private val methodDesignAnons = collection.mutable.Map.empty[Symbol, DefDef]
  private var methodDesignAnonsRun: AnyRef | Null = null
  protected var scopeFunctionCls: Symbol = uninitialized
  protected var scopeProceduralCls: Symbol = uninitialized
  protected var domainTypeStaticSym: Symbol = uninitialized
  protected var domainTypeEDSym: Symbol = uninitialized
  protected var domainTypeDFSym: Symbol = uninitialized

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    scopeFunctionCls = getClassIfDefined("dfhdl.core.DFC.Scope.Function")
    scopeProceduralCls = getClassIfDefined("dfhdl.core.DFC.Scope.Procedural")
    // The domain evidence types are OPAQUE, so they are type aliases rather than classes and
    // `getClassIfDefined` cannot reach them. Outside `object DomainType` the opacity holds, so
    // `Static` and `ED` are distinct and mutually unrelated, which is what makes them a sound
    // discriminator between a static function and an ED method (both carry `Scope.Function`).
    domainTypeStaticSym = domainTypeSym("Static")
    domainTypeEDSym = domainTypeSym("ED")
    domainTypeDFSym = domainTypeSym("DF")
    // the trees of a new run carry new symbols
    if (methodDesignAnonsRun ne ctx.run)
      methodDesignAnonsRun = ctx.run
      methodDesignAnons.clear()
      methodCaptures.clear()
    ctx
  end prepareForUnit

  private def domainTypeSym(name: String)(using Context): Symbol =
    val domainTypeMod = getModuleIfDefined("dfhdl.core.DomainType")
    if (domainTypeMod.exists) domainTypeMod.moduleClass.info.member(name.toTypeName).symbol
    else NoSymbol

  // ED methods and static functions (HDL methods) are methods regardless of their
  // DFHDL-value parameter count; they are detected by the scope evidence parameter that the
  // `<> EDRET` / `<> CONSTRET` match types inject into the context lambda.
  protected def isHDLMethodAnonDef(anonDef: DefDef)(using Context): Boolean =
    anonDef.paramss.flatten.exists {
      case vd: ValDef =>
        (scopeFunctionCls.exists && vd.tpe <:< scopeFunctionCls.typeRef) ||
        (scopeProceduralCls.exists && vd.tpe <:< scopeProceduralCls.typeRef)
      case _ => false
    }
  // A static function (`<> CONSTRET`) carries `Scope.Function` exactly as an ED function does, so
  // ONLY the domain evidence separates the two.
  protected def isStaticAnonDef(anonDef: DefDef)(using Context): Boolean =
    domainTypeStaticSym.exists && anonDef.paramss.flatten.exists {
      case vd: ValDef => vd.tpt.tpe.typeSymbol == domainTypeStaticSym
      case _          => false
    }
  // the context lambda of a def that `Methods` transforms into a design (the conditions its
  // `transformDefDef` matches on), or None when the def is not one
  protected def methodDesignAnonOf(dd: DefDef)(using Context): Option[DefDef] =
    dd.rhs match
      case Block(List(anonDef: DefDef), _: Closure)
          if !dd.isInline && !dd.symbol.is(Exported) && anonDef.dfValTpeOpt.nonEmpty &&
            (dd.paramss.view.flatten.exists {
              case vd: ValDef => vd.dfValTpeOpt.nonEmpty && !vd.tpt.tpe.isDFConst
              case _          => false
            } || isHDLMethodAnonDef(anonDef)) =>
        Some(anonDef)
      case _ => None
  // Registers the methods of a tree. The registration is a PRE-pass (over the whole unit,
  // before any of it is transformed) rather than a per-def hook: a call contributes the callee's
  // captures to the CALLER, and a def may well be called by a def declared before it, which a
  // hook walking the tree in order would not have registered yet.
  protected def collectDFHDLMethods(tree: Tree)(using Context): Unit =
    object collector extends TreeTraverser:
      def traverse(t: Tree)(using Context): Unit =
        t match
          case dd: DefDef if dd.symbol.exists && !dd.symbol.isConstructor =>
            methodDesignAnonOf(dd).foreach(anonDef =>
              methodDesignAnons.getOrElseUpdate(dd.symbol, anonDef)
            )
          case _ =>
        traverseChildren(t)
    collector.traverse(tree)
  // the methods of the run, as registered by `collectDFHDLMethods`
  protected def isDFHDLMethod(sym: Symbol): Boolean = methodDesignAnons.contains(sym)
  protected def methodDesignAnon(sym: Symbol): Option[DefDef] = methodDesignAnons.get(sym)

  // ~~~ method capture discovery ~~~
  // Captures are keyed by their full stable access path: the same member symbol reached
  // through different instance paths must not unify.
  final protected case class MethodCaptures(
      phantomConsts: List[(List[Symbol], Tree)],
      phantomVals: List[(List[Symbol], Tree)],
      scalaCaptures: List[(List[Symbol], Tree)]
  )
  // a capture: its stable access path, a reference tree for it, and its kind
  private type Capture = (List[Symbol], Tree, Int)
  // the (transitive) captures of a method, memoized by symbol
  private val methodCaptures = collection.mutable.Map.empty[Symbol, List[Capture]]
  // the symbol path of a stable reference, leaf first
  protected def stablePathKey(t: Tree)(using Context): Option[List[Symbol]] = t match
    case id: Ident if id.symbol.exists && id.symbol.isTerm               => Some(List(id.symbol))
    case th: This                                                        => Some(List(th.symbol))
    case sel @ Select(qual, _) if sel.symbol.exists && sel.symbol.isTerm =>
      stablePathKey(qual).map(sel.symbol :: _)
    case _ => None
  // deterministic phantom naming: after the captured value itself. The runtime harness
  // names the phantom member from the applied value's own meta (exactly like
  // `cloneUnreachable` auto-parameters); this static leaf name is the compile-time
  // prediction of that name, used by PureCheck for impure-param recording and passed as
  // the runtime fallback for anonymous applied values.
  protected def captureName(path: List[Symbol])(using Context): String =
    path.head.name match
      // a capture of a generated design-parameter member (a rewritten reference to a
      // `<> CONST` class parameter) is named after the original parameter, matching the
      // parameter's runtime meta name and PureCheck's pre-rewrite prediction
      case NameKinds.UniqueName(prefix, _) if prefix.toString.endsWith("_plugin") =>
        prefix.toString.dropRight("_plugin".length)
      case _ => path.head.getFinalName()
  // rooted at `this` of an enclosing container: an instance member is capturable; static
  // (global) values are reachable/code-determined everywhere and never captured; the def's own
  // parameters and body locals are not captures
  private def methodRootOk(defSym: Symbol, anonDefSym: Symbol)(path: List[Symbol])(using
      Context
  ): Boolean =
    val root = path.last
    if (root.isClass) true
    else !root.isStatic && !root.ownersIterator.exists(o => o == defSym || o == anonDefSym)
  protected def discoverMethodCaptures(defSym: Symbol, anonDefSym: Symbol, body: Tree)(using
      Context
  ): MethodCaptures =
    asMethodCaptures(
      discoverCaptures(
        List(body),
        methodRootOk(defSym, anonDefSym),
        transitive = true,
        visiting = Set(defSym)
      )
    )
  // The captures a method lifts, transitively over the methods IT calls, memoized by
  // symbol. `visiting` breaks a call cycle (a recursive method cannot elaborate anyway).
  private def capturesOfMethod(defSym: Symbol, visiting: Set[Symbol])(using
      Context
  ): List[Capture] =
    methodCaptures.get(defSym) match
      case Some(captures) => captures
      case None           =>
        methodDesignAnons.get(defSym) match
          case Some(anonDef) =>
            val captures = discoverCaptures(
              List(anonDef.rhs),
              methodRootOk(defSym, anonDef.symbol),
              transitive = true,
              visiting = visiting + defSym
            )
            methodCaptures += defSym -> captures
            captures
          // a def declared outside this run cannot be looked into: its captures stay its own
          case None => Nil

  // ~~~ class-template capture discovery (shared by the PureCheck and MetaContextPlacer
  // phases) ~~~
  // A design class's template may reference values from outside the class ("captures").
  // Class designs get no phantom rigging: captured DFHDL constants materialize as
  // auto-created design parameters at runtime (`cloneUnreachable`), and captured plain
  // Scala values join the design load key through the `__clsScalaArgs` chain, closing
  // the per-instance-Scala-data soundness hole for classes (e.g. a local class whose
  // body reads an enclosing loop's variable).
  protected def discoverClsCaptures(clsSym: ClassSymbol, tmpl: Template)(using
      Context
  ): MethodCaptures =
    def rootOk(path: List[Symbol]): Boolean =
      val root = path.last
      if (root.isClass)
        // `this`-rooted: only an OUTER instance's members are captures; the class's own
        // members and members of classes nested WITHIN it (which the traversal also
        // reaches) are not
        root != clsSym && clsSym.isContainedIn(root)
      else
        !root.isStatic &&
        !root.ownersIterator.exists(o => o == clsSym || o == clsSym.primaryConstructor)
    // a class template calls a method from the design itself, where the def's captures are
    // by construction reachable, so no transitive capture propagation is needed here
    asMethodCaptures(
      discoverCaptures(tmpl.parents ++ tmpl.body, rootOk, transitive = false, visiting = Set.empty)
    )
  end discoverClsCaptures

  private def asMethodCaptures(captures: List[Capture]): MethodCaptures =
    def ofKind(kind: Int): List[(List[Symbol], Tree)] =
      captures.collect { case (path, t, `kind`) => (path, t) }
    MethodCaptures(ofKind(1), ofKind(2), ofKind(3))

  private def discoverCaptures(
      bodies: List[Tree],
      rootOk: List[Symbol] => Boolean,
      transitive: Boolean,
      visiting: Set[Symbol]
  )(using Context): List[Capture] =
    // capture kinds: 0 = not a capture, 1 = DFHDL constant, 2 = DFHDL value, 3 = plain Scala
    def captureKindOf(t: Tree): Int =
      // NOTE: the type must be widened before the DFHDL-value test, since a member with an
      // explicit `<> ...` type annotation carries the unreduced match-type alias on its
      // TermRef (unlike inferred-type members)
      if (!t.tpe.isStable || !t.symbol.exists || t.symbol.isStatic) 0
      else
        stablePathKey(t) match
          case Some(path) if rootOk(path) =>
            val widened = t.tpe.widen
            if (widened.dfValTpeOpt.nonEmpty)
              if (t.tpe.isDFConst) 1 else 2
            else if (widened.isMetaContext) 0
            else 3
          case _ => 0
    val captured = collection.mutable.LinkedHashMap.empty[List[Symbol], (Tree, Int)]
    object captureFinder extends TreeTraverser:
      def traverse(t: Tree)(using Context): Unit = t match
        // A call to another method. Its body does not run here, but its captures ARE
        // evaluated here, at the call, in this scope. So a capture of the callee that this
        // scope cannot reach either (the callee's design is nested in this one, and the value
        // belongs to a design further out) is a capture of THIS def as well, and materializing
        // it here as a phantom is what gives the call something reachable to bind to (see
        // `localize` in the `designFromDef` harness). Captures the caller CAN reach are
        // filtered out by its own `rootOk`, exactly as if it had referenced them itself.
        case _: (Ident | Select)
            if transitive && !visiting.contains(t.symbol) && methodDesignAnons.contains(t.symbol) =>
          capturesOfMethod(t.symbol, visiting).foreach { (path, capture, kind) =>
            if (rootOk(path)) captured.getOrElseUpdate(path, (capture, kind))
          }
          traverseChildren(t)
        case _: (Ident | Select) =>
          captureKindOf(t) match
            case 0    => traverseChildren(t)
            case kind => captured.getOrElseUpdate(stablePathKey(t).get, (t, kind))
        case _ => traverseChildren(t)
    end captureFinder
    bodies.foreach(captureFinder.traverse)
    captured.view.map((path, capture) => (path, capture._1, capture._2)).toList
  end discoverCaptures
end CapturePhase
