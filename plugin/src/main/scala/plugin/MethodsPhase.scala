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

class MethodsPhase(setting: Setting) extends CapturePhase:
  import tpd._

  val phaseName = "Methods"
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  var designFromDefSym: Symbol = uninitialized
  var designFromDefEDSym: Symbol = uninitialized
  var designFromDefStaticSym: Symbol = uninitialized
  var designFromDefGetInputSym: Symbol = uninitialized
  var designFromDefGetParamSym: Symbol = uninitialized
  var irDFUnitCls: Symbol = uninitialized
  var scopeProcessCls: Symbol = uninitialized
  var designCls: Symbol = uninitialized
  var domainContainerCls: Symbol = uninitialized

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
    // HDL methods (ED methods and static functions) are detected by the scope evidence
    // parameter that the `<> EDRET` / `<> CONSTRET` match types inject into the context lambda:
    // `Scope.Function` for functions and `Scope.Procedural` for procedural ED methods (Unit).
    def hdlMethodScopeKindOf(anonDef: DefDef): Option[Boolean] =
      anonDef.paramss.flatten.collectFirst {
        case vd: ValDef if vd.tpe <:< scopeFunctionCls.typeRef   => true
        case vd: ValDef if vd.tpe <:< scopeProceduralCls.typeRef => false
      }
    // a `Unit` return, which is what declares a PROCEDURAL method under `<> EDRET`. `<> CONSTRET`
    // has no procedural form (static procedures are explicitly deferred), so this is an error there
    def hasUnitRet(anonDef: DefDef): Boolean =
      anonDef.dfValTpeOpt.map(_.widenDealias).exists {
        case AppliedType(_, dfTypeTpe :: _) =>
          dfTypeTpe.dealias match
            case AppliedType(_, irTpe :: _) => irTpe.typeSymbol == irDFUnitCls
            case _                          => false
        case _ => false
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
              (dfValArgs.nonEmpty || hdlMethodScopeKindOf(anonDef).nonEmpty)
          ) =>
        debug("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        debug(tree.show)
        // A static function carries `Scope.Function` exactly as an ED function does, so the scope
        // evidence identifies an HDL method and only the DOMAIN evidence says which kind it is.
        val isHDLMethod = hdlMethodScopeKindOf(anonDef).nonEmpty
        val isStatic = isStaticAnonDef(anonDef)
        val isEDMethod = isHDLMethod && !isStatic
        // "A static function ..." / "An ED method ..." and the plural, for the shared messages
        val kindStr = if (isStatic) "a static function" else "an ED method"
        val kindPluralStr = if (isStatic) "static functions" else "ED methods"
        var hasHDLMethodErrors = false
        def hdlMethodError(msg: String): Unit =
          report.error(msg, tree.srcPos)
          hasHDLMethodErrors = true
        if (isHDLMethod)
          // an explicit (possibly empty `()`) term parameter block is required, so that
          // method call sites always read as calls
          val hasTermParamBlock = tree.paramss.exists { clause =>
            clause.isEmpty || clause.headOption.exists {
              case vd: ValDef => !vd.symbol.is(Given)
              case _          => false
            }
          }
          if (!hasTermParamBlock)
            hdlMethodError(
              s"${kindStr.capitalize} must declare an explicit parameter block. Use an empty `()` parameter block if the method has no arguments."
            )
          // Direct recursion cannot be modeled: a method is a self-contained design hierarchy
          // that cannot contain itself. The reason is ELABORATION termination (the Scala body is
          // re-run per call site), not purity, so this applies to static functions too even though
          // a pure function may legally recurse in both Scala and VHDL.
          var hasRecursion = false
          object recursionFinder extends TreeTraverser:
            def traverse(t: Tree)(using Context): Unit = t match
              case _: (Ident | Select) if t.symbol == sym => hasRecursion = true
              case _                                      => traverseChildren(t)
          recursionFinder.traverse(anonDef.rhs)
          if (hasRecursion)
            hdlMethodError(s"Recursion is not allowed for $kindPluralStr.")
        end if
        if (isEDMethod)
          // An ED method's arguments are input PORTS, wired by a net at the call site, so their
          // values are invisible to elaboration and every call site necessarily elaborates a
          // structurally identical body. That is what lets one printed method serve all calls,
          // and it is exactly what a `<> CONST` argument would break: its value IS visible to
          // elaboration, so two call sites could elaborate genuinely different bodies, and an ED
          // method has no body-dedup step to fall back on. A static function accepts const
          // arguments precisely because it does (see the static-domain plan §5.6a).
          // Captured outer constants remain supported here: they materialize as phantom
          // parameters that print at the enclosing design's scope.
          dfConstValArgs.foreach { v =>
            report.error(
              s"""Constant arguments are not supported for ED methods.
                 |The `${v.name}` argument is a `<> CONST` value, which an ED method cannot take as a parameter.
                 |Use a `<> VAL` argument instead, reference a constant declared outside the method, or declare a static function (`<> CONSTRET`).""".stripMargin,
              v.srcPos
            )
            hasHDLMethodErrors = true
          }
        if (isStatic)
          // The inverse of the ED rule: a static function is a region in which every value is
          // constant, so a non-constant argument has no meaning in it. Its const args become
          // design PARAMETERS rather than input ports, which is also what lets a static function
          // be called from the global scope, where there is no block to own an input port's net.
          dfValArgs.foreach { v =>
            report.error(
              s"""Non-constant arguments are not supported for static functions.
                 |The `${v.name}` argument is a `<> VAL` value, but every value in a static function is constant.
                 |Use a `<> CONST` argument instead.""".stripMargin,
              v.srcPos
            )
            hasHDLMethodErrors = true
          }
          // static procedures (a `Unit` return with `out` formals, as VHDL procedures allow) are
          // explicitly deferred
          if (hasUnitRet(anonDef))
            hdlMethodError(
              "A static function must return a value. A `Unit` return type (a procedure) is not supported with `<> CONSTRET`."
            )
        end if
        // Compile-time enforcement of the method BODY content rules. The SanityCheck
        // stage's `hdlMethodCheck` remains the debug-mode backstop for constructs smuggled
        // in through helper defs, whose bodies this syntactic check cannot see.
        if (isHDLMethod)
          checkHDLMethodContent(anonDef, isStatic, isEDMethod && hasUnitRet(anonDef)) {
            (msg, pos) =>
              report.error(msg, pos)
              hasHDLMethodErrors = true
          }
        if (hasHDLMethodErrors) tree
        else
          val dfc = ContextArg.at(anonDef).get
          // out-of-scope value references become explicit: DFHDL constants as phantom design
          // parameters, DFHDL values as phantom input ports (both tagged so the method
          // view form hides them), and plain Scala values as cache key extensions. All are
          // evaluated in the def's rhs scope at every call, so a pure cache hit (which skips
          // the body) still binds this call's captured values.
          val captures = discoverMethodCaptures(sym, anonDef.symbol, anonDef.rhs)
          // A captured non-constant would become a phantom INPUT PORT, i.e. a non-constant input,
          // which contradicts staticness outright. This is the DFHDL-level half of a static
          // function's purity (`PureCheck` reasons about Scala-level effects and, since DFHDL's
          // own core is on its trusted list, would never flag this one).
          if (isStatic)
            captures.phantomVals.foreach { (path, t) =>
              report.error(
                s"""Non-constant captured values are not supported for static functions.
                   |The captured `${captureName(
                    path
                  )}` value is not a `<> CONST`, but every value in a static function is constant.
                   |Capture a constant instead, or pass it in as a `<> CONST` argument.""".stripMargin,
                t.srcPos
              )
            }
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
            // the def's nearest enclosing class anchors the future disk-tier code-identity
            // digest (`factum.CodeRef`): the def's body compiles into this class's class
            // file and TASTy, while the runtime lambda class itself is unresolvable
            val ownerClass = clsOf(sym.ownersIterator.find(_.isClass).get.typeRef)
            // calling the runtime method that constructs the design from the definition. All three
            // share a signature, caching, and purity treatment, and differ only in the domain the
            // design block is constructed under: DF, ED (an HDL method), or Static (an HDL
            // method whose formals are design parameters rather than input ports).
            val designFromDefKindSym =
              if (isStatic) designFromDefStaticSym
              else if (isEDMethod) designFromDefEDSym
              else designFromDefSym
            ref(designFromDefKindSym)
              .appliedToType(anonDef.dfValTpeOpt.get.widen)
              .appliedToArgs(List(
                args,
                constArgs,
                tree.genMeta, // meta represents the transformed tree
                scalaArgs,
                phantomArgs,
                phantomConstArgs,
                ownerClass
              ))
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

  // The method body content rules: a method (an ED method or a static function) body
  // may not construct design content that has no HDL-method form. Mirrors the rule set of
  // the SanityCheck stage's `hdlMethodCheck` backstop, but reports at compile time on the
  // offending expression. The check is syntactic over the def's own body: constructs reached
  // through helper defs are invisible here and are left to the backstop.
  private def checkHDLMethodContent(
      anonDef: DefDef,
      isStatic: Boolean,
      isProcedural: Boolean
  )(err: (String, util.SrcPos) => Unit)(using Context): Unit =
    val kindNoun = if (isStatic) "a static function" else "an ED method"
    def designInstanceError(pos: util.SrcPos): Unit =
      if (isStatic)
        err(
          "Design instances are not allowed inside a static function. Only calls to other static functions are.",
          pos
        )
      else
        err(
          "Design instances are not allowed inside an ED method. Only calls to other ED methods and to static functions are.",
          pos
        )
    object checker extends TreeTraverser:
      def traverse(t: Tree)(using Context): Unit =
        t match
          // nested methods (including nested methods) are checked independently by
          // their own `transformDefDef` pass
          case dd: DefDef if methodDesignAnonOf(dd).nonEmpty => // skip
          case nt: New                                       =>
            if (designCls.exists && nt.tpt.tpe.derivesFrom(designCls))
              designInstanceError(nt.srcPos)
            else if (domainContainerCls.exists && nt.tpt.tpe.derivesFrom(domainContainerCls))
              err(s"This construct is not allowed inside $kindNoun.", nt.srcPos)
          case ap @ Apply(fun, args) =>
            val funSym = fun.symbol
            // process/fork bodies are `DFC.Scope.Process ?=> Unit` context lambdas, so an
            // argument of that type identifies the construct regardless of its export path
            val opensProcessScope = scopeProcessCls.exists && args.exists { a =>
              a.tpe.dealias match
                case ContextFunctionType(ctxParams, _) =>
                  ctxParams.exists(_ <:< scopeProcessCls.typeRef)
                case _ => false
            }
            if (opensProcessScope)
              val ownerName = funSym.maybeOwner.name.toString
              if (ownerName == "process$" || ownerName == "process")
                err(s"Process blocks are not allowed inside $kindNoun.", ap.srcPos)
              else err(s"This construct is not allowed inside $kindNoun.", ap.srcPos)
            else if (funSym.name.toString == ":==")
              val msg =
                if (isStatic)
                  "Non-blocking assignments `:==` are not allowed inside a static function."
                else if (isProcedural)
                  "Non-blocking assignments `:==` are not allowed inside an ED method (writes to outer state are not yet supported)."
                else "Non-blocking assignments `:==` are not allowed inside an ED function."
              err(msg, ap.srcPos)
            else
              // the evidence arguments a call applies identify the callee kind: methods
              // take scope evidence (`Scope.Function`/`Scope.Procedural`) and their domain
              // evidence separates ED methods (`DomainType.ED`) from static functions; DF
              // methods take `DomainType.DF`
              def hasArgOf(cls: Symbol): Boolean =
                cls.exists && args.exists(_.tpe <:< cls.typeRef)
              def hasDomainArg(domainSym: Symbol): Boolean =
                // a summoned evidence argument's type is a TermRef of the given; widen it to
                // reach the (opaque) domain evidence type itself
                domainSym.exists &&
                  args.exists(_.tpe.widenTermRefExpr.dealias.typeSymbol == domainSym)
              val isHDLMethodCall = hasArgOf(scopeFunctionCls) || hasArgOf(scopeProceduralCls)
              if (isStatic && isHDLMethodCall && hasDomainArg(domainTypeEDSym))
                err(
                  "ED method calls are not allowed inside a static function. A static function is callable from any domain, so it may only call other static functions.",
                  ap.srcPos
                )
              else if (
                !isHDLMethodCall &&
                ((hasDomainArg(domainTypeDFSym) && ap.tpe.dfValTpeOpt.nonEmpty) ||
                  (isDFHDLMethod(funSym) && !methodDesignAnon(funSym).exists(isHDLMethodAnonDef)))
              )
                designInstanceError(ap.srcPos)
            end if
            traverseChildren(t)
          case _ => traverseChildren(t)
      end traverse
    end checker
    checker.traverse(anonDef.rhs)
  end checkHDLMethodContent

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    designFromDefSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDef")
    designFromDefEDSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefED")
    designFromDefStaticSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefStatic")
    designFromDefGetInputSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefGetInput")
    designFromDefGetParamSym = requiredMethod("dfhdl.core.r__For_Plugin.designFromDefGetParam")
    irDFUnitCls = requiredClass("dfhdl.compiler.ir.DFUnit")
    scopeProcessCls = getClassIfDefined("dfhdl.core.DFC.Scope.Process")
    designCls = getClassIfDefined("dfhdl.core.Design")
    domainContainerCls = getClassIfDefined("dfhdl.core.DomainContainer")
    // the unit's methods, registered before any of it is transformed (see `collectDFHDLMethods`)
    collectDFHDLMethods(tree)
    ctx
end MethodsPhase
