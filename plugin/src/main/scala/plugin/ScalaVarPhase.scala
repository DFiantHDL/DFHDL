package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Decorators.*
import ast.Trees.*
import ast.tpd
import Names.*
import Types.*

import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable

/** The rules for a Scala `var` that holds a DFHDL value.
  *
  * A Scala `var` is rebound at ELABORATION time, while a DFHDL variable is assigned at RUNTIME with
  * `:=`. The two look alike and mean different things, so a `var` is admitted only in the positions
  * where it cannot express something the elaboration cannot honour, and rejected everywhere else.
  * There is deliberately no relaxation flag: an allowed position must be safe by construction.
  *
  * The permission list (see devdocs/scala-var-rules.md for the reasoning):
  *
  *   1. a CONSTANT `var` inside a `simulation { ... }` host block is exempt from the scope rules (2
  *      and 3): it is plain testbench Scala, not elaboration
  *   1. no `var` DECLARATION inside a sequential scope
  *   1. no `var` ACCESS from a sequential scope, nor from inside a named method
  *   1. a DFHDL `var` must be `private` (or local)
  *   1. a DFHDL `var` must be ascribed `T <> VAL` or `T <> CONST`
  *   1. no `var` holding a design, domain, or interface instance
  *
  * `DB.blockScopeCheck` is the elaboration backstop for what a lexical view cannot see.
  *
  * The phase runs between the typer and `MetaContextPlacer`, so it reads the user's own trees:
  * before inlining, before the loop/control rewrites, and with source positions intact.
  */
class ScalaVarPhase(setting: Setting) extends CommonPhase:
  import tpd.*

  val phaseName = "ScalaVar"

  override val runsAfter = Set("CodeDigest")
  override val runsBefore = Set("MetaContextPlacer")

  private var containerCls: Symbol = NoSymbol
  private var simCtxCls: Symbol = NoSymbol
  private var scopeSequenceCls: Symbol = NoSymbol
  private var scopeFunctionCls: Symbol = NoSymbol
  private var edDomainSym: Symbol = NoSymbol
  private var modifierValTpe: Type = NoType
  private var modifierConstTpe: Type = NoType
  private val tDomainName = "TDomain".toTypeName
  // one diagnostic per (var, offending owner), so a method that touches the same `var` twice
  // reports once, at its first access
  private val reportedAccess = mutable.Set.empty[(Symbol, Symbol)]

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    containerCls = getClassIfDefined("dfhdl.core.Container")
    // `dfhdl.sim.SimCtx` lives in the `compiler_stages` subproject and is not on the classpath
    // of every compilation the plugin runs in, so it is resolved defensively
    simCtxCls = getClassIfDefined("dfhdl.sim.SimCtx")
    scopeSequenceCls = getClassIfDefined("dfhdl.core.DFC.Scope.Sequence")
    scopeFunctionCls = getClassIfDefined("dfhdl.core.DFC.Scope.Function")
    val domainTypeMod = getModuleIfDefined("dfhdl.core.DomainType")
    edDomainSym =
      if (domainTypeMod.exists) domainTypeMod.moduleClass.info.member("ED".toTypeName).symbol
      else NoSymbol
    val modifierRef = requiredClassRef("dfhdl.core.Modifier")
    // `T <> VAL` is `DFVal[_, Modifier[Any, Any, Any, Any]]` and `T <> CONST` is
    // `DFVal[_, Modifier[Any, Any, Any, ISCONST[true]]]`; every other declaration modifier
    // (`VAR`, `IN`, `OUT`, `INOUT`, and their `.REG`/`.SHARED`/`.NB` variants) narrows at least
    // one of the first three arguments
    modifierValTpe =
      modifierRef.appliedTo(List(defn.AnyType, defn.AnyType, defn.AnyType, defn.AnyType))
    modifierConstTpe =
      modifierRef.appliedTo(List(defn.AnyType, defn.AnyType, defn.AnyType, constModTpe))
    reportedAccess.clear()
    ctx
  end prepareForUnit

  private enum ScopeKind derives CanEqual:
    case Concurrent, Sequential

  private enum Barrier derives CanEqual:
    case Sequential, Method

  /** The DFHDL scope a symbol's own body establishes, if it establishes one at all.
    *
    * Two shapes carry it, and neither needs anything the compiler cannot already see:
    *   - a sequential BLOCK (`process`, `initial`, an HDL method body) arrives as a CONTEXT
    *     FUNCTION, so its scope is a real parameter on an anonymous method (`DFC.Scope.Process ?=>
    *     Unit`, `(DFC, DomainType.ED, DFC.Scope.Function) ?=> ...`);
    *   - a design or domain BODY is a class body, discriminated by `Container.TDomain`: an
    *     event-driven (ED) body is the only concurrent scope, RT and DF bodies are sequential.
    *
    * Everything else (a plain `def`, a lambda, a `val` initializer, a class local dummy)
    * establishes no scope of its own and inherits the enclosing one.
    *
    * Reading a parameter's DECLARED type is what keeps this sound. The Scala-side form of the same
    * question is booby-trapped: `Scope.Function`'s given is ambient, so a plain summon of any
    * capability it has succeeds from anywhere (see devdocs/scoping.md §3).
    */
  private def scopeKindOf(sym: Symbol)(using Context): Option[ScopeKind] =
    if (sym.is(Method))
      val carriesScope = sym.paramSymss.iterator.flatten.exists { p =>
        p.isTerm && (
          scopeSequenceCls.exists && p.info.derivesFrom(scopeSequenceCls) ||
            scopeFunctionCls.exists && p.info.derivesFrom(scopeFunctionCls)
        )
      }
      if (carriesScope) Some(ScopeKind.Sequential) else None
    else if (sym.isClass && containerCls.exists && sym.derivesFrom(containerCls))
      // SYMBOL identity, never `<:<`: `DomainType.ED`, `.RT` and `.DF` are opaque aliases of the
      // same `Dynamic`, and opacity is already gone by this phase, so every pair of them is
      // mutually conforming here. Subtyping would classify an RT body as event-driven.
      if (
        edDomainSym.exists && sym.typeRef.member(tDomainName).info.hiBound.typeSymbol == edDomainSym
      )
        Some(ScopeKind.Concurrent)
      else Some(ScopeKind.Sequential)
    else None

  /** The scope `from` sits in: the innermost enclosing owner that establishes one. */
  private def enclosingScopeKind(from: Symbol)(using Context): Option[ScopeKind] =
    from.ownersIterator.flatMap(scopeKindOf(_)).nextOption()

  /** The first thing between an access and the `var`'s own owner that makes the access unreachable
    * from the elaboration that runs the declaration.
    */
  private def barrierBetween(access: Symbol, declOwner: Symbol)(using Context): Option[Barrier] =
    access.ownersIterator.takeWhile(_ != declOwner).flatMap { o =>
      if (o.isConstructor || o.isLocalDummy) None
      else
        scopeKindOf(o) match
          case Some(ScopeKind.Sequential) => Some(Barrier.Sequential)
          case Some(ScopeKind.Concurrent) => None
          // A NAMED method can be invoked from a scope other than the one it is defined in,
          // including from inside a hardware loop. A lambda (`(0 until 4).foreach(i => ...)`,
          // which is what a `for` over a Scala range desugars to) runs in place, so it is
          // transparent: banning it would ban the very accumulator idiom this list preserves.
          // The scope test above must come FIRST: a sequential block arrives as a synthetic
          // anonymous method, so both exclusions here would otherwise skip it.
          case None =>
            Option.when(o.is(Method) && !o.isAnonymousFunction && !o.is(Synthetic))(Barrier.Method)
    }.nextOption()

  private def isDFValTpe(tpe: Type)(using Context): Boolean = tpe.dfValTpeOpt.nonEmpty
  private def isContainerTpe(tpe: Type)(using Context): Boolean =
    containerCls.exists && tpe.derivesFrom(containerCls)

  /** `T <> VAL` or `T <> CONST`, and nothing else. */
  private def isValOrConstTpe(tpe: Type)(using Context): Boolean =
    tpe.widenDealias match
      case AppliedType(_, _ :: modifierTpe :: Nil) =>
        modifierTpe =:= modifierValTpe || modifierTpe =:= modifierConstTpe
      case _ => false

  /** The same DFHDL type, re-shown with the modifier the ascription must carry: the exact text to
    * write. A constant keeps `<> CONST`, everything else becomes `<> VAL`.
    */
  private def ascriptionFor(tpe: Type)(using Context): String =
    val modifier = if (tpe.isDFConst) modifierConstTpe else modifierValTpe
    tpe.widenDealias match
      case AppliedType(tycon, dfTypeTpe :: _ :: Nil) =>
        AppliedType(tycon, List(dfTypeTpe, modifier)).show
      case _ => "T <> VAL"

  /** An inferred type tree covers no source of its own. */
  private def isInferredTpt(tpt: Tree)(using Context): Boolean =
    !tpt.span.exists || tpt.span.start == tpt.span.end

  /** Within a `simulation { ... }` host block, holding a DFHDL CONSTANT reference model in a Scala
    * `var` (e.g. `var model: UInt[8] <> CONST = d"8'0"` updated by constant arithmetic) is an
    * intentional, idiomatic testbench pattern: nothing there is elaborated, so the scope rules have
    * nothing to protect. The waiver is limited to constants, and it does not extend to the
    * declaration rules (4, 5 and 6), which a testbench `var` satisfies on its own.
    */
  private def isWaivedSimConstVar(sym: Symbol, tpe: Type)(using Context): Boolean =
    tpe.isDFConst && simCtxCls.exists && sym.ownersIterator.exists { owner =>
      // the `SimCtx ?=> ...` host-block context function becomes an anonymous method carrying a
      // `SimCtx` parameter, and its presence in the owner chain marks the simulation block
      owner.is(Method) &&
      owner.paramSymss.iterator.flatten.exists(_.info.typeSymbol == simCtxCls)
    }

  private def isCandidateVar(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.isTerm && sym.is(Mutable) && !sym.is(Synthetic) && !sym.is(Param)

  override def transformValDef(tree: ValDef)(using Context): Tree =
    val sym = tree.symbol
    if (isCandidateVar(sym))
      val tpe = tree.tpt.tpe.widen
      if (isContainerTpe(tpe))
        report.error(
          """|A Scala `var` cannot hold a DFHDL design, domain, or interface instance.
             |An instance is structural: it is created once, during elaboration, and rebinding the
             |Scala name neither removes the old instance nor creates a new one.
             |To Fix: change the `var` to a `val`.""".stripMargin,
          tree.srcPos
        )
      else if (isDFValTpe(tpe))
        // ~~~ rule 4: `private` or local ~~~
        if (sym.owner.isClass && !sym.is(Private))
          report.error(
            s"""|A Scala `var` holding a DFHDL value must be `private`.
                |A public (or `protected`) `var` member stays reassignable from outside the design
                |once elaboration is over, and it takes part in the design's selectable surface.
                |To Fix: add the `private` modifier, or make `${sym.name}` a local `var`.""".stripMargin,
            tree.srcPos
          )
        // ~~~ rule 5: ascribed `<> VAL` or `<> CONST` ~~~
        if (isInferredTpt(tree.tpt))
          report.error(
            s"""|A Scala `var` holding a DFHDL value must be explicitly ascribed `T <> VAL` or `T <> CONST`.
                |An inferred type comes from the initializer, so it fixes the width at the first
                |assignment (`var acc = x(0).bits` infers `Bits[1]`) and it carries the initializer's
                |scope, domain and assignability markers into every later use.
                |To Fix: write the type, e.g. `var ${sym.name}: ${ascriptionFor(tpe)} = ...`.
                |Note that an unbounded `Int` width is checked at elaboration, not at compile time.""".stripMargin,
            tree.srcPos
          )
        else if (!isValOrConstTpe(tpe))
          report.error(
            s"""|A Scala `var` holding a DFHDL value must be ascribed `T <> VAL` or `T <> CONST`, not `${tpe.show}`.
                |A Scala `var` is rebound with `=` during elaboration, while a DFHDL variable or port
                |is assigned with `:=` or connected with `<>`. Holding one in a Scala `var` mixes the two.
                |To Fix:
                |* To rebind a value during elaboration, ascribe `<> VAL` (or `<> CONST`).
                |* To assign in hardware, declare the variable in a `val` and use `:=`.""".stripMargin,
            tree.tpt.srcPos
          )
        // ~~~ rule 2: no declaration inside a sequential scope ~~~
        if (
          !isWaivedSimConstVar(sym, tpe) &&
          enclosingScopeKind(sym.owner).contains(ScopeKind.Sequential)
        )
          report.error(
            s"""|A Scala `var` holding a DFHDL value cannot be declared inside a sequential scope.
                |A sequential scope (a process, an `initial` block, a method body, or a
                |register-transfer (RT) or dataflow (DF) design or domain body) is elaborated once,
                |not once per execution, so a Scala `var` there cannot accumulate across it:
                |reassigning it only rebinds the Scala name.
                |To Fix:
                |* To accumulate in hardware, declare a DFHDL variable (`<> VAR`) and assign it with `:=`.
                |* To accumulate during elaboration, move `${sym.name}` to an event-driven (ED) design or domain body.
                |* If `${sym.name}` is never reassigned, change it to a `val`.""".stripMargin,
            tree.srcPos
          )
      end if
    end if
    tree
  end transformValDef

  private def checkVarAccess(tree: Tree)(using Context): Unit =
    val sym = tree.symbol
    if (isCandidateVar(sym))
      val tpe = sym.info.widen
      if (isDFValTpe(tpe) && !isWaivedSimConstVar(sym, tpe) && ctx.owner.isContainedIn(sym.owner))
        barrierBetween(ctx.owner, sym.owner).foreach { barrier =>
          if (reportedAccess.add((sym, ctx.owner)))
            val where = barrier match
              case Barrier.Sequential =>
                """|A sequential scope is elaborated once, so the access would run inside the
                   |elaborated hardware rather than during the accumulation.""".stripMargin
              case Barrier.Method =>
                """|A method can be invoked from anywhere, including from inside a hardware loop, so
                   |an access from a method body cannot be shown to run during elaboration.""".stripMargin
            report.error(
              s"""|A Scala `var` holding a DFHDL value cannot be accessed from here.
                  |$where
                  |To Fix:
                  |* Freeze the accumulated value first (`val frozen = ${sym.name}`) and use that instead.
                  |* To accumulate in hardware, declare a DFHDL variable (`<> VAR`) and assign it with `:=`.""".stripMargin,
              tree.srcPos
            )
        }
    end if
  end checkVarAccess

  override def transformIdent(tree: Ident)(using Context): Tree =
    checkVarAccess(tree)
    tree

  override def transformSelect(tree: Select)(using Context): Tree =
    checkVarAccess(tree)
    tree
end ScalaVarPhase
