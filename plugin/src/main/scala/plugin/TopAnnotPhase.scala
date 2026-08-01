package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.{tpd, untpd, TreeTypeMap}
import StdNames.nme
import Names.{Designator, *}
import Constants.Constant
import Types.*

import scala.language.implicitConversions
import scala.compiletime.uninitialized
import collection.mutable
import annotation.tailrec
import dotty.tools.dotc.ast.Trees.Alternative
import dotty.tools.dotc.semanticdb.BooleanConstant
import java.time.Instant

/*
  This phase injects a `main` entry point for each design that has a `@top`
  annotation. The `main` instantiates a `DFApp`, primes it, and reroutes the argv
  to `DFApp.run`; the design instance is built lazily from the (possibly
  CLI-overridden) argument values. The design parameters must have default
  arguments of the supported types, which the command-line options may override.

  Where the `main` is hosted depends on how the design is nested:
    - Top-level design (directly in a package): `main` is injected into the
      design's own companion object (guaranteed to exist — PreTyperPhase
      synthesizes an empty one when the user did not write it), so the companion
      (e.g. `Foo`) is itself the runnable entry point — no separate object.
    - Design nested inside object(s): a nested companion cannot be exposed as a
      runnable top-level `main` (the JVM/Zinc do not surface it under a usable
      name), so a fresh top-level object named by the nesting path (e.g.
      `inside_Foo` for `object inside { class Foo }`) is generated at the
      enclosing package to host the `main`.
 */
class TopAnnotPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "TopAnnot"

  override val runsAfter = Set("typer")
  override val runsBefore = Set("MetaContextGen")
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")
  var topAnnotSym: ClassSymbol = uninitialized
  var appTpe: TypeRef = uninitialized
  var dfcTpe: TypeRef = uninitialized
  var designTpe: TypeRef = uninitialized
  var resourceOwnerTpe: TypeRef = uninitialized
  var portModTpe: TypeRef = uninitialized
  // IR type discriminators — the first type arg of the front-end `DFType[IR, Args]`
  // alias, always reducible to a class symbol.
  var irDFBoolClsSym: ClassSymbol = uninitialized
  var irDFBitClsSym: ClassSymbol = uninitialized
  var irDFStringClsSym: ClassSymbol = uninitialized
  var irDFDoubleClsSym: ClassSymbol = uninitialized
  var irDFBitsClsSym: ClassSymbol = uninitialized
  var irDFDecimalClsSym: ClassSymbol = uninitialized
  // Symbols for the `defaults.*` helpers in r__For_Plugin
  var defaultsBoolSym: TermSymbol = uninitialized
  var defaultsBitSym: TermSymbol = uninitialized
  var defaultsInt32Sym: TermSymbol = uninitialized
  var defaultsStringSym: TermSymbol = uninitialized
  var defaultsDoubleSym: TermSymbol = uninitialized
  var defaultsBitsSym: TermSymbol = uninitialized
  var defaultsUIntSym: TermSymbol = uninitialized
  var defaultsSIntSym: TermSymbol = uninitialized
  // `DFC.emptyNoEO` — an empty DFC instance supplied when we inject a synthetic
  // default tree, since there is no DFC in scope where setInitials evaluates them.
  var emptyNoEODFCSym: TermSymbol = uninitialized

  // The enclosing package of the current owner.
  private def getPackageOwner(using Context): Symbol =
    var owner = ctx.owner
    while (!owner.is(Package))
      owner = owner.owner
    owner

  // The plan for a `@top` design's entry point:
  //   - `CompanionInject` for a top-level design: splice `main` into its own
  //     companion module (guaranteed to exist), so the design is runnable under
  //     its plain name (e.g. `Foo`).
  //   - `PackageObject` for a design nested inside object(s): a new top-level
  //     object at package level (named by the nesting path, e.g. `inside_Foo`)
  //     that hosts `main`. A nested companion cannot serve as the entry point —
  //     the JVM/Zinc do not expose a nested object's `main` under a runnable
  //     top-level name.
  private enum EntryPlan:
    case CompanionInject(moduleClsSym: ClassSymbol, mainDef: DefDef)
    case PackageObject(moduleDef: Thicket)

  // Buffer for `PackageObject` thickets discovered while transforming the stats
  // of a nested owner; flushed into the enclosing package's stat list.
  private val pendingPackageObjects = mutable.ListBuffer.empty[Thicket]

  // Extract a literal `Int` from a type arg (e.g. from `Args1[8]` or the width
  // slot of `Args4[_, 8, _, _]`). Returns None for non-literal widths.
  private def literalIntOf(tpe: Type)(using Context): Option[Int] =
    tpe.dealias match
      case ConstantType(Constant(w: Int)) => Some(w)
      case _                              => None

  // Apply the empty DFC to a synthetic default call so the tree is
  // self-contained (no DFC is in scope at DFApp.setInitials call time).
  private def withEmptyDFC(tree: Tree)(using Context): Tree =
    tree.appliedTo(ref(emptyNoEODFCSym))

  private def nullaryDefault(sym: TermSymbol)(using Context): Tree =
    withEmptyDFC(ref(sym))

  private def bitsDefault(sym: TermSymbol, w: Int)(using Context): Tree =
    withEmptyDFC(
      ref(sym)
        .appliedToType(ConstantType(Constant(w)))
        .appliedTo(Literal(Constant(w)))
    )

  // Given the second type arg of `DFType[ir.DFDecimal, Args4[S, W, 0, N]]`,
  // dispatch to the right `defaults.*` helper. `N = true` is Int32, `N = false`
  // is BitAccurate — with signed bit selecting UInt vs SInt.
  private def literalBoolOf(tpe: Type)(using Context): Option[Boolean] =
    tpe.dealias match
      case ConstantType(Constant(b: Boolean)) => Some(b)
      case _                                  => None

  private def decimalDefault(argsTpe: Type)(using Context): Option[Tree] =
    argsTpe.dealias match
      case AppliedType(_, signedTpe :: widthTpe :: _ :: nativeTpe :: Nil) =>
        literalBoolOf(nativeTpe) match
          // Int32 wildcard — defaults.int32 (signed/width irrelevant here).
          case Some(true) => Some(nullaryDefault(defaultsInt32Sym))
          // BitAccurate — UInt[W] / SInt[W] picked by the `signed` slot.
          case Some(false) =>
            (literalBoolOf(signedTpe), literalIntOf(widthTpe)) match
              case (Some(false), Some(w)) => Some(bitsDefault(defaultsUIntSym, w))
              case (Some(true), Some(w))  => Some(bitsDefault(defaultsSIntSym, w))
              case _                      => None
          case _ => None
      case _ => None

  // Walk through AndType intersections (e.g. `DFBit & DFTypeAny` that
  // `DFType.Of` produces for its DFTypeAny branch) and pick the first branch
  // that normalizes to `DFType[IR, Args]`. Returns the (IR classSymbol, Args
  // type arg) pair, or None if no branch fits the shape.
  private def dfTypeShape(tpe: Type)(using Context): Option[(Symbol, Type)] =
    tpe.widenDealias match
      case AppliedType(_, irTpe :: argsTpe :: Nil) =>
        Some(irTpe.classSymbol -> argsTpe)
      case AndType(tp1, tp2) =>
        dfTypeShape(tp1).orElse(dfTypeShape(tp2))
      case _ => None

  // If `vd` is a `<> CONST` parameter of a supported primitive type, return a
  // tree that evaluates to a tagged synthetic default. Otherwise None.
  private def synthDefaultFor(vd: ValDef)(using Context): Option[Tree] =
    if (!vd.tpt.tpe.isDFConst) None
    else
      vd.tpt.tpe.widenDealias match
        case AppliedType(_, dfTypeTpe :: _ :: Nil) => // DFVal[dfTypeTpe, mod]
          dfTypeShape(dfTypeTpe) match
            case Some((irCls, argsTpe)) =>
              if (irCls == irDFBoolClsSym) Some(nullaryDefault(defaultsBoolSym))
              else if (irCls == irDFBitClsSym) Some(nullaryDefault(defaultsBitSym))
              else if (irCls == irDFStringClsSym) Some(nullaryDefault(defaultsStringSym))
              else if (irCls == irDFDoubleClsSym) Some(nullaryDefault(defaultsDoubleSym))
              else if (irCls == irDFBitsClsSym)
                argsTpe.dealias match
                  case AppliedType(_, widthTpe :: Nil) =>
                    literalIntOf(widthTpe).map(w => bitsDefault(defaultsBitsSym, w))
                  case _ => None
              else if (irCls == irDFDecimalClsSym) decimalDefault(argsTpe)
              else None
            case None => None
        case _ => None
    end if
  end synthDefaultFor

  private def portCond(tpe: Type)(using Context): Boolean =
    if (tpe.typeSymbol == dfValSym)
      tpe match
        // DFVal[_, Modifier[Port, _, _, _]]
        case AppliedType(_, _ :: AppliedType(_, a :: _) :: Nil) => a <:< portModTpe
        case _                                                  => false
    else false

  // Build the `main(args)` entry point hosted by `moduleClsSym` (a companion
  // module class). Rather than extending `DFApp`, the body instantiates one,
  // primes it via `setInitials`, hands it the design thunk via `setDsn`, and
  // reroutes the raw argv to `DFApp.run`. The design instance itself is built
  // lazily from the (possibly CLI-overridden) `getDsnArg` values.
  private def mkMainDef(
      moduleClsSym: ClassSymbol,
      clsSym: ClassSymbol,
      paramVDs: List[ValDef],
      classOfTree: Tree,
      designNameTree: Tree,
      topScalaPathTree: Tree,
      topAnnotTree: Tree,
      dsnArgNames: Tree,
      dsnArgValues: Tree,
      dsnArgDescs: Tree,
      werror: Tree,
      hasResourceOwnerTree: Tree,
      hasPorts: Tree,
      span: util.Spans.Span
  )(using Context): DefDef =
    val mainSym = newSymbol(
      moduleClsSym,
      "main".toTermName,
      Method,
      MethodType(
        List("args".toTermName),
        List(defn.ArrayOf(defn.StringType)),
        defn.UnitType
      ),
      coord = span
    ).entered.asTerm
    val mainDef = DefDef(
      mainSym,
      paramss =>
        val argsRef = paramss.head.head
        val appSym = newSymbol(mainSym, "app".toTermName, Synthetic, appTpe, coord = span).asTerm
        val appVal = ValDef(appSym, New(appTpe, Nil))
        val setInitialsCall =
          ref(appSym).select("setInitials".toTermName).appliedToArgs(
            List(
              classOfTree, designNameTree, topScalaPathTree, topAnnotTree, dsnArgNames,
              dsnArgValues, dsnArgDescs, werror, hasResourceOwnerTree, hasPorts
            )
          )
        val dsnInstArgs = paramVDs.map(vd =>
          ref(appSym).select("getDsnArg".toTermName).appliedTo(Literal(Constant(vd.name.toString)))
        )
        // The instantiation is hosted by a local method taking a DFHDL context, so that the
        // MetaContextPlacer phase binds the design to that parameter (its normal rule: a
        // design takes the context in scope where it is instantiated) instead of falling back
        // to the elaboration options at the design's declaration site. The app supplies the
        // context, so the top design elaborates under the options the app resolved, command
        // line included. `setDsn` takes its argument by name, so the call below runs at
        // elaboration time, once `run` has had the command line.
        val mkDsnSym = newSymbol(
          mainSym,
          "mkDsn".toTermName,
          Synthetic | Method,
          MethodType(List("dfc".toTermName), List(dfcTpe), clsSym.typeRef),
          coord = span
        ).asTerm
        val mkDsnDef = DefDef(mkDsnSym, _ => New(clsSym.typeRef, dsnInstArgs))
        val dsnInst =
          ref(mkDsnSym).appliedTo(ref(appSym).select("getDsnDFC".toTermName))
        val setDsnCall = ref(appSym).select("setDsn".toTermName).appliedTo(dsnInst)
        val runCall = ref(appSym).select("run".toTermName).appliedTo(argsRef)
        Block(List(appVal, setInitialsCall, mkDsnDef, setDsnCall), runCall)
    )
    mainDef.withSpan(span)
  end mkMainDef

  // Build a fresh top-level object at `packageOwner` named `moduleName`, hosting
  // the entry-point `main`. Used for designs nested inside object(s), which
  // cannot expose a runnable `main` through their (nested) companion.
  private def mkPackageEntryObject(
      packageOwner: Symbol,
      moduleName: String,
      clsSym: ClassSymbol,
      paramVDs: List[ValDef],
      classOfTree: Tree,
      designNameTree: Tree,
      topAnnotTree: Tree,
      dsnArgNames: Tree,
      dsnArgValues: Tree,
      dsnArgDescs: Tree,
      werror: Tree,
      hasResourceOwnerTree: Tree,
      hasPorts: Tree,
      span: util.Spans.Span
  )(using Context): Thicket =
    val moduleSym = newCompleteModuleSymbol(
      packageOwner,
      moduleName.toTermName,
      Touched,
      Touched | NoInits,
      List(defn.ObjectType),
      Scopes.newScope,
      coord = span,
      compUnitInfo = clsSym.compUnitInfo
    )
    val moduleClsSym = moduleSym.moduleClass.asClass
    // The runnable Scala path is this new top-level object.
    val topScalaPathTree = Literal(Constant(moduleSym.fullName.toString))
    val mainDef = mkMainDef(
      moduleClsSym, clsSym, paramVDs, classOfTree, designNameTree, topScalaPathTree,
      topAnnotTree, dsnArgNames, dsnArgValues, dsnArgDescs, werror, hasResourceOwnerTree,
      hasPorts, span
    )
    // Give the module def a real source span (the design class's name) so tooling
    // (e.g. Metals run/debug code lenses) anchors on the design class.
    Thicket(ModuleDef(moduleSym, List(mainDef)).trees.map(_.withSpan(span)))
  end mkPackageEntryObject

  // For a `@top` design class `td`, build the entry-point `main` to inject into
  // its companion module class (guaranteed to exist — synthesized by PreTyper
  // when the user did not write one). Returns the (companion module class,
  // `main` DefDef) pair, or None when no entry point should be generated (e.g.
  // `@top(false)`, a non-Design `@top(true)`, or a validation error).
  private def planEntryPoint(td: TypeDef, template: Template, trees: List[Tree])(using
      Context
  ): Option[EntryPlan] =
    val clsSym = td.symbol.asClass
    clsSym.getAnnotation(topAnnotSym).map(a => dropProxies(a.tree)) match
      case Some(topAnnotTree @ Apply(Apply(Apply(_, topAnnotOptionsTrees), _), _)) =>
        // Distinguish `@top` (default genMain) from explicit `@top(true)`/`@top(false)`.
        val (genMain, genMainIsExplicit) = topAnnotOptionsTrees match
          case Literal(Constant(b: Boolean)) :: _ => (b, true)
          case NamedArg(n, Literal(Constant(b: Boolean))) :: _ if n.toString == "genMain" =>
            (b, true)
          case _ => (true, false)
        val isDesign = td.tpe <:< designTpe
        if (!genMain) None
        else if (!isDesign)
          if (genMainIsExplicit) None // `@top(true)` — silently skip
          else
            report.error(
              "`@top` must be applied to a subclass of `dfhdl.core.Design`.\n" +
                "Use `@top(true)` if you want the top entry point to be silently skipped " +
                "for non-Design classes, or `@top(false)` to disable entry point generation.",
              topAnnotTree.srcPos
            )
            None
        else if (template.constr.paramss.length > 1)
          report.error(
            "Unsupported multiple parameter blocks for top-level design.",
            template.constr.srcPos
          )
          None
        else
          // The entry point must be reachable as a top-level name, i.e. the
          // design lives directly in a package or nested only inside objects.
          val nested = !clsSym.owner.is(Package)
          var owner = clsSym.owner
          while (!owner.is(Package) && owner.is(ModuleClass))
            owner = owner.owner
          if (!owner.is(Package))
            report.error(
              "Top-level main generation must be defined only in nested objects or packages.",
              topAnnotTree.srcPos
            )
            None
          else
            // Anchor the entry point on the design class's name span so tooling
            // (e.g. Metals run/debug code lenses) positions on the class.
            val span = td.nameSpan
            val companionModuleSym = clsSym.companionModule
            // The companion module class TypeDef (and its template) within the
            // current stat list, if a companion already exists. A companion is
            // also synthesized by the typer to hold constructor default getters,
            // so designs with Scala-level default args always have one here.
            val companionInfo: Option[(TypeDef, Template)] =
              if (!companionModuleSym.exists) None
              else
                trees.collectFirst {
                  case ctd @ TypeDef(_, ct: Template)
                      if ctd.symbol == companionModuleSym.moduleClass =>
                    (ctd, ct)
                }
            // Constructor default getters live in the companion template; map
            // each parameter index to a reference to its default getter.
            val defaultMap = mutable.Map.empty[Int, Tree]
            companionInfo.foreach { (_, ct) =>
              ct.body.foreach {
                case dd @ DefDef(name = NameKinds.DefaultGetterName(_, i)) =>
                  defaultMap += i -> ref(companionModuleSym).select(dd.symbol)
                case _ =>
              }
            }
            val paramVDs = template.constr.paramss.flatten.collect { case vd: ValDef => vd }
            val classOfTree = Literal(Constant(clsSym.typeRef))
            val designNameTree = Literal(Constant(clsSym.name.toString))
            val dsnArgNames = mkList(paramVDs.map(vd => Literal(Constant(vd.name.toString))))
            val dsnArgValues =
              mkList(
                paramVDs.zipWithIndex.map((vd, i) =>
                  defaultMap.get(i).orElse(synthDefaultFor(vd)) match
                    case Some(value) => value
                    case None        =>
                      report.error(
                        "Missing argument's default value for top-level design with a default app entry point.\nEither add a default value, use one of the supported `<> CONST` primitive types (Int, Boolean, Bit, String, Double, Bits[W], UInt[W], SInt[W] with literal W), or disable the app entry point generation with `@top(false)`.",
                        vd.srcPos
                      )
                      EmptyTree
                )
              )
            val dsnArgDescs =
              mkList(paramVDs.map(vd => Literal(Constant(vd.symbol.docString.getOrElse("")))))
            val werror = Literal(Constant(ctx.settings.Werror.value))
            val hasResourceOwnerTree =
              Literal(Constant(clsSym.hasNestedMemberCond(_ <:< resourceOwnerTpe)))
            val hasPorts = Literal(Constant(clsSym.hasNestedMemberCond(portCond)))

            if (nested)
              // A design nested inside object(s) cannot expose a runnable `main`
              // through its (nested) companion, so host the entry point in a fresh
              // top-level object named by the nesting path (e.g. `inside_Foo`).
              val packageOwner = getPackageOwner
              var moduleName = td.name.toString
              var o = clsSym.owner
              while (!o.is(Package) && o.is(ModuleClass))
                moduleName = s"${o.name.toString.replace("$", "")}_$moduleName"
                o = o.owner
              inContext(ctx.withOwner(packageOwner)):
                val thicket = mkPackageEntryObject(
                  packageOwner, moduleName, clsSym, paramVDs, classOfTree, designNameTree,
                  topAnnotTree, dsnArgNames, dsnArgValues, dsnArgDescs, werror,
                  hasResourceOwnerTree, hasPorts, span
                )
                Some(EntryPlan.PackageObject(thicket))
            else
              // Top-level design: inject `main` into its own companion module
              // (guaranteed to exist — synthesized by PreTyper when absent), so the
              // design is runnable under its plain name.
              val topScalaPathTree = Literal(Constant(clsSym.fullName.toString))
              companionInfo match
                case Some((compTd, compTemplate)) =>
                  val alreadyHasMain = compTemplate.body.exists {
                    case dd: DefDef => dd.name.toString == "main"
                    case _          => false
                  }
                  if (alreadyHasMain)
                    report.error(
                      "The companion object already defines a `main`; cannot generate a top-level entry point. Use `@top(false)` to disable entry point generation.",
                      topAnnotTree.srcPos
                    )
                    None
                  else
                    val moduleClsSym = compTd.symbol.asClass
                    val mainDef = mkMainDef(
                      moduleClsSym, clsSym, paramVDs, classOfTree, designNameTree,
                      topScalaPathTree, topAnnotTree, dsnArgNames, dsnArgValues, dsnArgDescs,
                      werror, hasResourceOwnerTree, hasPorts, span
                    )
                    Some(EntryPlan.CompanionInject(moduleClsSym, mainDef))
                case None =>
                  report.error(
                    "Internal error: expected a companion object for the top-level design " +
                      "(it should have been synthesized in the PreTyper phase).",
                    topAnnotTree.srcPos
                  )
                  None
              end match
            end if
          end if
        end if
      case _ => None
    end match
  end planEntryPoint

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    // First pass: plan an entry point for every `@top` design class in this stat
    // list. Top-level designs inject `main` into their companion module (spliced
    // below); designs nested inside object(s) produce a fresh top-level object
    // that must be added at the enclosing package's stat list.
    val injections = mutable.Map.empty[Symbol, DefDef] // companion module class -> main
    val packageObjects = mutable.ListBuffer.empty[Thicket]
    trees.foreach {
      case td @ TypeDef(_, template: Template)
          if td.symbol.isClass && !td.symbol.is(Trait) && td.symbol.hasAnnotation(topAnnotSym) =>
        planEntryPoint(td, template, trees).foreach {
          case EntryPlan.CompanionInject(moduleClsSym, mainDef) =>
            injections += moduleClsSym -> mainDef
          case EntryPlan.PackageObject(thicket) =>
            packageObjects += thicket
        }
      case _ =>
    }
    // Splice companion `main`s into the matching companion module classes.
    val withInjections =
      if (injections.isEmpty) trees
      else
        trees.map {
          case td @ TypeDef(_, template: Template) if injections.contains(td.symbol) =>
            val mainDef = injections(td.symbol)
            cpy.TypeDef(td)(rhs = cpy.Template(template)(body = template.body :+ mainDef))
          case other => other
        }
    // Package-level entry objects are always emitted at the enclosing package's
    // stat list. When this stat list belongs to a nested owner, buffer them until
    // the package's stat list is transformed; otherwise flush them here.
    packageObjects.foreach(pendingPackageObjects += _)
    if (ctx.owner.is(Package))
      val flushed = withInjections ++ pendingPackageObjects.view.flatMap(_.trees)
      pendingPackageObjects.clear()
      flushed
    else withInjections
  end transformStats

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    topAnnotSym = requiredClass("dfhdl.top")
    appTpe = requiredClassRef("dfhdl.app.DFApp")
    dfcTpe = requiredClassRef("dfhdl.core.DFC")
    designTpe = requiredClassRef("dfhdl.core.Design")
    resourceOwnerTpe = requiredClassRef("dfhdl.platforms.resources.ResourceOwner")
    portModTpe = requiredClassRef("dfhdl.core.Modifier.Port")
    // DFBool/DFBit exist only as case objects — the front-end type refers to the
    // singleton via `ir.DFBool.type`, whose classSymbol is the module class.
    irDFBoolClsSym = requiredModule("dfhdl.compiler.ir.DFBool").moduleClass.asClass
    irDFBitClsSym = requiredModule("dfhdl.compiler.ir.DFBit").moduleClass.asClass
    // DFString/DFDouble have BOTH a trait and a case object with the same name.
    // The front-end type refers to the trait (no `.type` suffix), so resolve the
    // class (trait) symbol here.
    irDFStringClsSym = requiredClass("dfhdl.compiler.ir.DFString")
    irDFDoubleClsSym = requiredClass("dfhdl.compiler.ir.DFDouble")
    irDFBitsClsSym = requiredClass("dfhdl.compiler.ir.DFBits")
    irDFDecimalClsSym = requiredClass("dfhdl.compiler.ir.DFDecimal")
    defaultsBoolSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.bool")
    defaultsBitSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.bit")
    defaultsInt32Sym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.int32")
    defaultsStringSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.string")
    defaultsDoubleSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.double")
    defaultsBitsSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.bits")
    defaultsUIntSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.uint")
    defaultsSIntSym = requiredMethod("dfhdl.core.r__For_Plugin.defaults.sint")
    emptyNoEODFCSym = requiredMethod("dfhdl.core.DFC.emptyNoEO")
    ctx
  end prepareForUnit
end TopAnnotPhase
