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
  This phase creates a `top_<design_name>` object with a `DFApp` main entry point
  for each design that has a `@top` annotation. The design parameters must have
  default arguments and have the supported types. These arguments can then be
  overridden by the command-line options that `DFApp` application provides.
 */
class TopAnnotPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "TopAnnot"

  override val runsAfter = Set("typer")
  override val runsBefore = Set("MetaContextGen")
  // override val debugFilter: String => Boolean = _.contains("Playground.scala")
  var topAnnotSym: ClassSymbol = uninitialized
  var appTpe: TypeRef = uninitialized
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

  private def getPackageOwner(using Context): Symbol =
    var owner = ctx.owner
    while (!owner.is(Package))
      owner = owner.owner
    owner
  end getPackageOwner

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

  object TopMainThicket:
    def unapply(explored: List[Tree])(using Context): Option[(TypeDef, Thicket, List[Tree])] =
      val origOwner = ctx.owner
      val packageOwner = getPackageOwner
      explored match
        case (td @ TypeDef(tn, template: Template)) :: rest
            if td.symbol.isClass && !td.symbol.is(Trait) &&
              td.symbol.hasAnnotation(topAnnotSym) =>
          // all thickets will be added at a package level
          inContext(ctx.withOwner(packageOwner)):
            val clsSym = td.symbol.asClass
            // has top annotation and no companion object
            clsSym.getAnnotation(topAnnotSym).map(a => dropProxies(a.tree)) match
              case Some(topAnnotTree @ Apply(Apply(Apply(_, topAnnotOptionsTrees), _), _)) =>
                // Distinguish between `@top` (default genMain) and an explicitly-given
                // `@top(true)` / `@top(false)`. An explicit literal/named-arg indicates
                // the user opted into the lenient variant; an implicit default means
                // the strict form was used.
                val (genMain, genMainIsExplicit) = topAnnotOptionsTrees match
                  case Literal(Constant(b: Boolean)) :: _ => (b, true)
                  case NamedArg(n, Literal(Constant(b: Boolean))) :: _
                      if n.toString == "genMain" =>
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
                else
                  if (template.constr.paramss.length > 1)
                    report.error(
                      "Unsupported multiple parameter blocks for top-level design.",
                      template.constr.srcPos
                    )
                    None
                  else
                    var topName = tn.toString
                    var owner = origOwner
                    while (!owner.is(Package) && owner.is(ModuleClass))
                      topName = s"${owner.name.toString.replace("$", "")}_$topName"
                      owner = owner.owner
                    if (!owner.is(Package))
                      report.error(
                        "Top-level main generation must be defined only in nested objects or packages.",
                        topAnnotTree.srcPos
                      )
                      None
                    else
                      topName = s"top_${topName}"
                      // Anchor the synthetic entry point on the design class's
                      // name. This is what tooling (e.g. Metals `run`/`debug`
                      // code lenses) uses to position itself on the class that
                      // introduces the entry point rather than at the top of the
                      // file. Using the class name (instead of the `@top`
                      // annotation) also keeps this working if/when the design no
                      // longer needs an explicit `@top` annotation.
                      val designNameSpan = td.nameSpan
                      // the top entry point module symbol
                      val dfApp = newCompleteModuleSymbol(
                        packageOwner,
                        topName.toTermName,
                        Touched,
                        Touched | NoInits,
                        List(defn.ObjectType, appTpe),
                        Scopes.newScope,
                        coord = designNameSpan,
                        compUnitInfo = clsSym.compUnitInfo
                      )
                      val moduleCls = dfApp.moduleClass.asClass
                      val designNameTree = Literal(Constant(tn.toString))
                      val topScalaPathTree = Literal(Constant(dfApp.fullName.toString()))
                      val paramVDs =
                        template.constr.paramss.flatten.collect { case vd: ValDef => vd }
                      val dsnArgNames =
                        mkList(paramVDs.map(vd => Literal(Constant(vd.name.toString))))
                      val defaultMap = mutable.Map.empty[Int, Tree]
                      rest match
                        case (module: ValDef) :: (compSym @ TypeDef(_, compTemplate: Template)) :: _
                            if compSym.symbol.companionClass == clsSym =>
                          compTemplate.body.foreach {
                            case dd @ DefDef(name = NameKinds.DefaultGetterName(n, i)) =>
                              defaultMap += i -> ref(module.symbol).select(dd.symbol)
                            case _ =>
                          }
                        case _ =>
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
                        mkList(paramVDs.map(vd =>
                          Literal(Constant(vd.symbol.docString.getOrElse("")))
                        ))
                      val Werror = Literal(Constant(ctx.settings.Werror.value))
                      val hasResourceOwnerTree =
                        Literal(Constant(clsSym.hasNestedMemberCond(_ <:< resourceOwnerTpe)))
                      def portCond(tpe: Type): Boolean =
                        if (tpe.typeSymbol == dfValSym)
                          tpe match
                            // DFVal[_, Modifier[Port, _, _, _]]
                            case AppliedType(_, _ :: AppliedType(_, a :: _) :: Nil) =>
                              a <:< portModTpe
                            case _ => false
                        else false
                      val hasPorts = Literal(Constant(clsSym.hasNestedMemberCond(portCond)))
                      val setInitials =
                        This(moduleCls).select("setInitials".toTermName).appliedToArgs(
                          List(
                            designNameTree, topScalaPathTree, topAnnotTree, dsnArgNames,
                            dsnArgValues,
                            dsnArgDescs, Werror, hasResourceOwnerTree, hasPorts
                          )
                        )
                      val dsnInstArgs = paramVDs.map(vd =>
                        This(moduleCls).select("getDsnArg".toTermName).appliedTo(
                          Literal(Constant(vd.name.toString))
                        )
                      )
                      val dsnInst = New(clsSym.typeRef, dsnInstArgs)
                      val setDsn = This(moduleCls).select("setDsn".toTermName).appliedTo(dsnInst)
                      // Give the generated module definition a real source span
                      // (the design class's name) so that `ExtractSemanticDB`
                      // records a definition *occurrence* for it. Without a
                      // positioned tree the synthetic entry point only appears in
                      // the SemanticDB `Symbols` section (no `Occurrences`), which
                      // forces Metals to fall back to placing the run/debug code
                      // lenses at the very top of the file instead of above the
                      // design class.
                      val moduleDef = ModuleDef(dfApp, List(setInitials, setDsn))
                      val positionedModuleDef =
                        Thicket(moduleDef.trees.map(_.withSpan(designNameSpan)))
                      Some(td, positionedModuleDef, rest)
                    end if
                  end if
                end if
              case _ => None
            end match
        case _ => None
      end match
    end unapply
  end TopMainThicket

  val thickets = mutable.ListBuffer.empty[Thicket]

  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    val retTrees = mutable.ListBuffer.empty[Tree]
    var explored: List[Tree] = trees
    while (explored.nonEmpty)
      explored match
        case TopMainThicket(td, thicket, rest) =>
          // all thickets are added at a package level
          if (ctx.owner.is(Package))
            retTrees ++= td :: thicket.trees
          else
            thickets += thicket
            retTrees += td
          explored = rest
        case _ =>
          retTrees += explored.head
          explored = explored.drop(1)
      end match
    end while
    if (ctx.owner.is(Package))
      retTrees ++= thickets.view.flatMap(_.trees)
      thickets.clear()
    end if
    retTrees.toList
  end transformStats

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    topAnnotSym = requiredClass("dfhdl.top")
    appTpe = requiredClassRef("dfhdl.app.DFApp")
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
end TopAnnotPhase
