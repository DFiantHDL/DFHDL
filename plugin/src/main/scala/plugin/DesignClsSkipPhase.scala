package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Decorators.*
import ast.tpd
import Names.*
import Types.*
import Constants.Constant

import scala.language.implicitConversions
import scala.compiletime.uninitialized

/** ~~~ the class-design body-skip rigging ~~~
  *
  * A design def's body is a thunk the design load gate can simply not call, its public interface
  * being created by the harness (`r__For_Plugin.designFromDef`) outside it. A design class's body
  * IS its constructor, and it declares the class's public interface itself, so this phase gives it
  * the same shape: the interface declarations (ports, constants, interfaces) stay unguarded, every
  * other body statement is guarded by the gate's decision, and the gate itself is called at the
  * body's head (right after the plugin-generated design parameters, which its key needs).
  *
  * On a gate hit the guarded statements do not run: the shell design ends up holding exactly the
  * public interface, binds the instantiation site's ports and applied parameters as it always did,
  * and drops out of the final assembly as a duplicate of the loaded design (this run's canonical,
  * or a design adopted from the sub-design cache).
  *
  * A class is guarded only when skipping it can reproduce that interface faithfully:
  *   - it captures no DFHDL value from an enclosing design (such a capture materializes an
  *     auto-created design parameter INSIDE the body, which a skipped body would not create);
  *   - no unguarded declaration depends on a guarded one (a port whose width comes out of the
  *     skipped part of the body cannot be re-created).
  *
  * The phase runs LAST, on the final trees: the guards it introduces are plain Scala `if`s that no
  * other phase should reinterpret (`CustomControl` in particular turns DFHDL-conditioned `if`s into
  * DFHDL conditionals), and the statements it guards are the fully transformed ones.
  */
class DesignClsSkipPhase(setting: Setting) extends CommonPhase:
  import tpd.*

  val phaseName = "DesignClsSkip"

  // LAST of the DFHDL phases: `MetaContextDelegate` (which follows `MetaContextGen`) and
  // `OnCreateEvents` are the two ends of the phase plan, and both must precede the guards. A
  // meta-context pass in particular reads a value declaration's rhs to name the value it creates,
  // and would not look through a guard.
  override val runsAfter = Set("OnCreateEvents", "MetaContextDelegate")
  override val runsBefore = Set(transform.FirstTransform.name)

  var designTpe: TypeRef = uninitialized
  var interfaceTpe: TypeRef = uninitialized
  var portModTpe: TypeRef = uninitialized
  var dfhdlPkgCls: Symbol = uninitialized
  var clsParamSym: TermSymbol = uninitialized
  var clsParamTpe: TypeRef = uninitialized

  // a DFHDL port value type: DFVal[_, Modifier[Port, _, _, _]]
  private def isPortTpe(tpe: Type)(using Context): Boolean =
    tpe.widenDealias match
      case AppliedType(tycon, _ :: modTpe :: Nil) if tycon.typeSymbol == dfValSym =>
        modTpe.dealias match
          case AppliedType(_, portTpe :: _) => portTpe <:< portModTpe
          case _                            => false
      case _ => false

  // A DFHDL entity: a container (a design, an interface, a domain — anything carrying a DFC and
  // therefore elaborating INTO the design), a DFHDL value, or any type the DFHDL library declares.
  // The container test is a subtyping one and not a "declared in the dfhdl package" one, since the
  // designs that matter most here are the USER's own classes.
  private def isDFHDLPart(t: Type)(using Context): Boolean =
    val sym = t.typeSymbol
    (sym.exists && sym.isContainedIn(dfhdlPkgCls)) ||
    (t.isValueType && (t <:< hasDFCTpe || t.dealias.typeSymbol == dfValSym))

  // Does any part of the type name a DFHDL entity? A value of such a type is created by DFHDL code,
  // which (unlike plain Scala code) may plant members in the design, so it is only ever re-created
  // on a skip when it is part of the design's public interface.
  private def isDFHDLTpe(tpe: Type)(using Context): Boolean =
    val acc = new TypeAccumulator[Boolean]:
      def apply(found: Boolean, t: Type): Boolean =
        if (found) true
        else if (isDFHDLPart(t)) true
        else foldOver(false, t)
    acc(false, tpe.widenDealias)

  // The class's public interface: what an instantiation site can reach through the instance, and
  // therefore what a skipped body must still create. Definitions (types, methods) declare rather
  // than execute, so they are trivially unguarded.
  private def isInterfaceStat(stat: Tree)(using Context): Boolean = stat match
    case _: TypeDef | _: DefDef => true
    case vd: ValDef             =>
      val tpe = vd.tpt.tpe
      vd.rhs.isEmpty || isPortTpe(tpe) || tpe.isDFConst || tpe <:< interfaceTpe ||
      // a plain Scala value (e.g. a port width computed from the parameters): it plants no
      // member of its own and the interface declarations may well depend on it. A Unit value
      // is not one of those: it is a statement in disguise.
      (!isDFHDLTpe(tpe) && !tpe.widenDealias.isRef(defn.UnitClass))
    case _ => false

  // The design parameter declaration the plugin generates from a class's `<> CONST` constructor
  // parameter (`MetaContextPlacerPhase`): `genContainerParam[T](applied, default, meta)(dfc)`. Its
  // three value arguments are what this phase lifts into the gate call, so the harness (and not the
  // body) creates the design's parameter members.
  private object ParamGen:
    private def strip(t: Tree)(using Context): Tree = t match
      case Block(_, expr)     => strip(expr)
      case Inlined(_, _, exp) => strip(exp)
      case Typed(expr, _)     => strip(expr)
      case t                  => t
    def unapply(vd: ValDef)(using Context): Option[(Tree, Tree, Tree)] =
      def argsOf(t: Tree, argss: List[List[Tree]]): (Symbol, List[List[Tree]]) = strip(t) match
        case Apply(fun, args)  => argsOf(fun, args :: argss)
        case TypeApply(fun, _) => argsOf(fun, argss)
        case t                 => (t.symbol, argss)
      val (sym, argss) = argsOf(vd.rhs, Nil)
      if (sym == genContainerParamSym)
        argss.headOption.collect { case applied :: default :: meta :: Nil =>
          (applied, default, meta)
        }
      else None
  end ParamGen

  private def isParamGen(vd: ValDef)(using Context): Boolean = ParamGen.unapply(vd).isDefined

  // the class prologue: everything preceding the body proper (the constructor parameter
  // accessors, the plugin-injected overrides and design parameters, and any leading definition)
  private def isPrologueStat(stat: Tree)(using Context): Boolean = stat match
    case _: TypeDef | _: DefDef => true
    case vd: ValDef             => vd.rhs.isEmpty || isParamGen(vd)
    case _                      => false

  private def zeroOf(tpe: Type)(using Context): Tree =
    val w = tpe.widenDealias
    if (w.isRef(defn.UnitClass)) unitLiteral
    else if (w.isRef(defn.BooleanClass)) Literal(Constant(false))
    else if (w.isRef(defn.IntClass)) Literal(Constant(0))
    else if (w.isRef(defn.LongClass)) Literal(Constant(0L))
    else if (w.isRef(defn.DoubleClass)) Literal(Constant(0.0))
    else if (w.isRef(defn.FloatClass)) Literal(Constant(0.0f))
    else if (w.isRef(defn.ShortClass)) Literal(Constant(0.toShort))
    else if (w.isRef(defn.ByteClass)) Literal(Constant(0.toByte))
    else if (w.isRef(defn.CharClass)) Literal(Constant(0.toChar))
    else Literal(Constant(null)).cast(tpe)

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    tree.rhs match
      case template: Template if isGuardableCls(tree) =>
        val clsSym = tree.tpe.classSymbol.asClass
        val (prologue, body) = template.body.span(isPrologueStat)
        val paramGens = prologue.collect { case vd: ValDef if isParamGen(vd) => vd }
        val (interfaceStats, guardedStats) = body.partition(isInterfaceStat)
        val hasBody = guardedStats.nonEmpty
        val skippable =
          hasBody && isSkippable(clsSym, template, interfaceStats, guardedStats)
        // nothing to lift and nothing to skip: the class stays as it is
        if (paramGens.isEmpty && !hasBody) tree
        else
          val skipTree = This(clsSym).select(clsSym.requiredMethod("__clsSkipBody".toTermName))
          def guard(stat: Tree): Tree = stat match
            case vd: ValDef =>
              cpy.ValDef(vd)(rhs = If(skipTree, zeroOf(vd.tpt.tpe), vd.rhs))
            case stat =>
              If(skipTree, unitLiteral, Block(stat :: Nil, unitLiteral))
          // the gate creates the design's parameters from the lifted applied values, then decides
          val paramEntries = paramGens.flatMap(vd =>
            ParamGen.unapply(vd).map((applied, default, meta) =>
              ref(clsParamSym).appliedTo(applied, default, meta)
            )
          )
          val gateTree = This(clsSym)
            .select(clsSym.requiredMethod("__clsBodyGate".toTermName))
            .appliedTo(
              clsOf(clsSym.typeRef),
              mkList(paramEntries, Some(clsParamTpe)),
              Literal(Constant(skippable)),
              Literal(Constant(hasBody))
            )
          // ...and the body's parameter declarations fetch them back from it
          val getParamSym = clsSym.requiredMethod("__clsGetParam".toTermName)
          val paramFetches = paramGens.zipWithIndex.map { (vd, idx) =>
            cpy.ValDef(vd)(rhs =
              This(clsSym)
                .select(getParamSym)
                .appliedToType(vd.tpt.tpe)
                .appliedTo(clsOf(clsSym.typeRef), Literal(Constant(idx)))
            )
          }
          val newBody =
            prologue.filterNot(paramGens.contains) ++ (gateTree :: paramFetches) ++
              body.map(stat =>
                if (!skippable || isInterfaceStat(stat)) stat else guard(stat)
              )
          cpy.TypeDef(tree)(rhs = cpy.Template(template)(body = newBody))
        end if
      case _ => tree
    end match
  end transformTypeDef

  // a design class whose body the gate could skip: an abstract class or a trait is never
  // instantiated on its own (its body runs as a base template, where the gate stands down), and
  // an anonymous class carries no design body of its own
  private def isGuardableCls(tree: TypeDef)(using Context): Boolean =
    val clsSym = tree.symbol
    tree.tpe <:< designTpe && clsSym.isClass && !clsSym.isAnonymousClass &&
    !clsSym.is(Trait) && !clsSym.is(Abstract)

  private def isSkippable(
      clsSym: ClassSymbol,
      template: Template,
      interfaceStats: List[Tree],
      guardedStats: List[Tree]
  )(using Context): Boolean =
    val captures = discoverClsCaptures(clsSym, template)
    // A captured DFHDL value of an enclosing design materializes as an auto-created design
    // parameter of THIS design (`cloneUnreachable`), created by the body reference itself: a
    // skipped body would leave the design without it, and the instantiation site without the
    // applied value to bind to it. (Captured plain Scala values are safe: they join the gate's
    // key through `__clsScalaArgs`.)
    if (captures.phantomConsts.nonEmpty || captures.phantomVals.nonEmpty) false
    else
      // an unguarded declaration must not read a guarded one, directly or through a method of
      // this class: on a skip it would read the guarded value's zero
      val guardedSyms = guardedStats.collect { case vd: ValDef => vd.symbol }.toSet
      def readsGuarded(tree: Tree): Boolean =
        tree.existsSubTree(t => t.symbol.exists && guardedSyms.contains(t.symbol))
      val guardedMethods = template.body.collect {
        case dd: DefDef if !dd.rhs.isEmpty && readsGuarded(dd.rhs) => dd.symbol
      }.toSet
      def unsafe(tree: Tree): Boolean =
        tree.existsSubTree(t =>
          t.symbol.exists && (guardedSyms.contains(t.symbol) || guardedMethods.contains(t.symbol))
        )
      !interfaceStats.exists {
        case vd: ValDef => unsafe(vd.rhs)
        case _          => false
      }
    end if
  end isSkippable

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    designTpe = requiredClassRef("dfhdl.core.Design")
    interfaceTpe = requiredClassRef("dfhdl.core.Interface")
    portModTpe = requiredClassRef("dfhdl.core.Modifier.Port")
    dfhdlPkgCls = requiredPackage("dfhdl").moduleClass
    clsParamSym = requiredMethod("dfhdl.core.r__For_Plugin.clsParam")
    clsParamTpe = requiredClassRef("dfhdl.core.r__For_Plugin.ClsParam")
    ctx
end DesignClsSkipPhase
