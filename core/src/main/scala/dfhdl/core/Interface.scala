package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import ir.DFDesignBlock.InstMode
import scala.annotation.Annotation
import Interface.ViewBuilder
import NamedTuple.AnyNamedTuple
import scala.quoted.*

// A basic interface declaration: structurally an "empty design" that carries
// ports and views but no behavioral statements (no processes/connections/
// assignments). It elaborates to a `DFDesignBlock(InstMode.Interface)` and runs
// under `DFC.Scope.Interface`, which is used to reject illegal constructs inside
// an interface body. The lifecycle mirrors `Design` but drops the top-level /
// device-top / resource and top-check machinery (never applies to an interface).
//
// An interface is purely structural, so it has no behavioral (DF/RT/ED) domain
// semantics of its own. There is a single, domain-neutral `Interface` based on
// the ED domain under the hood: ED is terminal in the lowering pipeline
// (DF -> RT -> ED), so an ED interface is never transformed by ToRT/ToED and
// never has clk/rst injected, and the same `Interface` is reusable inside a
// design of any domain.
abstract class Interface
    extends DomainContainer(DomainType.ED), HasClsMetaArgs, HasConstParams:
  self =>
  private[core] type TScope = DFC.Scope.Interface
  private[core] type TOwner = Design.Block
  final protected given TScope = DFC.Scope.Interface
  private[core] def mkInstMode: InstMode = InstMode.Interface
  private[dfhdl] def initOwner: TOwner =
    // Build the interface block directly from the `__clsMetaArgs` chain (leaf
    // names the interface).
    val blockDFC = __clsMetaArgs.headOption match
      case Some(a) =>
        dfc.setMeta(r__For_Plugin.metaGen(Some(a.name), a.position, a.docOpt, a.annotations))
      case None => dfc.anonymize
    Design.Block(__domainType, mkInstMode)(using blockDFC)
  private var hasStartedLate: Boolean = false
  final override def onCreateStartLate: Unit =
    hasStartedLate = true
    val paramEntries = Design.Inst.collectParamEntries
    val endedInterface = containedOwner.asIR
    // TODO: check interface has at least one port or view, otherwise error
    dfc.exitOwner()
    Design.Inst(endedInterface, paramEntries)
    dfc.enterLate()
  final override def onCreateEnd(thisOwner: Option[This]): Unit =
    if (hasStartedLate) dfc.exitLate()
    else dfc.exitOwner()
  protected object view:
    transparent inline def apply(inline args: DFDclAny*)(using
        DFC
    ): ViewBuilder[? <: Interface, ? <: AnyNamedTuple] = ???
    // Entry points are macros (not delegations to the `ViewBuilder` extensions)
    // so the port names are read from the literal `p1, p2` references at the
    // call site rather than from a forwarded `Seq`. The enclosing interface type
    // (e.g. `MyIfc`) is discovered by the macro; we deliberately do NOT use
    // `self.type` here (the `self =>` alias leaks into member types and crashes
    // a later transform phase), and the interface class type is the right choice
    // anyway so views of distinct instances stay type-compatible.
    transparent inline def in(inline args: DFDclAny*)(using
        DFC
    ): ViewBuilder[? <: Interface, ? <: AnyNamedTuple] =
      ${ ViewBuilder.viewEntryMacro('args, true) }
    transparent inline def out(inline args: DFDclAny*)(using
        DFC
    ): ViewBuilder[? <: Interface, ? <: AnyNamedTuple] =
      ${ ViewBuilder.viewEntryMacro('args, false) }
  end view
  transparent inline def VIEW(using DFC): DFValOf[DFView[self.type, AnyNamedTuple]] = ???
end Interface

object Interface:
  sealed class ViewBuilder[I <: Interface, F <: AnyNamedTuple]:
    type VIEW = DFValOf[DFView[I, F]]
    def VIEW(using DFC): DFValOf[DFView[I, F]] = ???

  object ViewBuilder:
    extension [I <: Interface, F <: AnyNamedTuple](vb: ViewBuilder[I, F])
      transparent inline def apply(inline args: DFDclAny*)(using
          DFC
      ): ViewBuilder[I, ? <: AnyNamedTuple] =
        ???
      transparent inline def in(inline args: DFDclAny*)(using
          DFC
      ): ViewBuilder[I, ? <: AnyNamedTuple] =
        ${ addPortsMacro[I, F]('args, true) }
      transparent inline def out(inline args: DFDclAny*)(using
          DFC
      ): ViewBuilder[I, ? <: AnyNamedTuple] =
        ${ addPortsMacro[I, F]('args, false) }
    end extension

    // Chaining: the interface type `I` and current field tuple `F` come from the
    // builder's type parameters.
    def addPortsMacro[I <: Interface: Type, F <: AnyNamedTuple: Type](
        args: Expr[Seq[DFDclAny]],
        isIn: Boolean
    )(using Quotes): Expr[ViewBuilder[I, ? <: AnyNamedTuple]] =
      import quotes.reflect.*
      buildViewBuilder(TypeRepr.of[I], TypeRepr.of[F], args, isIn)
        .asExprOf[ViewBuilder[I, ? <: AnyNamedTuple]]

    // Entry: the field tuple starts empty and the interface type is the enclosing
    // interface class at the call site.
    def viewEntryMacro(
        args: Expr[Seq[DFDclAny]],
        isIn: Boolean
    )(using Quotes): Expr[ViewBuilder[? <: Interface, ? <: AnyNamedTuple]] =
      import quotes.reflect.*
      buildViewBuilder(enclosingInterfaceTpe, TypeRepr.of[NamedTuple.Empty], args, isIn)

    private def enclosingInterfaceTpe(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      def loop(s: Symbol): TypeRepr =
        if (!s.exists)
          report.errorAndAbort("`view` can only be used inside an Interface.")
        else if (s.isClassDef && s.typeRef <:< TypeRepr.of[Interface]) s.typeRef
        else loop(s.owner)
      loop(Symbol.spliceOwner)

    // Appends the given ports to the field named-tuple, each tagged with the
    // IN/OUT direction, and returns a (currently empty) ViewBuilder carrying the
    // extended named-tuple type. The runtime value holds no fields yet — only the
    // type is computed here.
    private def buildViewBuilder(using
        Quotes
    )(
        iTpe: quotes.reflect.TypeRepr,
        fTpe: quotes.reflect.TypeRepr,
        args: Expr[Seq[DFDclAny]],
        isIn: Boolean
    ): Expr[ViewBuilder[? <: Interface, ? <: AnyNamedTuple]] =
      import quotes.reflect.*
      val argExprs: Seq[Expr[DFDclAny]] = args match
        case Varargs(es) => es
        case _           =>
          report.errorAndAbort("`view` ports must be passed as explicit arguments.")
      // extract the (port name, port type) of each argument from the reference
      def portInfo(expr: Expr[DFDclAny]): (String, TypeRepr) =
        def loop(t: Term): Term = t match
          case Inlined(_, _, inner) => loop(inner)
          case Typed(inner, _)      => loop(inner)
          case Block(_, inner)      => loop(inner)
          case TypeApply(inner, _)  => loop(inner)
          case _                    => t
        val core = loop(expr.asTerm)
        if (!core.symbol.exists)
          report.errorAndAbort(
            "`view` arguments must be references to interface ports.",
            core.pos
          )
        (core.symbol.name, core.tpe.widen)
      val infos = argExprs.map(portInfo).toList
      val newNameTpes: List[TypeRepr] =
        infos.map((n, _) => ConstantType(StringConstant(n)))
      val newValTpes: List[TypeRepr] = infos.map { (n, portTpe) =>
        portTpe.asType match
          case '[DFVal[t, m]] =>
            if (isIn) TypeRepr.of[DFVal[t, Modifier.IN.type]]
            else TypeRepr.of[DFVal[t, Modifier.OUT.type]]
          case _ =>
            report.errorAndAbort(s"`$n` is not a DFHDL port value.")
      }
      // the builder's existing field names/value types
      val (existingNameTpes, existingValTpes) = fTpe.asType match
        case '[type f <: AnyNamedTuple; f] =>
          (
            TypeRepr.of[NamedTuple.Names[f]].getTupleArgs,
            TypeRepr.of[NamedTuple.DropNames[f]].getTupleArgs
          )
        case _ => report.errorAndAbort("internal error: expected a named tuple")
      // build a tuple type from a list of element type reprs
      def mkTupleType(elems: List[TypeRepr]): TypeRepr =
        elems.foldRight(TypeRepr.of[EmptyTuple]) { (h, acc) =>
          (h.asType, acc.asType) match
            case ('[h], '[type t <: Tuple; t]) => TypeRepr.of[h *: t]
            case _ => report.errorAndAbort("internal error: view field tuple build")
        }
      val namesTpe = mkTupleType(existingNameTpes ++ newNameTpes)
      val valsTpe = mkTupleType(existingValTpes ++ newValTpes)
      (iTpe.asType, namesTpe.asType, valsTpe.asType) match
        case ('[type i <: Interface; i], '[type n <: Tuple; n], '[type v <: Tuple; v]) =>
          '{ new ViewBuilder[i, NamedTuple.NamedTuple[n, v]]() }
        case _ => report.errorAndAbort("internal error: view type build")
    end buildViewBuilder
  end ViewBuilder
end Interface
