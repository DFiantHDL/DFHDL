package dfhdl.plugin

import dotty.tools.dotc.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import TypeApplications.*
import Constants.Constant
import Decorators.*
import printing.*
import printing.Texts.Text
// the printer builds its output out of string literals, which `PlainPrinter.stringToText`
// converts into limiter-aware `Text` nodes
import scala.language.implicitConversions
import scala.compiletime.uninitialized

/** The IR data types the printer can name, i.e. the `T` of a `DFType[T, Args]` frontend type.
  * Anything outside this set is left to the standard printer.
  */
enum DFTypeKind derives CanEqual:
  case DFBool, DFBit, DFBits, DFDecimal, DFEnum, DFVector, DFOpaque, DFStruct, DFPhysical,
    DFDouble, DFString, DFUnit

/** The DFHDL symbols [[DFHDLTypePrinter]] matches against.
  *
  * Symbol lookups walk the symbol table, and the printer is consulted for every type the compiler
  * prints, so they are resolved once and reused (see [[DFHDLSymbols.Cache]]).
  *
  * Every lookup is optional: a compilation that has no DFHDL on its classpath simply leaves
  * `available` false, and the printer then defers to the standard one for everything.
  */
final class DFHDLSymbols(using Context):
  private def irClass(name: String): Symbol = getClassIfDefined(s"dfhdl.compiler.ir.$name")
  // `DFBool`/`DFBit` are IR case objects, so the frontend type refers to them as `DFBool.type`
  // and the symbol behind that reference is the module class
  private def irModuleClass(name: String): Symbol =
    getModuleIfDefined(s"dfhdl.compiler.ir.$name").moduleClass

  val dfType: Symbol = getClassIfDefined("dfhdl.core.DFType")
  val dfVal: Symbol = getClassIfDefined("dfhdl.core.DFVal")
  val modifier: Symbol = getClassIfDefined("dfhdl.core.Modifier")
  val assignable: Symbol = getClassIfDefined("dfhdl.core.Modifier.Assignable")
  val isConst: Symbol = getClassIfDefined("dfhdl.core.ISCONST")
  val timeNumber: Symbol = irClass("TimeNumber")
  val freqNumber: Symbol = irClass("FreqNumber")
  val literalNumber: Symbol = irClass("LiteralNumber")

  private val kinds: Map[Symbol, DFTypeKind] =
    List(
      irModuleClass("DFBool") -> DFTypeKind.DFBool,
      irModuleClass("DFBit") -> DFTypeKind.DFBit,
      irClass("DFBits") -> DFTypeKind.DFBits,
      irClass("DFDecimal") -> DFTypeKind.DFDecimal,
      irClass("DFEnum") -> DFTypeKind.DFEnum,
      irClass("DFVector") -> DFTypeKind.DFVector,
      irClass("DFOpaque") -> DFTypeKind.DFOpaque,
      irClass("DFStruct") -> DFTypeKind.DFStruct,
      irClass("DFPhysical") -> DFTypeKind.DFPhysical,
      irClass("DFDouble") -> DFTypeKind.DFDouble,
      irClass("DFString") -> DFTypeKind.DFString,
      irClass("DFUnit") -> DFTypeKind.DFUnit
    ).collect { case (sym, kind) if sym.exists => sym -> kind }.toMap

  def kindOf(sym: Symbol): Option[DFTypeKind] = kinds.get(sym)

  /** False when this compilation has no DFHDL on its classpath, which disables the printer. */
  val available: Boolean = dfType.exists && dfVal.exists && modifier.exists
end DFHDLSymbols

object DFHDLSymbols:
  /** Resolves [[DFHDLSymbols]] once per compiler run.
    *
    * Symbols are only valid inside the run that created them, hence the run-id key. The cache is an
    * instance rather than a global: sbt compiles subprojects concurrently in one JVM, and those
    * compilers share nothing but their run ids, so a global cache would hand one compiler's symbols
    * to another.
    */
  final class Cache:
    private var cachedRunId: Int = -1
    private var cached: DFHDLSymbols = uninitialized
    def apply()(using Context): DFHDLSymbols =
      if (cachedRunId != ctx.runId)
        cachedRunId = ctx.runId
        cached = DFHDLSymbols()
      cached
  end Cache
end DFHDLSymbols

/** A `RefinedPrinter` that renders DFHDL's own types the way a DFHDL user writes them:
  * `Bits[8] <> VAL` rather than `DFVal[DFType[DFBits, Args1[8]], Modifier[...]]`.
  *
  * It is the compiler-side counterpart of `dfhdl.core.ShowType`, which renders the same types for
  * the error messages DFHDL itself produces. ShowType covers the messages DFHDL authors; this
  * printer covers everywhere else a type reaches the user on the compiler's own initiative (a type
  * mismatch, a missing member, an IDE hover).
  *
  * A type it cannot name is left to the standard printer. That is what keeps DFHDL's own internal
  * signatures readable: a generic `DFXInt[S, W, N]` stays as written instead of collapsing into
  * ShowType's catch-all `DFType`.
  *
  * Two deliberate departures from ShowType, whose unwrapping only makes sense inside a
  * DFHDL-authored message: a context function (`DFC ?=> T`) and an `Inlined[T]` print as themselves
  * here, since hiding them would misrepresent what the compiler is complaining about.
  */
class DFHDLTypePrinter(_ctx: Context, syms: DFHDLSymbols) extends RefinedPrinter(_ctx):
  import DFHDLTypePrinter.*
  import DFTypeKind.*

  override def toText(tp: Type): Text =
    // the recursion/size limiter has to be in effect while the text is BUILT, hence the
    // `optText` adapter rather than `controlled(...)` around an already-built text
    optText(t => controlled(t()))(dfhdlText(tp)).getOrElse(super.toText(tp))

  /** The DFHDL rendering of `tp`, if it is a DFHDL type this printer can name. */
  private def dfhdlText(tp: Type)(using Context): Option[Text] =
    if (!syms.available) None
    else
      tp match
        // a term reference prints as the reference itself; dealiasing it here would replace
        // the path the user wrote with the structure behind it
        case _: TermRef   => None
        case DFVal(text)  => Some(text)
        case DFType(text) => Some(text)
        case _            => None

  /** Matches a DFHDL value type, rendered as `<dataflow type> <> <modifier>`. Declines whenever the
    * dataflow type does, so an unnameable value type keeps its standard rendering as a whole rather
    * than half of it.
    */
  private object DFVal:
    def unapply(tp: Type)(using Context): Option[Text] =
      tp.dealias match
        case AppliedType(tycon, List(dfTpe, modTpe)) if tycon.typeSymbol == syms.dfVal =>
          infixText(ConnPrec, "<>")(DFType.unapply(dataTpe(dfTpe)))(modifierText(modTpe))
        case _ => None
  end DFVal

  /** Reduces a type in a position that holds a dataflow type (a value's type, a vector's cell type)
    * to the structure that names it. Unlike a value type, which is named after the reference the
    * user wrote, a dataflow type is named after its structure, so this position takes the structure
    * however it is reached: through a reference to the value that defines it (`Bit` is a `def` in
    * the `dfhdl` package, so it arrives as a term reference to an expression type), through an
    * unreduced `DFType.Of[T]` match type (what the `X` vector operator leaves in its cell), and
    * through the `DFBits[8] & DFTypeAny` shape an `Exact`-driven inference leaves behind, whose
    * first component carries the actual type.
    */
  private def dataTpe(tp: Type)(using Context): Type =
    tp.widenTermRefExpr.dealias.normalized match
      case AndType(tp1, _) => dataTpe(tp1)
      case tp              => tp

  /** Matches a DFHDL dataflow type (`DFType[irType, args]`), rendered under its user-facing name.
    * The frontend type is decomposed ONCE here and the IR type it wraps then selects the rendering,
    * so adding a type is a case in [[dfTypeText]] rather than another extractor re-matching the
    * same shape.
    */
  private object DFType:
    def unapply(tp: Type)(using Context): Option[Text] =
      tp.dealias match
        case AppliedType(tycon, List(irTpe, argsTpe)) if tycon.typeSymbol == syms.dfType =>
          syms.kindOf(irTpe.typeSymbol).flatMap(dfTypeText(_, irTpe, argsTpe.dealias.argInfos))
        case _ => None

  private def dfTypeText(kind: DFTypeKind, irTpe: Type, args: List[Type])(using
      Context
  ): Option[Text] =
    (kind, args) match
      case (DFBool, _)                  => Some("Boolean")
      case (DFBit, _)                   => Some("Bit")
      case (DFBits, IntP(width) :: Nil) => Some("Bits[" ~ width ~ "]")
      case (DFDecimal, sign :: IntP(magnitude) :: fraction :: native :: Nil) =>
        decimalText(sign, magnitude, fraction, native)
      case (DFEnum, encoding :: Nil)       => Some(toText(encoding))
      case (DFOpaque, frontend :: Nil)     => Some(toText(frontend))
      case (DFStruct, fields :: Nil)       => Some(structText(fields))
      case (DFVector, cell :: dims :: Nil) =>
        infixText(VecPrec, "X")(Some(toText(dataTpe(cell))))(dimsText(dims))
      case (DFPhysical, _) => physicalText(irTpe)
      case (DFDouble, _)   => Some("Double")
      case (DFString, _)   => Some("String")
      case (DFUnit, _)     => Some("Unit")
      case _               => None

  /** A decimal is named by its signedness and fraction width: the zero-fraction cases are the
    * integer types, the rest the fixed-point ones. A native decimal is the wildcard `Int`, whose
    * width its value determines. Declines when either is not statically known, leaving generic
    * signatures such as `DFXInt[S, W, N]` as written.
    */
  private def decimalText(sign: Type, magnitude: Text, fraction: Type, native: Type)(using
      Context
  ): Option[Text] =
    constBoolean(native) match
      case Some(true) => Some("Int")
      case _          =>
        (constBoolean(sign), constInt(fraction)) match
          case (Some(false), Some(0)) => Some("UInt[" ~ magnitude ~ "]")
          case (Some(true), Some(0))  => Some("SInt[" ~ magnitude ~ "]")
          case (Some(false), Some(_)) =>
            Some("UFix[" ~ magnitude ~ ", " ~ intPText(fraction) ~ "]")
          case (Some(true), Some(_)) =>
            Some("SFix[" ~ magnitude ~ ", " ~ intPText(fraction) ~ "]")
          case _ => None

  /** A struct over a tuple prints as that tuple of field types; a `DFStruct.Fields` case class (or
    * a named tuple) prints as itself.
    */
  private def structText(fields: Type)(using Context): Text =
    fields.tupleElementTypes match
      case Some(elems @ (_ :: _)) => "(" ~ argsText(elems) ~ ")"
      case _                      => toText(fields)

  /** A vector's dimensions are a tuple of lengths: one-dimensional vectors print the single length
    * bare, multi-dimensional ones print the whole tuple.
    */
  private def dimsText(dims: Type)(using Context): Text =
    dims.tupleElementTypes match
      case Some(dim :: Nil)    => intPText(dim)
      case Some(ds @ (_ :: _)) => "(" ~ Text(ds.map(intPText), ", ") ~ ")"
      case _                   => intPText(dims)

  private def physicalText(irTpe: Type)(using Context): Option[Text] =
    irTpe.argInfos match
      case unit :: Nil =>
        val sym = unit.typeSymbol
        if (sym == syms.timeNumber) Some("Time")
        else if (sym == syms.freqNumber) Some("Freq")
        else if (sym == syms.literalNumber) Some("Number")
        else None
      case _ => None

  private def modifierText(tp: Type)(using Context): Text =
    tp.dealias match
      case AppliedType(tycon, List(access, _, _, param)) if tycon.typeSymbol == syms.modifier =>
        // the same three names `ShowType` reports, and for the same reasons: a constant value
        // is a `CONST`, an assignable one a `VAR`, and anything else a plain readable `VAL`
        if (isConstParam(param)) "CONST"
        else if (access.derivesFrom(syms.assignable)) "VAR"
        else "VAL"
      case _ => "VAL"

  private def isConstParam(tp: Type)(using Context): Boolean =
    tp.dealias match
      case AppliedType(tycon, List(arg)) if tycon.typeSymbol == syms.isConst =>
        constBoolean(arg).contains(true)
      case _ => false

  /** Matches the `IntP` parameter of a DFHDL type (a width, a fixed-point fraction width, a vector
    * length). A literal prints as its number and a reference to a parameter as that parameter's
    * name; every other form (an `IntP.Sig` arithmetic signature, an inference variable, a bare
    * `Int`) prints as an unbounded `Int`, since none of them names a value.
    */
  private object IntP:
    def unapply(tp: Type)(using Context): Some[Text] = Some(intPText(tp))

  private def intPText(tp: Type)(using Context): Text =
    tp.stripTypeVar.dealias match
      case ConstantType(Constant(width: Int))                    => width.toString
      case tp: TermRef if tp.termSymbol.exists                   => tp.termSymbol.name.toString
      case tp: TypeRef if tp.symbol.exists && !tp.symbol.isClass => tp.symbol.name.toString
      case tp: TypeParamRef                                      => tp.paramName.toString
      case _                                                     => "Int"

  private def constBoolean(tp: Type)(using Context): Option[Boolean] =
    tp.dealias match
      case ConstantType(Constant(value: Boolean)) => Some(value)
      case _                                      => None

  private def constInt(tp: Type)(using Context): Option[Int] =
    tp.dealias match
      case ConstantType(Constant(value: Int)) => Some(value)
      case _                                  => None

  /** Renders `lhs <op> rhs` with the precedence handling `RefinedPrinter.toTextInfixType` gives
    * Scala's own infix types, so a DFHDL type nested in a tighter-binding position gets its
    * parentheses (`(Bit X 4) <> VAL`, since `X` binds looser than `<>`). Both DFHDL operators
    * printed here are left-associative, so the left operand keeps the operator's own precedence and
    * the right operand binds one level tighter. Declines when the left operand does.
    */
  private def infixText(prec: Precedence, op: String)(lhs: => Option[Text])(rhs: => Text)(using
      Context
  ): Option[Text] =
    optText(t => changePrec(prec)(t())):
      optText(t => atPrec(prec)(t()))(lhs).map(_ ~ " " ~ op ~ " " ~ atPrec(prec + 1)(rhs))

  /** Applies one of the printer's `Text` wrappers (`controlled`, `atPrec`, `changePrec`) to a
    * builder that may decline to produce anything. The wrapper has to be in effect while the text
    * is BUILT: an already-built `Text` can no longer be parenthesized, and a limiter entered after
    * the fact has counted nothing. `op` is evaluated at most once.
    */
  private def optText(wrap: (() => Text) => Text)(op: => Option[Text]): Option[Text] =
    var res: Option[Text] = None
    val wrapped = wrap { () =>
      res = op
      res.getOrElse(Str(""))
    }
    res.map(_ => wrapped)
end DFHDLTypePrinter

object DFHDLTypePrinter:
  // Scala's own operator precedences, the same ones `RefinedPrinter` computes for infix types:
  // `<>` is a comparison-class operator and `X` an alphanumeric one.
  private val ConnPrec: Precedence = parsing.precedence("<>".toTypeName)
  private val VecPrec: Precedence = parsing.precedence("X".toTypeName)
end DFHDLTypePrinter
