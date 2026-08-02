package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import compiletime.ops.int
import int.*
import compiletime.ops.any.IsConst
import compiletime.ops.boolean.{&&, !}
import compiletime.{constValueOpt, constValue}
import dfhdl.internals.{Inlined, ITE}
import scala.annotation.targetName

type IntP = Int | DFConstInt32 | IntP.Sig
object IntP:
  sealed trait Sig:
    val value: DFConstInt32
  object Sig:
    given [S <: Sig](using s: S): ValueOf[S] = ValueOf[S](s)
    given [F <: FuncOp, L <: IntP, R <: IntP](using
        ValueOf[F],
        ValueOf[L],
        ValueOf[R],
        DFC
    ): Sig2[F, L, R] with
      val value: DFConstInt32 = ???
  sealed trait Sig1[F <: FuncOp, A <: IntP] extends Sig
  sealed trait Sig2[F <: FuncOp, A <: IntP, B <: IntP] extends Sig

  /** Whether both operands are literal `Int` widths, and so the operation can be folded to a
    * literal at the type level.
    *
    * A width that is anything else (an `Int` of unknown value, a DFHDL constant, an expression over
    * either) answers `false` and the operation collapses to `Int`. The type level cannot say more
    * about such a width than "some `Int`", and it does not have to: elaboration carries the real
    * width symbolically in the IR's `IntParamRef` and checks it there, which is where every
    * non-literal width check has always happened.
    *
    * `IsConst` is what makes this decidable. Testing literal-ness with `Int & Singleton` instead
    * leaves `Int` neither matching nor provably disjoint, which STUCKS the reduction rather than
    * falling through, and a collapsed width feeding a further operation is exactly the common case
    * (`Max[Int, Int]`).
    *
    * The guard must be applied to a width DIRECTLY, never to a composition of these operators.
    * `IsConst` answers `false` for an unreduced match type instead of deferring
    * (https://github.com/scala/scala3/issues/26683), so `CLog2[+[V, 1]]` collapses to `Int` at the
    * DEFINITION site, where `+[V, 1]` is still stuck, and the call site never gets to supply a
    * literal. The failure is silent: no error, just a width that quietly became `Int`. A composed
    * width is therefore written as one [[FoldConst1]] whose body does the whole calculation in
    * `compiletime.ops.int`, which reduces normally.
    */
  type IsConstInt2[L, R] <: Boolean = (L, R) match
    case (Int, Int) => IsConst[L] && IsConst[R]
    case _          => false
  type IsConstInt1[T] <: Boolean = T match
    case Int => IsConst[T]
    case _   => false

  /** Applies `F` to `V` when `V` is a literal `Int`, and collapses to `Int` otherwise. `F` is
    * spelled in `compiletime.ops.int` so that no intermediate result is ever handed back to a
    * guarded operator of this algebra.
    */
  type FoldConst1[V <: IntP, F[_ <: Int] <: Int] <: IntP = IsConstInt1[V] match
    case true  => F[V & Int]
    case false => Int

  /** The two-operand [[FoldConst1]]. `F` may close over other type parameters (an operand's sign,
    * say) as long as the widths themselves are the two it receives.
    */
  type FoldConst2[L <: IntP, R <: IntP, F[_ <: Int, _ <: Int] <: Int] <: IntP =
    IsConstInt2[L, R] match
      case true  => F[L & Int, R & Int]
      case false => Int

  type +[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.+[L, R]
    case false => Int
  type -[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.-[L, R]
    case false => Int
  type *[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.*[L, R]
    case false => Int
  type /[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int./[L, R]
    case false => Int
  type %[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.%[L, R]
    case false => Int
  infix type Max[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.Max[L, R]
    case false => Int
  infix type Min[L <: IntP, R <: IntP] <: IntP = IsConstInt2[L, R] match
    case true  => int.Min[L, R]
    case false => Int
  // the arithmetic is spelled with `int.*` rather than the infix operators, which inside
  // `object IntP` would bind to `IntP`'s own (guarded) ones and not reduce
  type CLog2[T <: IntP] =
    FoldConst1[T, [X <: Int] =>> int.-[32, NumberOfLeadingZeros[int.-[X, 1]]]]
  type Abs[T <: IntP] = FoldConst1[T, [X <: Int] =>> int.Abs[X]]

  // Fused compositions over CLog2. Each is what it says in its comment, but computed under a
  // SINGLE guard on `V`; writing them as nested applications of the operators above collapses
  // them to `Int` (see the note on `IsConstInt2`).
  /** `CLog2[V + 1]`, i.e. the width that represents the values `0` to `V`. */
  type CLog2P1[V <: IntP] =
    FoldConst1[V, [X <: Int] =>> int.-[32, NumberOfLeadingZeros[X]]]

  /** `CLog2[V] + 1`, i.e. [[CLog2]] plus a sign bit. */
  type CLog2Signed[V <: IntP] =
    FoldConst1[V, [X <: Int] =>> int.+[int.-[32, NumberOfLeadingZeros[int.-[X, 1]]], 1]]

  /** `CLog2[V + 1] + 1`, i.e. [[CLog2P1]] plus a sign bit. */
  type CLog2P1Signed[V <: IntP] =
    FoldConst1[V, [X <: Int] =>> int.+[int.-[32, NumberOfLeadingZeros[X]], 1]]

  /** The result width of a commutative arithmetic operation (`+`, `*`, `max`, `min`) on two
    * bit-accurate operands: the wider of the two, after widening an unsigned operand by the sign
    * bit it gains when it meets a signed one.
    *
    * Fused for the same reason as the [[CLog2]] compositions: handing the sign-adjusted widths to
    * [[Max]] would put an unreduced match type under its guard.
    */
  type ArithMaxWidth[LS <: Boolean, LW <: IntP, RS <: Boolean, RW <: IntP] =
    FoldConst2[LW, RW, [X <: Int, Y <: Int] =>> int.Max[
      ITE[![LS] && RS, int.+[X, 1], X],
      ITE[![RS] && LS, int.+[Y, 1], Y]
    ]]

  /** [[ArithMaxWidth]] for two same-signed operands, plus the carry bit (`+^`, `-^`). */
  type ArithCarryWidth[LW <: IntP, RW <: IntP] =
    FoldConst2[LW, RW, [X <: Int, Y <: Int] =>> int.+[int.Max[X, Y], 1]]

  /** `BI + SW - 1`, the high index of an ascending part-select anchored at `BI`. */
  type PartSelectHigh[BI <: IntP, SW <: IntP] =
    FoldConst2[BI, SW, [X <: Int, Y <: Int] =>> int.-[int.+[X, Y], 1]]

  /** `V + 1` for a width already known to be an `Int`, staying inside `compiletime.ops` so the
    * result is `Int`-bounded and needs no guard.
    */
  type Inc[V <: Int] = int.+[V, 1]

  /** `HI - LO + 1`, the width of an inclusive bit range. */
  type RangeWidth[HI <: IntP, LO <: IntP] =
    FoldConst2[HI, LO, [X <: Int, Y <: Int] =>> int.+[int.-[X, Y], 1]]

  /** `BI - SW + 1`, the low index of a descending part-select anchored at `BI`. */
  type PartSelectLow[BI <: IntP, SW <: IntP] = RangeWidth[BI, SW]
end IntP

into opaque type IntParam[V <: IntP] = Int | DFConstInt32
protected sealed trait IntParamLP:
  given [T <: IntP]: Conversion[IntParam[T], IntParam[Int]] = value =>
    value.asInstanceOf[IntParam[Int]]
object IntParam extends IntParamLP:
  given [L <: IntP, R <: IntP](using CanEqual[L, R]): CanEqual[IntParam[L], IntParam[R]] =
    CanEqual.derived
  given [T <: IntP]: CanEqual[IntParam[T], Int] = CanEqual.derived
  given [T <: IntP]: CanEqual[Int, IntParam[T]] = CanEqual.derived

  inline implicit def fromValue[T <: IntP & Singleton](inline value: T): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueInlined")
  inline implicit def fromValue[T <: Int](inline value: Inlined[T]): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueWide")
  inline implicit def fromValue[Wide <: IntP](inline value: Wide): IntParam[Wide] =
    value.asInstanceOf[IntParam[Wide]]
  inline def apply[T <: IntP](inline value: T): IntParam[T] = value match
    case sig: IntP.Sig => sig.value.asInstanceOf[IntParam[T]]
    case _             => value.asInstanceOf[IntParam[T]]
  inline def forced[V <: IntP](inline value: IntP): IntParam[V] = value.asInstanceOf[IntParam[V]]
  @targetName("applyInlined")
  inline def apply[V <: Int](inline value: Inlined[V]): IntParam[V] =
    value.asInstanceOf[IntParam[V]]
  private def calc[O <: IntP, V <: IntP](op: FuncOp, arg: IntParam[V])(
      opInt: Int => Int
  )(using dfc: DFC): IntParam[O] =
    given DFC = dfc.anonymize
    arg match
      case int: Int            => forced[O](opInt(int))
      case const: DFConstInt32 => forced[O](DFVal.Func(DFInt32, op, List(const)))
  private def calc[O <: IntP, L <: IntP, R <: IntP](
      op: FuncOp,
      argL: IntParam[L],
      argR: IntParam[R]
  )(
      opInt: (Int, Int) => Int
  )(using dfc: DFC): IntParam[O] =
    given DFC = dfc.anonymize
    (argL, argR) match
      case (intL: Int, intR: Int) => forced[O](opInt(intL, intR))
      case _                      =>
        val constL = argL.toDFConst
        val constR = argR.toDFConst
        import dfc.getSet
        def func = forced[O](DFVal.Func(DFInt32, op, List(constL, constR)))
        func
    end match

  end calc
  extension [L <: IntP](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstInt32 =
      lhs match
        case int: Int            => DFConstInt32(int, named = true)
        case const: DFConstInt32 => const
    def toScalaIntOpt: Option[Int] =
      lhs match
        case int: Int            => Some(int)
        case const: DFConstInt32 =>
          import dfc.getSet
          val constIR = const.asIR
          constIR.injectGlobalCtx()
          constIR.getConstData[Option[BigInt]] match
            case ir.ConstData.KnownConst(Some(i: BigInt)) => Some(i.toInt)
            case _                                        => None
    def toScalaIntUNSAFE: Int = toScalaIntOpt.get
    def ref: ir.IntParamRef =
      lhs match
        case int: Int            => ir.IntParamRef(int)
        case const: DFConstInt32 =>
          val constIR = const.asIR
          constIR.injectGlobalCtx()
          val reachable = constIR.getReachableMember
          val newRef = dfc.refGen.genTypeRef
          ir.IntParamRef(dfc.mutableDB.newRefFor(newRef, reachable))
    def +[R <: IntP](rhs: IntParam[R]): IntParam[IntP.+[L, R]] =
      calc(FuncOp.+, lhs, rhs)(_ + _)
    def -[R <: IntP](rhs: IntParam[R]): IntParam[IntP.-[L, R]] =
      calc(FuncOp.-, lhs, rhs)(_ - _)
    def *[R <: IntP](rhs: IntParam[R]): IntParam[IntP.*[L, R]] =
      calc(FuncOp.`*`, lhs, rhs)(_ * _)
    def /[R <: IntP](rhs: IntParam[R]): IntParam[IntP./[L, R]] =
      calc(FuncOp./, lhs, rhs)(_ / _)
    def %[R <: IntP](rhs: IntParam[R]): IntParam[IntP.%[L, R]] =
      calc(FuncOp.%, lhs, rhs)(_ % _)
    infix def max[R <: IntP](rhs: IntParam[R]): IntParam[IntP.Max[L, R]] =
      import scala.runtime.RichInt
      calc(FuncOp.max, lhs, rhs)((x, y) => RichInt(x) max y)
    infix def min[R <: IntP](rhs: IntParam[R]): IntParam[IntP.Min[L, R]] =
      import scala.runtime.RichInt
      calc(FuncOp.min, lhs, rhs)((x, y) => RichInt(x) min y)
    def clog2: IntParam[IntP.CLog2[L]] =
      calc(FuncOp.clog2, lhs)(dfhdl.internals.clog2)
    def =~[R <: IntP](that: IntParam[R]): Boolean =
      import dfc.getSet
      (lhs, that) match
        case (intL: Int, intR: Int)                       => intL == intR
        case (constL: DFConstInt32, constR: DFConstInt32) => constL.asIR =~ constR.asIR
        case _                                            => false
    protected[dfhdl] def cloneAnonValueAndDepsHere: IntParam[Int] =
      lhs match
        case int: Int            => int
        case const: DFConstInt32 =>
          dfhdl.core.cloneAnonValueAndDepsHere(const.asIR).asConstOf[DFInt32]
  end extension
end IntParam

extension (intParamRef: ir.IntParamRef)
  def get(using dfc: DFC): IntParam[Int] =
    intParamRef match
      case int: Int              => IntParam.forced[Int](int)
      case ref: ir.DFRef.TypeRef =>
        import dfc.getSet
        IntParam.forced[Int](ref.get.asConstOf[DFInt32])
  protected[core] def refCodeString(using dfc: DFC): String =
    import dfc.getSet
    import dfhdl.compiler.printing.{Printer, DefaultPrinter}
    import dfhdl.compiler.printing.refCodeString as refCodeStringIR
    given printer: Printer = DefaultPrinter
    intParamRef.refCodeStringIR
