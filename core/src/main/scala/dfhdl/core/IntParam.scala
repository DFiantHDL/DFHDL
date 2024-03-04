package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import compiletime.ops.int
import int.*
import compiletime.{constValueOpt, constValue}
import dfhdl.internals.Inlined
import dfhdl.internals.<:!
import scala.annotation.targetName

type IntP = Int | DFConstInt32 | IntP.Sig
object IntP:
  trait ToInt2[T <: IntP]:
    type Out <: Int
  type ToInt[V <: IntP] <: Int = V match
    case Int          => V <:! Int
    case DFConstInt32 => Int
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
  type +[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.+[L, R]
    case _          => Sig2[FuncOp.+.type, L, R]
  type -[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.-[L, R]
    case _          => Sig2[FuncOp.-.type, L, R]
  type *[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.*[L, R]
    case _          => Sig2[FuncOp.*.type, L, R]
  type /[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int./[L, R]
    case _          => Sig2[FuncOp./.type, L, R]
  type %[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.%[L, R]
    case _          => Sig2[FuncOp.%.type, L, R]
  infix type Max[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.Max[L, R]
    case _          => Sig2[FuncOp.max.type, L, R]
  infix type Min[L <: IntP, R <: IntP] <: IntP = (L, R) match
    case (Int, Int) => int.Min[L, R]
    case _          => Sig2[FuncOp.min.type, L, R]
  type CLog2[T <: IntP] <: IntP = T match
    case Int => 32 - NumberOfLeadingZeros[T - 1]
    case _   => Sig1[FuncOp.clog2.type, T]
end IntP

opaque type IntParam[V <: IntP] = Int | DFConstInt32
protected sealed trait IntParamLP:
  given [T <: IntP]: Conversion[IntParam[T], IntParam[Int]] = value =>
    value.asInstanceOf[IntParam[Int]]
object IntParam extends IntParamLP:
  given [L <: IntP, R <: IntP](using CanEqual[L, R]): CanEqual[IntParam[L], IntParam[R]] =
    CanEqual.derived
  given [T <: IntP]: CanEqual[IntParam[T], Int] = CanEqual.derived
  given [T <: IntP]: CanEqual[Int, IntParam[T]] = CanEqual.derived

  inline implicit def getValue[T <: IntP](intParam: IntParam[T])(using DFC): Int =
    intParam.toScalaInt

  inline implicit def fromValue[T <: IntP & Singleton](value: T): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueInlined")
  inline implicit def fromValue[T <: Int](value: Inlined[T]): IntParam[T] =
    value.asInstanceOf[IntParam[T]]
  @targetName("fromValueWide")
  inline implicit def fromValue[Wide <: IntP](value: Wide): IntParam[Wide] = value
  def apply[T <: IntP](value: T): IntParam[T] = value match
    case sig: IntP.Sig => sig.value.asInstanceOf[IntParam[T]]
    case _             => value.asInstanceOf[IntParam[T]]
  def forced[V <: IntP](value: IntP): IntParam[V] = value.asInstanceOf[IntParam[V]]
  @targetName("applyInlined")
  def apply[V <: Int](value: Inlined[V]): IntParam[V] = value.asInstanceOf[IntParam[V]]
  def calc[O <: IntP](op: FuncOp, arg: IntParam[Int])(
      opInt: Int => Int
  )(using dfc: DFC): IntParam[O] =
    given DFC = dfc.anonymize
    val ret: IntParam[Int] = arg match
      case int: Int            => IntParam(opInt(int))
      case const: DFConstInt32 => IntParam(DFVal.Func(DFInt32, op, List(const)))
    ret.asInstanceOf[IntParam[O]]
  def calc[O <: IntP](op: FuncOp, argL: IntParam[Int], argR: IntParam[Int])(
      opInt: (Int, Int) => Int
  )(using dfc: DFC): IntParam[O] =
    given DFC = dfc.anonymize
    val ret: IntParam[Int] = (argL, argR) match
      case (intL: Int, intR: Int) => IntParam(opInt(intL, intR))
      case _ =>
        val constL = argL.toDFConst
        val constR = argR.toDFConst
        IntParam(DFVal.Func(DFInt32, op, List(constL, constR)))
    ret.asInstanceOf[IntParam[O]]
  extension [L <: IntP](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstInt32 =
      lhs match
        case int: Int            => DFVal.Const(DFInt32, Some(BigInt(int)), named = true)
        case const: DFConstInt32 => const
    def toScalaInt: Int =
      lhs match
        case int: Int            => int
        case const: DFConstInt32 => DFXInt.Val.Ops.toScalaInt(const)
    def ref: ir.IntParamRef =
      lhs match
        case int: Int => ir.IntParamRef(int)
        case const: DFConstInt32 =>
          val constIR = const.asInstanceOf[DFValAny].asIR
          constIR.injectGlobalCtx()
          val newRef = new ir.DFRef.TypeRef {}
          ir.IntParamRef(dfc.mutableDB.newRefFor(newRef, constIR))
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
  end extension
end IntParam

extension (intParamRef: ir.IntParamRef)
  def get(using dfc: DFC): IntParam[Int] =
    intParamRef match
      case int: Int => IntParam(int)
      case ref: ir.DFRef.TypeRef =>
        import dfc.getSet
        IntParam.forced[Int](ref.get.asConstOf[DFInt32])
