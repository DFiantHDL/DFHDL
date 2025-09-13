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

  inline implicit def getValue[T <: IntP](inline intParam: IntParam[T])(using DFC): Int =
    intParam.toScalaInt

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
        op match
          // special casing max/min to remove the need for max and min if the same value is used
          case FuncOp.max | FuncOp.min if constL.asIR =~ constR.asIR =>
            constL.asInstanceOf[IntParam[O]]
          // special casing +/- operations and applying common associative reductions
          case FuncOp.+ | FuncOp.- =>
            (constL.asIR, argR) match
              case (
                    f @ ir.DFVal.Func(
                      _,
                      opL @ (FuncOp.+ | FuncOp.-),
                      List(ir.DFRef(argLL), ir.DFRef(constLR: ir.DFVal.Const)),
                      _,
                      _,
                      _
                    ),
                    intLR: Int
                  ) if f.isAnonymous =>
                val intLL = constLR.data.asInstanceOf[Option[BigInt]].get.toInt
                val constL = argLL.asConstOf[DFInt32]
                val intR = opL match
                  case FuncOp.+ => opInt(intLL, intLR)
                  case FuncOp.- =>
                    (op: @unchecked) match
                      case FuncOp.+ => intLR - intLL
                      case FuncOp.- => intLL + intLR
                if (intR == 0) constL.asInstanceOf[IntParam[O]]
                else
                  val constR = DFConstInt32(intR)
                  forced[O](DFVal.Func(DFInt32, opL, List(constL, constR)))
              case _ => func
          case _ => func
        end match
    end match

  end calc
  extension [L <: IntP](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstInt32 =
      lhs match
        case int: Int            => DFConstInt32(int, named = true)
        case const: DFConstInt32 => const
    def toScalaInt: Int =
      lhs match
        case int: Int            => int
        case const: DFConstInt32 => DFXInt.Val.Ops.toScalaInt(const)
    def ref: ir.IntParamRef =
      lhs match
        case int: Int            => ir.IntParamRef(int)
        case const: DFConstInt32 =>
          val constIR = const.asInstanceOf[DFValAny].asIR
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
