package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import ir.DFDecimal.NativeType
import compiletime.ops.int.*
import compiletime.{constValueOpt, constValue}
import scala.annotation.targetName
type CLog2[T <: Int] = 32 - NumberOfLeadingZeros[T - 1]

opaque type IntParam[V <: Int] = V | DFConstOf[DFInt32]
protected sealed trait IntParamLP:
  given [T <: Int]: Conversion[IntParam[T], IntParam[Int]] = value =>
    value.asInstanceOf[IntParam[Int]]
object IntParam extends IntParamLP:
  given [L <: Int, R <: Int](using CanEqual[L, R]): CanEqual[IntParam[L], IntParam[R]] =
    CanEqual.derived
  given [T <: Int]: CanEqual[IntParam[T], Int] = CanEqual.derived
  given [T <: Int]: CanEqual[Int, IntParam[T]] = CanEqual.derived

  transparent inline implicit def getValue[T <: Int](
      intParam: IntParam[T]
  ): T =
    inline constValueOpt[T] match
      case Some(_) => constValue[T]
      case None =>
        val dfc = compiletime.summonInline[DFC]
        toScalaInt(intParam)(using dfc).asInstanceOf[T]

  inline implicit def fromValue[T <: Int & Singleton](value: T): IntParam[T] = value
  @targetName("fromValueWide")
  inline implicit def fromValue[Wide <: Int](value: Wide): IntParam[Wide] = value
  @targetName("fromValueDFConst")
  inline implicit def fromValue[S <: Boolean, W <: Int, N <: NativeType, P, R <: DFValTP[DFXInt[S, W, N], P]](
      value: R
  )(using tc: DFVal.TC[DFInt32, R], dfc: DFC, const: DFVal.ConstCheck[P]): IntParam[Int] =
    tc(DFInt32, value).asInstanceOf[IntParam[Int]]

  extension [L <: Int](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstOf[DFInt32] =
      lhs match
        case int: Int => DFVal.Const(DFInt32, Some(BigInt(int)), named = true)
        case const: DFConstOf[DFInt32] => const
    def toScalaInt: Int =
      lhs match
        case int: Int                     => int
        case const: DFConstOf[DFInt32] => DFXInt.Val.Ops.toScalaInt(const)
    def ref: ir.IntParamRef =
      lhs match
        case int: Int => ir.IntParamRef(int)
        case const: DFConstOf[DFInt32] =>
          ir.IntParamRef(const.asInstanceOf[DFValAny].asIR.refTW[ir.DFVal])
  end extension
end IntParam
