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
  inline implicit def fromValue[S <: Boolean, W <: Int, N <: NativeType, P, R <: DFValTP[
    DFXInt[S, W, N],
    P
  ]](
      value: R
  )(using tc: DFVal.TC[DFInt32, R], dfc: DFC, const: DFVal.ConstCheck[P]): IntParam[Int] =
    tc(DFInt32, value).asInstanceOf[IntParam[Int]]
  def apply(value: Int): IntParam[Int] = value
  def apply(value: DFConstOf[DFInt32]): IntParam[Int] = value
  def calc[O <: Int](op: FuncOp, argL: IntParam[Int], argR: IntParam[Int])(
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
  extension [L <: Int](lhs: IntParam[L])(using dfc: DFC)
    def toDFConst: DFConstOf[DFInt32] =
      lhs match
        case int: Int                  => DFVal.Const(DFInt32, Some(BigInt(int)), named = true)
        case const: DFConstOf[DFInt32] => const
    def toScalaInt: Int =
      lhs match
        case int: Int                  => int
        case const: DFConstOf[DFInt32] => DFXInt.Val.Ops.toScalaInt(const)
    def ref: ir.IntParamRef =
      lhs match
        case int: Int => ir.IntParamRef(int)
        case const: DFConstOf[DFInt32] =>
          val newRef = new ir.DFRef.TypeRef {}
          ir.IntParamRef(dfc.mutableDB.newRefFor(newRef, const.asInstanceOf[DFValAny].asIR))
    def +[R <: Int](rhs: IntParam[R]): IntParam[L + R] =
      calc(FuncOp.+, lhs, rhs)(_ + _)
    def *[R <: Int](rhs: IntParam[R]): IntParam[L * R] =
      calc(FuncOp.`*`, lhs, rhs)(_ * _)
  end extension
end IntParam

extension (intParamRef: ir.IntParamRef)
  def get(using dfc: DFC): IntParam[Int] =
    intParamRef match
      case int: Int => IntParam(int)
      case ref: ir.DFRef.TypeRef =>
        import dfc.getSet
        IntParam(ref.get.asConstOf[DFInt32])
