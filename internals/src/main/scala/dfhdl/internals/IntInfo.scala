package dfhdl.internals
import scala.quoted.*

trait IntInfo[V <: Int]:
  type OutS <: Boolean
  type OutW <: Int
  def signed(value: Int): Inlined[OutS] =
    Inlined.forced[OutS](value < 0)
  def width(value: Int): Inlined[OutW] =
    Inlined.forced[OutW](IntInfo.calcWidth(value))
object IntInfo:
  def gen[R <: Int, OS <: Boolean, OW <: Int]: Aux[R, OS, OW] = new IntInfo[R]:
    type OutS = OS
    type OutW = OW
  type Aux[R <: Int, OS <: Boolean, OW <: Int] = IntInfo[R]:
    type OutS = OS
    type OutW = OW
  def calcWidth(value: Int): Int =
    if (value == -1) 2
    else if (value < 0) 33 - Integer.numberOfLeadingZeros(-value - 1)
    else if (value > 0) 32 - Integer.numberOfLeadingZeros(value)
    else 1 // value == 0

  transparent inline given [V <: Int]: IntInfo[V] =
    ${ macroImpl[V] }
  def macroImpl[V <: Int](using
      Quotes,
      Type[V]
  ): Expr[IntInfo[V]] =
    import quotes.reflect.*
    val valueTpe = TypeRepr.of[V]
    valueTpe match
      case ConstantType(IntConstant(value)) =>
        val signedType =
          ConstantType(BooleanConstant(value < 0)).asTypeOf[Boolean]
        val widthType =
          ConstantType(IntConstant(calcWidth(value))).asTypeOf[Int]
        '{ IntInfo.gen[V, signedType.Underlying, widthType.Underlying] }
      case _ =>
        '{ IntInfo.gen[V, Boolean, Int] }
    end match
  end macroImpl
end IntInfo
