package DFiant.internals
import scala.quoted.*

trait IntInfo[V <: Int]:
  type OutS <: Boolean
  type OutW <: Int
  def signed(value: Int): Inlined[OutS] =
    Inlined.forced[OutS](value < 0)
  def width(value: Int): Inlined[OutW] =
    Inlined.forced[OutW](IntInfo.calcWidth(value))
object IntInfo:
  def calcWidth(value: Int): Int =
    if (value == -1) 2
    else if (value < 0) 33 - Integer.numberOfLeadingZeros(-value - 1)
    else if (value > 0) 32 - Integer.numberOfLeadingZeros(value)
    else 1 //value == 0

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
        '{
          new IntInfo[V]:
            type OutS = signedType.Underlying
            type OutW = widthType.Underlying
        }
      case _ =>
        '{
          new IntInfo[V]:
            type OutS = Boolean
            type OutW = Int
        }
    end match
  end macroImpl
end IntInfo
