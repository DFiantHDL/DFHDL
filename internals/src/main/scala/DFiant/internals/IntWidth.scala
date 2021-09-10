package DFiant.internals
import scala.quoted.*

trait IntWidth[V <: Int, Signed <: Boolean]:
  type Out <: Int
  def apply(value: Int)(using ValueOf[Signed]): Inlined[Out] =
    Inlined.forced[Out](IntWidth.calcWidth(value, valueOf[Signed]))
object IntWidth:
  private[internals] def calcWidth(value: Int, signed: Boolean): Int =
    val lzc = Integer.numberOfLeadingZeros(value)
    if (signed)
      if (value > 0) 33 - lzc
      else if (value == 0) 2
      else 33 - Integer.numberOfLeadingZeros(-value)
    else if (value != 0) 32 - lzc
    else 1

  transparent inline given [V <: Int, Signed <: Boolean]: IntWidth[V, Signed] =
    ${ macroImpl[V, Signed] }
  def macroImpl[V <: Int, Signed <: Boolean](using
      Quotes,
      Type[V],
      Type[Signed]
  ): Expr[IntWidth[V, Signed]] =
    import quotes.reflect.*
    val valueTpe = TypeRepr.of[V]
    val ConstantType(BooleanConstant(signed)) = TypeRepr.of[Signed]
    val widthTpe = valueTpe match
      case ConstantType(IntConstant(value)) =>
        ConstantType(IntConstant(calcWidth(value, signed)))
      case _ => TypeRepr.of[Int]
    val widthType = widthTpe.asTypeOf[Int]
    '{
      new IntWidth[V, Signed]:
        type Out = widthType.Underlying
    }
  end macroImpl
end IntWidth

type UIntWidth[V <: Int] = IntWidth[V, false]
type SIntWidth[V <: Int] = IntWidth[V, true]
