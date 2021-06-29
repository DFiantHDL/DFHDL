package DFiant.internals
import scala.quoted.*

opaque type Exact[T] = T
object Exact:
  //For singleton integers we create a special macro that offers some protection from hex literals that
  //overflow into negative values. E.g., 0x80000000
  //This is no way close to a full protection from such incidents, but this is enough for most newbie cases
  //that DFiant code may encounter.
  implicit inline def fromIntSing[T <: Int with Singleton](
      inline value: T
  ): Exact[ValueOf[T]] = fromIntSing2(value)
  //TODO: remove when https://github.com/lampepfl/dotty/issues/12975 is resolved
  inline def fromIntSing2[T <: Int](
      inline value: T
  ): Exact[ValueOf[T]] = ${ fromIntSingMacro[T]('value) }
  def fromIntSingMacro[T <: Int](
      value: Expr[T]
  )(using Quotes, Type[T]): Expr[Exact[ValueOf[T]]] = {
    import quotes.reflect.*
    val valueTerm = value.asTerm.underlyingArgument
    valueTerm match
      case Literal(IntConstant(i: Int)) if i < 0 =>
        val pos = Position.ofMacroExpansion
        val content = pos.sourceCode.get
        if (content.startsWith("0x") || content.startsWith("0X"))
          val properText = s"""h"${content.drop(2)}""""
          report.error(
            s"""Found a hex integer literal that overflows into a negative value.
               |Please use DFiant's built in string interpolator literals instead.
               |E.g.: $properText""".stripMargin
          )
      case _ => //do nothing
    '{ ValueOf[T]($value) }
  }
  implicit def fromAnyValSing[T <: Singleton with AnyVal](
      value: T
  ): Exact[ValueOf[T]] = ValueOf(value)
  implicit def fromStringSing[T <: Singleton with String](
      value: T
  ): Exact[ValueOf[T]] = ValueOf(value)
  implicit def fromNonSing[T](value: T)(implicit di: DummyImplicit): Exact[T] =
    value
  implicit def toValueSing[T](precise: Exact[ValueOf[T]]): T =
    precise.value
  implicit def toValue[T](precise: Exact[T]): T = precise

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def exactTerm: quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Literal(const) =>
        val constTpe = ConstantType(const).asTypeOf[Any]
        val expr =
          '{
            ValueOf[constTpe.Underlying](${ term.asExpr })
          }
        expr.asTerm
      case Apply(TypeApply(fun, _), tupleArgs)
          if term.tpe <:< TypeRepr.of[NonEmptyTuple] =>
        val terms = tupleArgs.map(t => t.exactTerm)
        val tpes = terms.map(_.tpe)
        val AppliedType(tycon, _) = term.tpe
        val tupleTypeArgs = tpes.map(_.asTypeTree)
        Apply(TypeApply(fun, tupleTypeArgs), terms)
      case _ =>
        term
