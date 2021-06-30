package DFiant.internals
import scala.quoted.*

trait Exactly:
  type Out
  val value: Out
object Exactly:
  //TODO: remove when https://github.com/lampepfl/dotty/issues/12975 is resolved
  implicit transparent inline def fromValue[T](
      inline value: T
  ): Exactly = ${ fromValueMacro[T]('value) }
  def fromValueMacro[T](
      value: Expr[T]
  )(using Quotes, Type[T]): Expr[Exactly] = {
    import quotes.reflect.*
    val valueTerm = value.asTerm.exactTerm
//    println(valueTerm.show)
    valueTerm match
      //For singleton integers we create a special macro that offers some protection from hex literals that
      //overflow into negative values. E.g., 0x80000000
      //This is no way close to a full protection from such incidents, but this is enough for most newbie cases
      //that DFiant code may encounter.
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
    val tpe = valueTerm.tpe.widen.asTypeOf[Any]
    '{ Exact[tpe.Underlying](${ valueTerm.asExpr }) }
  }

  implicit def toValueSing[T](precise: Exact[ValueOf[T]]): T =
    precise.value.value
  implicit def toValue[T](precise: Exact[T]): T = precise.value

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def exactTerm: quotes.reflect.Term =
    import quotes.reflect.*
    term.underlyingArgument match
      case Literal(const) =>
        val constTpe = ConstantType(const).asTypeOf[Any]
        val expr =
          '{
            ValueOf[constTpe.Underlying](${ term.asExpr })
          }
        expr.asTerm.underlyingArgument
      case t @ Apply(TypeApply(fun, _), tupleArgs)
          if t.tpe <:< TypeRepr.of[NonEmptyTuple] =>
        val terms = tupleArgs.map(t => t.exactTerm)
        val tpes = terms.map(_.tpe)
        val AppliedType(tycon, _) = t.tpe
        val tupleTypeArgs = tpes.map(_.asTypeTree)
        Apply(TypeApply(fun, tupleTypeArgs), terms)
      case t => t

type Exact[T] = Exactly { type Out = T }
object Exact:
  def apply[T](value_ : T): Exact[T] = new Exactly:
    type Out = T
    val value = value_
