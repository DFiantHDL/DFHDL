package dfhdl.internals
import scala.quoted.*
import util.NotGiven
import scala.annotation.unchecked.uncheckedVariance
type ExactTypes = NonEmptyTuple | Int | String | Boolean

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def exactTerm: quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, term) => term.exactTerm
      case Literal(const)      => term
      case t @ Apply(TypeApply(fun, _), tupleArgs) if t.tpe <:< TypeRepr.of[NonEmptyTuple] =>
        val terms = tupleArgs.map(t => t.exactTerm)
        val tpes = terms.map(_.tpe)
        val AppliedType(tycon, _) = t.tpe: @unchecked
        val tupleTypeArgs = tpes.map(_.asTypeTree)
        Apply(TypeApply(fun, tupleTypeArgs), terms)
      case t => t
    end match
  end exactTerm
end extension

final class Exact[T](val value: T) extends AnyVal
object Exact:
  def apply[T](value_ : T): Exact[T] = new Exact(value_)
  def strip(value: Any): Any =
    value match
      case exact: Exact[?] => strip(exact.value)
      case _               => value
  // We need this `fromRegularTypes` as a workaround for DFStruct where `v := XY(h"27", ...)`
  implicit inline def fromValue[T](value: T)(using NotGiven[T <:< ExactTypes]): Exact[T] =
    Exact[T](value)
  // TODO: remove when https://github.com/lampepfl/dotty/issues/12975 is resolved
  implicit transparent inline def fromExactTypes[T <: ExactTypes](
      inline value: T
  ): Exact[?] = ${ fromValueMacro[T]('value) }
  def fromValueMacro[T](
      value: Expr[T]
  )(using Quotes, Type[T]): Expr[Exact[?]] =
    import quotes.reflect.*
    val valueTerm = value.asTerm.exactTerm
//    println(valueTerm.show)
    valueTerm match
      // For singleton integers we create a special macro that offers some protection from hex literals that
      // overflow into negative values. E.g., 0x80000000
      // This is no way close to a full protection from such incidents, but this is enough for most newbie cases
      // that DFHDL code may encounter.
      case Literal(IntConstant(i: Int)) if i < 0 =>
        val pos = Position.ofMacroExpansion
        val content = pos.sourceCode.get
        if (content.startsWith("0x") || content.startsWith("0X"))
          val properText = s"""h"${content.drop(2)}""""
          report.error(
            s"""Found a hex integer literal that overflows into a negative value.
               |Please use DFHDL's built in string interpolator literals instead.
               |E.g.: $properText""".stripMargin
          )
      case _ => // do nothing
    end match

    val tpe = valueTerm.tpe match
      case c: ConstantType => c.asTypeOf[Any]
      case t               => t.widen.asTypeOf[Any]
    '{ Exact[tpe.Underlying](${ valueTerm.asExpr }) }
  end fromValueMacro

  implicit inline def toValue[T](exact: Exact[T]): T = exact.value

  trait Summon[R, T <: R]:
    type Out
    def apply(t: R): Out
  object Summon:
    given fromRegularTypes[R, T <: R]: Summon[R, T] with
      type Out = R
      def apply(t: R): Out = t
    transparent inline given fromExactTypes[R <: ExactTypes, T <: R]: Summon[R, T] =
      ${ summonMacro[R, T] }
    def summonMacro[R, T <: R](using
        Quotes,
        Type[R],
        Type[T]
    ): Expr[Summon[R, T]] =
      import quotes.reflect.*
      given CanEqual[Term, Term] = CanEqual.derived
      Expr.summon[ValueOf[T]].map(_.asTerm) match
        case Some(Apply(_, arg :: Nil)) =>
          val exact = arg.exactTerm
          val exactExpr = exact.asExpr
          val exactType = exact.tpe.asTypeOf[Any]
          '{
            new Summon[R, T]:
              type Out = exactType.Underlying
              def apply(t: R) = ${ exactExpr }
          }
        case _ =>
          '{
            new Summon[R, T]:
              type Out = R
              def apply(t: R): Out = t
          }
      end match
    end summonMacro
  end Summon
end Exact
