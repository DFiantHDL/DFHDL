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
  )(using Quotes, Type[T]): Expr[Exactly] =
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
  end fromValueMacro

  given toValueSing[T]: Conversion[Exact[ValueOf[T]], T] =
    precise => precise.value.value
  given toValue[T]: Conversion[Exact[T], T] = precise => precise.value
end Exactly

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
        val tupleType = tycon.appliedTo(tpes).asTypeOf[Any]
        val tupleTerm = Apply(TypeApply(fun, tupleTypeArgs), terms)
        val expr =
          '{
            ValueOf[tupleType.Underlying](${ tupleTerm.asExpr })
          }
        expr.asTerm.underlyingArgument
      case t =>
        val tpe = t.tpe.widen
        if (tpe <:< TypeRepr.of[NonEmptyTuple])
          val tType = tpe.asTypeOf[Any]
          '{ ValueOf[tType.Underlying](${ t.asExpr }) }.asTerm
        else t
    end match

type Exact[T] = Exactly { type Out = T }
object Exact:
  def apply[T](value_ : T): Exact[T] = new Exactly:
    type Out = T
    val value = value_

  trait Summon[T]:
    type Out
    def apply(t: T): Out
  object Summon:
    transparent inline given [T]: Summon[T] =
      ${ summonMacro[T] }
    def summonMacro[T](using Quotes, Type[T]): Expr[Summon[T]] =
      import quotes.reflect.*
      Expr.summon[ValueOf[T]].map(_.asTerm) match
        case Some(Apply(_, arg :: Nil)) =>
          val exact = arg.exactTerm
          val exactExpr = exact.asExpr
          val exactType = exact.tpe.asTypeOf[Any]
          '{
            new Summon[T]:
              type Out = exactType.Underlying
              def apply(t: T) = ${ exactExpr }
          }
        case _ =>
          '{
            new Summon[T]:
              type Out = T
              def apply(t: T): Out = t
          }
      end match
    end summonMacro
  end Summon
end Exact
