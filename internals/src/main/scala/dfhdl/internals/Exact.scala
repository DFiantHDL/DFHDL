package dfhdl.internals
import scala.quoted.*
import util.NotGiven
import scala.collection.concurrent.TrieMap

final class ExactInfo[Q <: Quotes & Singleton](using val quotes: Q)(val term: quotes.reflect.Term):
  import quotes.reflect.*
  val exactTpe: quotes.reflect.TypeRepr =
    term.tpe match
      // keeping constants as they are
      case c: ConstantType => c
      // term references are widened only if they are local or parameters
      case t: TermRef =>
        val flags = t.termSymbol.flags
        if (flags.is(Flags.Local) || flags.is(Flags.ParamAccessor)) t.widen
        else t
      // the rest are widened
      case t => t.widen
  val exactType: Type[Any] = exactTpe.asTypeOf[Any]
  val exactExpr: Expr[Any] = term.asExpr
  type Underlying = exactType.Underlying
end ExactInfo
extension [Q <: Quotes & Singleton](using quotes: Q)(exprOrTerm: Expr[Any] | quotes.reflect.Term)
  def exactInfo: ExactInfo[Q] =
    import quotes.reflect.*
    val term = exprOrTerm match
      case expr: Expr[Any]                      => expr.asTerm
      case term: quotes.reflect.Term @unchecked => term
    ExactInfo[Q](using quotes)(term.exactTerm)
extension [Q <: Quotes & Singleton](using quotes: Q)(term: quotes.reflect.Term)
  private def exactTerm: quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(a, b, term)     => Inlined(a, b, term.exactTerm)
      case Literal(NullConstant()) => report.errorAndAbort("null is not allowed here")
      // For singleton integers we create a special macro that offers some protection from hex literals that
      // overflow into negative values. E.g., 0x80000000
      // This is no way close to a full protection from such incidents, but this is enough for most newbie cases
      // that DFHDL code may encounter.
      case Literal(IntConstant(i: Int)) if i < 0 =>
        val content = term.pos.sourceCode.get
        if (content.startsWith("0x") || content.startsWith("0X"))
          val properText = s"""h"${content.drop(2)}""""
          report.warning(
            s"""Found a hex integer literal that overflows into a negative value.
               |Please use DFHDL's built in string interpolator literals instead.
               |E.g.: $properText""".stripMargin,
            term.pos
          )
        term
      case Literal(const) => term
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
  // private val cacheMap = TrieMap.empty[String, (Quotes, Expr[Any])]
  // def cacheErrorExpr(msg: String)(using Quotes): Expr[Nothing] =
  //   import quotes.reflect.*
  //   val sourceFile = Position.ofMacroExpansion.sourceFile
  //   val cached = cacheMap(sourceFile.path)
  //   println(s"get: ${Position.ofMacroExpansion}")
  //   val (start, end) =
  //     given Quotes = cached._1
  //     import quotes.reflect.*
  //     val term = cached._2.asTerm
  //     (term.pos.start, term.pos.end)
  //   println(s"actual: <$start..$end>")
  //   val msgExpr = Expr(msg)
  //   val startExpr = Expr(start)
  //   val endExpr = Expr(end)
  //   '{ compiletimeErrorPos($msgExpr, $startExpr, $endExpr) }
  // end cacheErrorExpr

  // transparent inline def cacheError(msg: String): Nothing = ${ cacheErrorMacro('msg) }
  // def cacheErrorMacro(msg: Expr[String])(using Quotes): Expr[Nothing] =
  //   cacheErrorExpr(msg.value.get)

  inline def apply[T](inline value: T): Exact[T] = new Exact(value)
  def strip(value: Any): Any =
    value match
      case exact: Exact[?] => strip(exact.value)
      case _               => value
  implicit transparent inline def fromValue[T](
      inline value: T
  ): Exact[?] = ${ fromValueMacro[T]('value) }
  def fromValueMacro[T](
      value: Expr[T]
  )(using Quotes, Type[T]): Expr[Exact[?]] =
    import quotes.reflect.*
    // val x = Position.ofMacroExpansion.sourceFile.path -> (quotes, value)
    // println("---------")
    // println(s"set: ${Position.ofMacroExpansion}")
    // cacheMap += x
    val exactInfo = value.exactInfo
    '{ Exact[exactInfo.Underlying](${ exactInfo.exactExpr }) }
  end fromValueMacro

  implicit inline def toValue[T](inline exact: Exact[T]): T = exact.value

  trait Summon[R, T <: R]:
    type Out
    def apply(t: R): Out
  object Summon:
    transparent inline given [R, T <: R]: Summon[R, T] =
      ${ summonMacro[R, T] }
    object Success extends Summon[Any, Any]:
      type Out = Any
      def apply(t: Any): Out = t

    def summonMacro[R, T <: R](using
        Quotes,
        Type[R],
        Type[T]
    ): Expr[Summon[R, T]] =
      import quotes.reflect.*
      given CanEqual[Term, Term] = CanEqual.derived
      Expr.summon[ValueOf[T]].map(_.asTerm) match
        case Some(Apply(_, arg :: Nil)) =>
          val exactInfo = arg.exactInfo
          '{ Success.asInstanceOf[Summon[R, T] { type Out = exactInfo.Underlying }] }
        case _ =>
          '{ Success.asInstanceOf[Summon[R, T] { type Out = R }] }
      end match
    end summonMacro
  end Summon
end Exact

trait Exact0[
    Ctx,
    TC[From] <: Exact0.TC[From, Ctx]
]:
  type ExactFrom
  type ExactTC <: TC[ExactFrom]
  val tc: ExactTC
  def apply()(using ctx: Ctx): tc.Out
object Exact0:
  def apply[From, Ctx, TC[From] <: Exact0.TC[From, Ctx]](
      from: From,
      tc0: TC[From]
  ): Exact0[Ctx, TC] {
    type ExactFrom = From
    type ExactTC = tc0.type
  } = new Exact0[Ctx, TC]:
    type ExactFrom = From
    type ExactTC = tc0.type
    final val tc: ExactTC = tc0
    final def apply()(using ctx: Ctx): tc.Out =
      tc.conv(from)(using ctx)

  trait TC[From, Ctx]:
    type Out
    def conv(from: From)(using Ctx): Out
  transparent inline implicit def conv[
      From,
      Ctx,
      TC[From] <: Exact0.TC[From, Ctx]
  ](inline from: From): Exact0[Ctx, TC] = ${
    convMacro[From, Ctx, TC]('from)
  }
  def convMacro[
      From: Type,
      Ctx: Type,
      TC[From] <: Exact0.TC[From, Ctx]: Type
  ](from: Expr[From])(using Quotes): Expr[Exact0[Ctx, TC]] =
    import quotes.reflect.*
    val fromExactInfo = from.exactInfo
    '{
      Exact0[fromExactInfo.Underlying, Ctx, TC](
        ${ fromExactInfo.exactExpr },
        compiletime.summonInline[TC[fromExactInfo.Underlying]]
      )
    }
  end convMacro
end Exact0

trait Exact1[
    Arg1UB,
    Arg1 <: Arg1UB,
    FArg1[Arg1 <: Arg1UB],
    Ctx,
    TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx]
]:
  type ExactFrom
  type ExactTC <: TC[Arg1, ExactFrom]
  val tc: ExactTC
  def apply(arg1: FArg1[Arg1])(using ctx: Ctx): tc.Out
end Exact1
object Exact1:
  def apply[
      Arg1UB,
      Arg1 <: Arg1UB,
      FArg1[Arg1 <: Arg1UB],
      From,
      Ctx,
      TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx]
  ](
      from: From,
      tc0: TC[Arg1, From]
  ): Exact1[Arg1UB, Arg1, FArg1, Ctx, TC] {
    type ExactFrom = From
    type ExactTC = tc0.type
  } = new Exact1[Arg1UB, Arg1, FArg1, Ctx, TC]:
    type ExactFrom = From
    type ExactTC = tc0.type
    final val tc: ExactTC = tc0
    final def apply(arg1: FArg1[Arg1])(using ctx: Ctx): tc.Out =
      tc.conv(arg1, from)(using ctx)
  trait TC[Arg1UB, Arg1 <: Arg1UB, FArg1[Arg1 <: Arg1UB], From, Ctx]:
    type Out
    def conv(arg1: FArg1[Arg1], from: From)(using Ctx): Out
  transparent inline implicit def conv[
      Arg1UB,
      Arg1 <: Arg1UB,
      FArg1[Arg1 <: Arg1UB],
      From,
      Ctx,
      TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx]
  ](inline from: From): Exact1[Arg1UB, Arg1, FArg1, Ctx, TC] = ${
    convMacro[Arg1UB, Arg1, FArg1, From, Ctx, TC]('from)
  }
  def convMacro[
      Arg1UB: Type,
      Arg1 <: Arg1UB: Type,
      FArg1[Arg1 <: Arg1UB]: Type,
      From: Type,
      Ctx: Type,
      TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx]: Type
  ](from: Expr[From])(using Quotes): Expr[Exact1[Arg1UB, Arg1, FArg1, Ctx, TC]] =
    import quotes.reflect.*
    val fromExactInfo = from.exactInfo
    '{
      Exact1[Arg1UB, Arg1, FArg1, fromExactInfo.Underlying, Ctx, TC](
        ${ fromExactInfo.exactExpr },
        compiletime.summonInline[TC[Arg1, fromExactInfo.Underlying]]
      )
    }
  end convMacro
end Exact1
