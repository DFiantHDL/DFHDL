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
      case Literal(const)                                                                  => term
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
    val exactInfo = value.exactInfo
    '{ Exact[exactInfo.Underlying](${ exactInfo.exactExpr }) }
  end fromValueMacro

  implicit inline def toValue[T](inline exact: Exact[T]): T = exact.value

  trait Summon[R, T <: R]:
    type Out
    def apply(t: R): Out
  object Summon:
    type Aux[R, T <: R, Out0] = Summon[R, T] { type Out = Out0 }
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
  val exactFrom: ExactFrom
  type ExactTC <: TC[ExactFrom]
  val tc: ExactTC
  def apply()(using ctx: Ctx): tc.Out
object Exact0:
  type Aux[Ctx, TC[From] <: Exact0.TC[From, Ctx], ExactFrom0, ExactTC0 <: TC[ExactFrom0]] =
    Exact0[Ctx, TC] {
      type ExactFrom = ExactFrom0
      type ExactTC = ExactTC0
    }
  implicit inline def toValue[
      Ctx,
      TC[From] <: Exact0.TC[From, Ctx]
  ](exact: Exact0[Ctx, TC]): exact.ExactFrom = exact.exactFrom
  def apply[From, Ctx, TC[From] <: Exact0.TC[From, Ctx]](
      from: From,
      tc0: TC[From]
  ): Aux[Ctx, TC, From, tc0.type] = new Exact0[Ctx, TC]:
    type ExactFrom = From
    final val exactFrom: ExactFrom = from
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
  lazy val exactFrom: ExactFrom
  type ExactTC <: TC[Arg1, ExactFrom]
  val tc: ExactTC
  def apply(arg1: FArg1[Arg1])(using ctx: Ctx): tc.Out
end Exact1
object Exact1:
  type Aux[
      Arg1UB,
      Arg1 <: Arg1UB,
      FArg1[Arg1 <: Arg1UB],
      Ctx,
      TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx],
      ExactFrom0,
      ExactTC0 <: TC[Arg1, ExactFrom0]
  ] = Exact1[Arg1UB, Arg1, FArg1, Ctx, TC] {
    type ExactFrom = ExactFrom0
    type ExactTC = ExactTC0
  }
  def apply[
      From,
      Arg1UB,
      Arg1 <: Arg1UB,
      FArg1[Arg1 <: Arg1UB],
      Ctx,
      TC[Arg1 <: Arg1UB, From] <: Exact1.TC[Arg1UB, Arg1, FArg1, From, Ctx]
  ](
      from: From,
      tc0: TC[Arg1, From]
  ): Aux[Arg1UB, Arg1, FArg1, Ctx, TC, From, tc0.type] = new Exact1[Arg1UB, Arg1, FArg1, Ctx, TC]:
    type ExactFrom = From
    final lazy val exactFrom: ExactFrom = from
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
      Exact1[fromExactInfo.Underlying, Arg1UB, Arg1, FArg1, Ctx, TC](
        ${ fromExactInfo.exactExpr },
        compiletime.summonInline[TC[Arg1, fromExactInfo.Underlying]]
      )
    }
  end convMacro
end Exact1

/////////////////////////////////////////////////////////////////////////////////
// ExactOp1
/////////////////////////////////////////////////////////////////////////////////
trait ExactOp1[Op, OutUB, Ctx, LHS]:
  type Out <: OutUB
  def apply(lhs: LHS)(using Ctx): Out
type ExactOp1Aux[Op, OutUB, Ctx, LHS, O <: OutUB] =
  ExactOp1[Op, OutUB, Ctx, LHS] { type Out = O }
transparent inline def exactOp1[Op, Ctx, OutUB](inline lhs: Any)(using ctx: Ctx): OutUB =
  ${ exactOp1Macro[Op, Ctx, OutUB]('lhs)('ctx) }
private def exactOp1Macro[Op, Ctx, OutUB](lhs: Expr[Any])(ctx: Expr[Ctx])(using
    Quotes,
    Type[Op],
    Type[Ctx],
    Type[OutUB]
): Expr[OutUB] =
  import quotes.reflect.*
  val lhsExactInfo = lhs.exactInfo
  Expr.summon[ExactOp1[Op, OutUB, Ctx, lhsExactInfo.Underlying]] match
    case Some(expr) => '{ $expr(${ lhsExactInfo.exactExpr })(using $ctx) }
    case None       =>
      ControlledMacroError.report("Unsupported argument type for this operation.")
  end match
end exactOp1Macro

/////////////////////////////////////////////////////////////////////////////////
// ExactOp2
/////////////////////////////////////////////////////////////////////////////////
trait ExactOp2[Op, Ctx, OutUB, LHS, RHS]:
  type Out <: OutUB
  def apply(lhs: LHS, rhs: RHS)(using Ctx): Out
type ExactOp2Aux[Op, Ctx, OutUB, LHS, RHS, O <: OutUB] =
  ExactOp2[Op, Ctx, OutUB, LHS, RHS] { type Out = O }
transparent inline def exactOp2[Op, Ctx, OutUB](
    inline lhs: Any,
    inline rhs: Any,
    inline bothWays: Boolean = false
)(using
    ctx: Ctx
): OutUB = ${ exactOp2Macro[Op, Ctx, OutUB]('lhs, 'rhs, 'bothWays)('ctx) }
private def exactOp2Macro[Op, Ctx, OutUB](
    lhs: Expr[Any],
    rhs: Expr[Any],
    bothWays: Expr[Boolean]
)(ctx: Expr[Ctx])(
    using
    Quotes,
    Type[Op],
    Type[Ctx],
    Type[OutUB]
): Expr[OutUB] =
  import quotes.reflect.*
  val lhsExactInfo = lhs.exactInfo
  val rhsExactInfo = rhs.exactInfo
  val exactOp2ExprOrError =
    try
      Expr.summonOrError[ExactOp2[
        Op,
        Ctx,
        OutUB,
        lhsExactInfo.Underlying,
        rhsExactInfo.Underlying
      ]]
    catch
      // TODO: this is a workaround for a Scala compiler bug that is not minimized yet.
      // It throws an exception which somehow disappears when we widen the types and run show.
      // Regression test is in platforms/src/test/scala/PlatformSpec.scala
      case e: Throwable =>
        lhsExactInfo.exactTpe.widen.show
        rhsExactInfo.exactTpe.widen.show
        Expr.summonOrError[ExactOp2[
          Op,
          Ctx,
          OutUB,
          lhsExactInfo.Underlying,
          rhsExactInfo.Underlying
        ]]
    end try
  end exactOp2ExprOrError
  exactOp2ExprOrError match
    case Right(expr) => '{
        $expr(${ lhsExactInfo.exactExpr }, ${ rhsExactInfo.exactExpr })(using $ctx)
      }
    case Left(msg) =>
      if (bothWays.value.getOrElse(false))
        Expr.summonOrError[ExactOp2[
          Op,
          Ctx,
          OutUB,
          rhsExactInfo.Underlying,
          lhsExactInfo.Underlying
        ]] match
          case Right(expr) => '{
              $expr(${ rhsExactInfo.exactExpr }, ${ lhsExactInfo.exactExpr })(using $ctx)
            }
          case Left(msg) =>
            ControlledMacroError.report("Unsupported argument types for this operation.")
      else
        ControlledMacroError.report("Unsupported argument types for this operation.")
  end match
end exactOp2Macro

/////////////////////////////////////////////////////////////////////////////////
// ExactOp3
/////////////////////////////////////////////////////////////////////////////////
trait ExactOp3[Op, Ctx, OutUB, LHS, MHS, RHS]:
  type Out <: OutUB
  def apply(lhs: LHS, mhs: MHS, rhs: RHS)(using Ctx): Out
type ExactOp3Aux[Op, Ctx, OutUB, LHS, MHS, RHS, O <: OutUB] =
  ExactOp3[Op, Ctx, OutUB, LHS, MHS, RHS] { type Out = O }
transparent inline def exactOp3[Op, Ctx, OutUB](
    inline lhs: Any,
    inline mhs: Any,
    inline rhs: Any
)(using ctx: Ctx): OutUB = ${ exactOp3Macro[Op, Ctx, OutUB]('lhs, 'mhs, 'rhs)('ctx) }
private def exactOp3Macro[Op, Ctx, OutUB](
    lhs: Expr[Any],
    mhs: Expr[Any],
    rhs: Expr[Any]
)(ctx: Expr[Ctx])(
    using
    Quotes,
    Type[Op],
    Type[Ctx],
    Type[OutUB]
): Expr[OutUB] =
  import quotes.reflect.*
  val lhsExactInfo = lhs.exactInfo
  val mhsExactInfo = mhs.exactInfo
  val rhsExactInfo = rhs.exactInfo
  Expr.summon[ExactOp3[
    Op,
    Ctx,
    OutUB,
    lhsExactInfo.Underlying,
    mhsExactInfo.Underlying,
    rhsExactInfo.Underlying
  ]] match
    case Some(expr) => '{
        $expr(
          ${ lhsExactInfo.exactExpr },
          ${ mhsExactInfo.exactExpr },
          ${ rhsExactInfo.exactExpr }
        )(using $ctx)
      }
    case None =>
      ControlledMacroError.report("Unsupported argument types for this operation.")
  end match
end exactOp3Macro
