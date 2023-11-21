package dfhdl.internals
import scala.quoted.*
import compiletime.ops.{int, string, boolean, any}
import compiletime.{constValue, constValueOpt}
export int.{ToString => _, + => _, ^ => _, *}
export string.{+ => _, *}
export boolean.{^ => _, *}
export any.*
type +[L, R] = (L, R) match
  case (Int, Int)       => int.+[L, R]
  case (String, String) => string.+[L, R]
  case (String, Int)    => string.+[L, ToString[R]]
  case (Int, String)    => string.+[ToString[L], R]
type ^[L, R] = (L, R) match
  case (Int, Int)         => int.^[L, R]
  case (Boolean, Boolean) => boolean.^[L, R]
type ITE[I <: Boolean, T, E] <: T | E = I match
  case true  => T
  case false => E

import scala.annotation.targetName

type XInt = Int with Singleton
type XLong = Long with Singleton
type XFloat = Float with Singleton
type XDouble = Double with Singleton
type XString = String with Singleton
type XBoolean = Boolean with Singleton

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived

opaque type Inlined[T] = T
object Inlined:
  given [L, R](using CanEqual[L, R]): CanEqual[Inlined[L], Inlined[R]] =
    CanEqual.derived
  given [T <: Int]: CanEqual[Inlined[T], Int] = CanEqual.derived
  given [T <: String]: CanEqual[Inlined[T], String] = CanEqual.derived
  given [T <: Boolean]: CanEqual[Inlined[T], Boolean] = CanEqual.derived
  extension [T](inlined: Inlined[T]) def value: T = inlined
  transparent inline implicit def getValue[T](
      inlined: Inlined[T]
  ): T =
    inline constValueOpt[T] match
      case Some(_) => constValue[T]
      case None    => inlined.asInstanceOf[T]
  // TODO: without @precise it's impossible to define a proper precise `Conversion`
  // as shown in https://github.com/lampepfl/dotty/pull/16499
  inline implicit def fromValue[T <: Singleton](value: T): Inlined[T] = value
  @targetName("fromValueWide")
  inline implicit def fromValue[Wide](value: Wide): Inlined[Wide] = value

  inline def forced[T](value: Any): Inlined[T] = value.asInstanceOf[T]
  inline def apply[T <: Singleton](value: T): Inlined[T] = value

  extension [T <: Int](lhs: Inlined[T])
    inline def widen: Inlined[Int] = forced[Int](lhs.value)
    inline def +[R <: Int](rhs: Inlined[R]) =
      forced[int.+[T, R]](lhs.value + rhs.value)
    inline def -[R <: Int](rhs: Inlined[R]) =
      forced[int.-[T, R]](lhs.value - rhs.value)
    inline def *[R <: Int](rhs: Inlined[R]) =
      forced[int.*[T, R]](lhs.value * rhs.value)
    inline def /[R <: Int](rhs: Inlined[R]) =
      forced[int./[T, R]](lhs.value / rhs.value)
    inline def >[R <: Int](rhs: Inlined[R]) =
      forced[int.>[T, R]](lhs.value > rhs.value)
    inline def <[R <: Int](rhs: Inlined[R]) =
      forced[int.<[T, R]](lhs.value < rhs.value)
    inline def >=[R <: Int](rhs: Inlined[R]) =
      forced[int.>=[T, R]](lhs.value >= rhs.value)
    inline def <=[R <: Int](rhs: Inlined[R]) =
      forced[int.<=[T, R]](lhs.value <= rhs.value)
    inline def ==[R <: Int](rhs: Inlined[R]) =
      forced[any.==[T, R]](lhs.value == rhs.value)
    inline def !=[R <: Int](rhs: Inlined[R]) =
      forced[any.!=[T, R]](lhs.value != rhs.value)
    inline def max[R <: Int](rhs: Inlined[R]) =
      forced[int.Max[T, R]](math.max(lhs.value, rhs.value))
    inline def min[R <: Int](rhs: Inlined[R]) =
      forced[int.Min[T, R]](math.min(lhs.value, rhs.value))
  end extension

  extension [T <: String](lhs: Inlined[T])
    inline def widen: Inlined[String] = forced[String](lhs.value)
    inline def +[R <: String](rhs: Inlined[R]) =
      forced[string.+[T, R]](lhs.value + rhs.value)
    inline def ==[R <: String](rhs: Inlined[R]) =
      forced[any.==[T, R]](lhs.value == rhs.value)
    inline def !=[R <: String](rhs: Inlined[R]) =
      forced[any.!=[T, R]](lhs.value != rhs.value)
  end extension

  extension [T <: Boolean](lhs: Inlined[T])
    inline def widen: Inlined[Boolean] = forced[Boolean](lhs.value)
    inline def ==[R <: Boolean](rhs: Inlined[R]) =
      forced[any.==[T, R]](lhs.value == rhs.value)
    inline def !=[R <: Boolean](rhs: Inlined[R]) =
      forced[any.!=[T, R]](lhs.value != rhs.value)
  end extension

  inline def require(
      inline cond: Boolean,
      inline msg: String
  ): Unit = ${ requireMacro('cond, 'msg) }
end Inlined

def requireMacro(cond: Expr[Boolean], msg: Expr[String])(using
    Quotes
): Expr[Unit] =
  import quotes.reflect.*

  val inlinedTpe = TypeRepr.of[dfhdl.internals.Inlined]
  extension (str: String)
    def toConstantExpr: Expr[?] =
      Literal(StringConstant(str)).asExpr
  object ValueExpr:
    def unapply[T](expr: Expr[T]): Option[T] =
      expr.asTerm.tpe match
        case ConstantType(const) => Some(const.value).asInstanceOf[Option[T]]
        case t: AppliedType if t.tycon <:< inlinedTpe =>
          t.args.head match
            case ConstantType(const) =>
              Some(const.value).asInstanceOf[Option[T]]
            case _ => None
        case t =>
          inliner(expr).asTerm match
            case Literal(const) => Some(const.value).asInstanceOf[Option[T]]
            case quotes.reflect.Inlined(_, _, Literal(const)) =>
              Some(const.value).asInstanceOf[Option[T]]
            case _ => None
  end ValueExpr

  def inliner(expr: Expr[?]): Expr[?] =
    expr match
      case '{ (${ ValueExpr(l) }: String) + ${ ValueExpr(r) } } =>
        s"$l$r".toConstantExpr
      case '{ (${ ValueExpr(l) }: Any).toString } =>
        l.toString.toConstantExpr
      case '{ (${ ValueExpr(l) }: String).length } =>
        Literal(IntConstant(l.length)).asExpr
      case '{ (${ ValueExpr(l) }: String).stripMargin } =>
        l.stripMargin.toConstantExpr
      case '{ (${ ValueExpr(l) }: String).stripMargin(${ ValueExpr(r) }) } =>
        l.stripMargin(r).toConstantExpr
      case '{
            StringContext
              .apply(${ Varargs(xargs) }: _*)
              .s(${ Varargs(yargs) }: _*)
          } =>
        var skip = false
        val xArgsStr = xargs.map {
          case ValueExpr(v) => v.toString
          case _ =>
            skip = true
            ""
        }
        val yArgsStr = yargs.map {
          case ValueExpr(v) => v.toString
          case _ =>
            skip = true
            ""
        }
        if (skip) expr
        else StringContext.apply(xArgsStr: _*).s(yArgsStr: _*).toConstantExpr
      case _ => expr

  cond match
    case ValueExpr(condBool) =>
      if (!condBool) msg match
        case ValueExpr(msgStr) =>
          report.error(msgStr)
          '{}
        case _ => '{ throw new IllegalArgumentException($msg) }
      else '{}
    case _ => '{ throw new IllegalArgumentException($msg) }
end requireMacro
