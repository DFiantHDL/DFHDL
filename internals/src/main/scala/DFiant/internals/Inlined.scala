package DFiant.internals
import scala.quoted.*
import compiletime.ops.{int, string, boolean, any}
import compiletime.{constValue, constValueOpt}
export int.{+ => _, ^ => _, *}
export string.{+ => _, *}
export boolean.{^ => _, *}
export any.*
type +[L, R] = (L, R) match
  case (Int, Int)       => int.+[L, R]
  case (String, String) => string.+[L, R]
  case (String, Int)    => string.+[L, int.ToString[R]]
  case (Int, String)    => string.+[int.ToString[L], R]
type ^[L, R] = (L, R) match
  case (Int, Int)         => int.^[L, R]
  case (Boolean, Boolean) => boolean.^[L, R]

import scala.annotation.targetName

protected object std:
  type Int = scala.Int
  type Long = scala.Long
  type Float = scala.Float
  type Double = scala.Double
  type String = java.lang.String
  type Boolean = scala.Boolean

type XInt = Int with Singleton
type XLong = Long with Singleton
type XFloat = Float with Singleton
type XDouble = Double with Singleton
type XString = String with Singleton
type XBoolean = Boolean with Singleton

opaque type Inlined[T <: Wide, Wide] = T

object Inlined:
  extension [T <: Wide, Wide](inlined: Inlined[T, Wide]) def value: T = inlined
  transparent inline implicit def getValue[T <: Wide, Wide](
      inlined: Inlined[T, Wide]
  ): T =
    inline constValueOpt[T] match
      case Some(_) => constValue[T]
      case None    => inlined.asInstanceOf[T]
  inline implicit def fromValue[T <: Wide with Singleton, Wide](
      inline value: T
  ): Inlined[T, Wide] = value
  @targetName("fromValueWide")
  implicit def fromValue[Wide](
      value: Wide
  ): Inlined[Wide, Wide] = value

  protected trait Companion[Wide]:
    def forced[T <: Wide](value: Wide): Inlined[T, Wide] = value.asInstanceOf[T]
    def apply[T <: Wide with Singleton](
        value: T
    ): Inlined[T, Wide] = value

  type Int[T <: std.Int] = Inlined[T, std.Int]
  object Int extends Companion[std.Int]
  extension [T <: std.Int](lhs: Int[T])
    def +[R <: std.Int](rhs: Int[R]) =
      Int.forced[int.+[T, R]](lhs.value + rhs.value)
    def >[R <: std.Int](rhs: Int[R]) =
      Boolean.forced[T > R](lhs.value > rhs.value)
  // def >[R <: std.Int with Singleton](rhs: R) =
  // new Boolean[T > R](lhs.value > rhs)

  type Boolean[T <: std.Boolean] = Inlined[T, std.Boolean]
  object Boolean extends Companion[std.Boolean]

  type String[T <: std.String] = Inlined[T, std.String]
  object String extends Companion[std.String]

  inline def require(
      inline cond: std.Boolean,
      inline msg: std.String
  ): Unit = ${ requireMacro('cond, 'msg) }
end Inlined

def requireMacro(cond: Expr[Boolean], msg: Expr[String])(using
    Quotes
): Expr[Unit] =
  import quotes.reflect.*

  val inlinedTpe = TypeRepr.of[DFiant.internals.Inlined]
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
        val xArgsStr = xargs.map {
          case ValueExpr(v) => v.toString
          case _            => return expr
        }
        val yArgsStr = yargs.map {
          case ValueExpr(v) => v.toString
          case _            => return expr
        }
        StringContext.apply(xArgsStr: _*).s(yArgsStr: _*).toConstantExpr
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
