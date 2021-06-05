package DFiant.internals
import scala.compiletime.{constValueOpt, error, constValue}
import scala.quoted.*
import javax.swing.text.StringContent

inline def requireMsg(
    inline cond: Boolean,
    inline msg: String
): Unit = ${ requireMsgMacro('cond, 'msg) }

def requireMsgMacro(cond: Expr[Boolean], msg: Expr[String])(using
    Quotes
): Expr[Unit] =
  import quotes.reflect.*

  val inlinedTpe = TypeRepr.of[DFiant.internals.Inlined[_, _]]
  extension (str: String)
    def toConstantExpr: Expr[?] =
      Literal(StringConstant(str)).asExpr
  object ValueExpr:
    def unapply[T](expr: Expr[T]): Option[T] =
      expr.asTerm.tpe match
        case ConstantType(const) => Some(const.value).asInstanceOf[Option[T]]
        case t: AppliedType if t <:< inlinedTpe =>
          t.args.head match
            case ConstantType(const) =>
              Some(const.value).asInstanceOf[Option[T]]
            case _ => None
        case t =>
          inliner(expr).asTerm match
            case Literal(const) => Some(const.value).asInstanceOf[Option[T]]
            case Inlined(_, _, Literal(const)) =>
              Some(const.value).asInstanceOf[Option[T]]
            case _ => None

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
