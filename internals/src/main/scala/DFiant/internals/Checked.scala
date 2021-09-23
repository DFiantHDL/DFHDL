package DFiant.internals
import scala.quoted.*

private val funcRealNameMap: Map[String, String] = Map(
  "BitwiseAnd" -> "&",
  "BitwiseOr" -> "|",
  "ToString" -> "toString",
  "Abs" -> "abs"
)
private class MacroClass[Q <: Quotes](using val quotes: Q)(
    condTpe: quotes.reflect.TypeRepr,
    msgTpe: quotes.reflect.TypeRepr,
    condValueTpe: quotes.reflect.TypeRepr,
    msgValueTpe: quotes.reflect.TypeRepr
):
  import quotes.reflect.*
  def lambdaTypeToTermRecur(
      tpe: TypeRepr,
      argTerm: List[Term],
      argTypeParam: List[TypeRepr]
  ): Term =
    import compiletime.ops.{int, string, any, boolean}
    tpe match
      case ConstantType(const) => Literal(const)
      case t if argTypeParam.indexOf(t) >= 0 =>
        argTerm(argTypeParam.indexOf(t))
      case func: AppliedType =>
        val funcTermParts =
          func.args.map(a => lambdaTypeToTermRecur(a, argTerm, argTypeParam))
        val arg0 = funcTermParts(0)
        lazy val arg1 = funcTermParts(1)
        lazy val arg2 = funcTermParts(2)
        val funcName = func.tycon.typeSymbol.name.toString
        val expr = funcName match
          case "Max" =>
            '{ ${ arg0.asExprOf[Int] } max ${ arg1.asExprOf[Int] } }
          case "Min" =>
            '{ ${ arg0.asExprOf[Int] } min ${ arg1.asExprOf[Int] } }
          case "S" =>
            '{ ${ arg0.asExprOf[Int] } + 1 }
          case "!" =>
            '{ !${ arg0.asExprOf[Boolean] } }
          case "Negate" =>
            '{ -${ arg0.asExprOf[Int] } }
          case "ITE" => //if-then-else
            '{
              if (${ arg0.asExprOf[Boolean] }) ${ arg1.asExpr }
              else ${ arg2.asExpr }
            }
          case _ =>
            val realFuncName = funcRealNameMap.getOrElse(funcName, funcName)
            Select
              .overloaded(arg0, realFuncName, Nil, funcTermParts.drop(1))
              .asExpr
        expr.asTerm
      case t =>
        report.errorAndAbort(s"Unsupported type function part ${t.show}")
        '{ ??? }.asTerm
    end match
  end lambdaTypeToTermRecur

  def lambdaTypeToTerm(argsExpr: List[Term], tpe: TypeRepr): Term =
    tpe match
      case lambda @ TypeLambda(names, _, tpe) =>
        lambdaTypeToTermRecur(
          tpe,
          argsExpr,
          argsExpr.zipWithIndex.map { case (_, i) => lambda.param(i) }
        )

  def applyExpr(argsTerm: List[Term]): Expr[Unit] =
    def condExpr = lambdaTypeToTerm(argsTerm, condTpe)
      .asExprOf[Boolean]
    def msgExpr = lambdaTypeToTerm(argsTerm, msgTpe)
      .asExprOf[String]
    condValueTpe match
      case ConstantType(BooleanConstant(cond)) =>
        if (cond)
          '{} //the condition is satisfied, hence the apply method does nothing
        else
          msgValueTpe match
            case ConstantType(StringConstant(msg)) =>
              '{ compiletime.error(${ Expr(msg) }) }
            case _ =>
              '{ throw new IllegalArgumentException($msgExpr) }
      case _ =>
        '{ if (! $condExpr) throw new IllegalArgumentException($msgExpr) }
  end applyExpr
end MacroClass

trait Check1[
    Wide,
    Cond[T <: Wide] <: Boolean,
    Msg[T <: Wide] <: String
]:
  type Check[T <: Wide] = Check1.Check[Wide, T, Cond, Msg, Cond[T], Msg[T]]
  inline def apply(arg: Wide): Unit =
    compiletime.summonInline[Check[Wide]]

object Check1:
  trait Check[
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ]:
    def apply(arg: Wide): Unit
  inline given [
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ]: Check[Wide, T, Cond, Msg, CondValue, MsgValue] =
    ${ checkMacro[Wide, T, Cond, Msg, CondValue, MsgValue] }

  final def checkMacro[
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ](using
      Quotes,
      Type[Wide],
      Type[T],
      Type[Cond],
      Type[Msg],
      Type[CondValue],
      Type[MsgValue]
  ): Expr[Check[Wide, T, Cond, Msg, CondValue, MsgValue]] =
    import quotes.reflect.*
    val condTpe = TypeRepr.of[Cond]
    val msgTpe = TypeRepr.of[Msg]
    val condValueTpe = TypeRepr.of[CondValue]
    val msgValueTpe = TypeRepr.of[MsgValue]
    val mc =
      new MacroClass[quotes.type](using quotes)(
        condTpe,
        msgTpe,
        condValueTpe,
        msgValueTpe
      )
    '{
      new Check[Wide, T, Cond, Msg, CondValue, MsgValue]:
        def apply(arg: Wide): Unit = ${ mc.applyExpr(List('arg.asTerm)) }
    }
  end checkMacro
end Check1

trait Check2[
    Wide1,
    Wide2,
    Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
    Msg[T1 <: Wide1, T2 <: Wide2] <: String
]:
  type Check[T1 <: Wide1, T2 <: Wide2] =
    Check2.Check[Wide1, Wide2, T1, T2, Cond, Msg, Cond[T1, T2], Msg[T1, T2]]
  inline def apply(arg1: Wide1, arg2: Wide2): Unit =
    compiletime.summonInline[Check[Wide1, Wide2]]

object Check2:
  trait Check[
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ]:
    def apply(arg1: Wide1, arg2: Wide2): Unit
  inline given [
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ]: Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue] =
    ${ checkMacro[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue] }

  final def checkMacro[
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String
  ](using
      Quotes,
      Type[Wide1],
      Type[Wide2],
      Type[T1],
      Type[T2],
      Type[Cond],
      Type[Msg],
      Type[CondValue],
      Type[MsgValue]
  ): Expr[Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue]] =
    import quotes.reflect.*
    val condTpe = TypeRepr.of[Cond]
    val msgTpe = TypeRepr.of[Msg]
    val condValueTpe = TypeRepr.of[CondValue]
    val msgValueTpe = TypeRepr.of[MsgValue]
    val mc =
      new MacroClass[quotes.type](using quotes)(
        condTpe,
        msgTpe,
        condValueTpe,
        msgValueTpe
      )
    '{
      new Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue]:
        def apply(arg1: Wide1, arg2: Wide2): Unit = ${
          mc.applyExpr(List('arg1.asTerm, 'arg2.asTerm))
        }
    }
  end checkMacro
end Check2
