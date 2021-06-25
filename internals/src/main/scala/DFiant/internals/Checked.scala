package DFiant.internals
import scala.quoted.*

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
        lazy val funcTermParts =
          func.args.map(a => lambdaTypeToTermRecur(a, argTerm, argTypeParam))
        lazy val arg0 = funcTermParts(0)
        lazy val arg1 = funcTermParts(1)
        val expr = func.tycon match
          case t if t =:= TypeRepr.of[any.==] =>
            '{ ${ arg0.asExprOf[Any] } == ${ arg1.asExprOf[Any] } }
          case t if t =:= TypeRepr.of[any.!=] =>
            '{ ${ arg0.asExprOf[Any] } != ${ arg1.asExprOf[Any] } }
          case t if t =:= TypeRepr.of[boolean.!] =>
            '{ !${ arg0.asExprOf[Boolean] } }
          case t if t =:= TypeRepr.of[boolean.&&] =>
            '{ ${ arg0.asExprOf[Boolean] } && ${ arg1.asExprOf[Boolean] } }
          case t if t =:= TypeRepr.of[boolean.||] =>
            '{ ${ arg0.asExprOf[Boolean] } || ${ arg1.asExprOf[Boolean] } }
          case t if t =:= TypeRepr.of[boolean.^] =>
            '{ ${ arg0.asExprOf[Boolean] } ^ ${ arg1.asExprOf[Boolean] } }
          case t if t =:= TypeRepr.of[int.+] =>
            '{ ${ arg0.asExprOf[Int] } + ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.-] =>
            '{ ${ arg0.asExprOf[Int] } - ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.`*`] =>
            '{ ${ arg0.asExprOf[Int] } * ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int./] =>
            '{ ${ arg0.asExprOf[Int] } / ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.%] =>
            '{ ${ arg0.asExprOf[Int] } % ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.<<] =>
            '{ ${ arg0.asExprOf[Int] } << ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.>>] =>
            '{ ${ arg0.asExprOf[Int] } >> ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.>>>] =>
            '{ ${ arg0.asExprOf[Int] } >>> ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.>] =>
            '{ ${ arg0.asExprOf[Int] } > ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.<] =>
            '{ ${ arg0.asExprOf[Int] } < ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.>=] =>
            '{ ${ arg0.asExprOf[Int] } >= ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.<=] =>
            '{ ${ arg0.asExprOf[Int] } <= ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.BitwiseAnd] =>
            '{ ${ arg0.asExprOf[Int] } & ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.BitwiseOr] =>
            '{ ${ arg0.asExprOf[Int] } | ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.^] =>
            '{ ${ arg0.asExprOf[Int] } ^ ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.Max] =>
            '{ ${ arg0.asExprOf[Int] } max ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.Min] =>
            '{ ${ arg0.asExprOf[Int] } min ${ arg1.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.Negate] =>
            '{ -${ arg0.asExprOf[Int] } }
          case t if t =:= TypeRepr.of[int.ToString] =>
            '{ ${ arg0.asExprOf[Int] }.toString }
          case t if t =:= TypeRepr.of[int.Abs] =>
            '{ ${ arg0.asExprOf[Int] }.abs }
          case t if t =:= TypeRepr.of[int.S] =>
            '{ ${ arg0.asExprOf[Int] } + 1 }
          case t if t =:= TypeRepr.of[string.+] =>
            '{ ${ arg0.asExprOf[String] } + ${ arg1.asExprOf[String] } }
          case t =>
            report.error(s"Unsupported type function ${t.show}")
            '{ ??? }
        expr.asTerm
      case t =>
        report.error(s"Unsupported type function part ${t.show}")
        '{ ??? }.asTerm
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
              report.error(msg)
              '{ ??? }
            case _ =>
              '{ throw new IllegalArgumentException($msgExpr) }
      case _ =>
        '{ if (! $condExpr) throw new IllegalArgumentException($msgExpr) }

trait Check1[
    Wide,
    Cond[T <: Wide] <: Boolean,
    Msg[T <: Wide] <: String
]:
  type Check[T <: Wide] = Check1.Check[Wide, T, Cond, Msg, Cond[T], Msg[T]]

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
