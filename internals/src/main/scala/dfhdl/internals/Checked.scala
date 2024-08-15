package dfhdl.internals
import scala.quoted.*

private val funcRealNameMap: Map[String, String] = Map(
  "BitwiseAnd" -> "&",
  "BitwiseOr" -> "|",
  "ToString" -> "toString",
  "Abs" -> "abs",
  "+" -> "+",
  "-" -> "-",
  "*" -> "*",
  "/" -> "/",
  "%" -> "%",
  ">>" -> ">>",
  "<<" -> "<<",
  ">>>" -> ">>>",
  ">" -> ">",
  "<" -> "<",
  "^" -> "^",
  "&&" -> "&&",
  "||" -> "||",
  ">=" -> ">=",
  "<=" -> "<=",
  "==" -> "==",
  "!=" -> "!="
)
private class MacroClass[Q <: Quotes](using val quotes: Q)(
    condTpe: quotes.reflect.TypeRepr,
    msgTpe: quotes.reflect.TypeRepr,
    condValueTpe: quotes.reflect.TypeRepr,
    msgValueTpe: quotes.reflect.TypeRepr,
    warn: Boolean
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
          case "ITE" => // if-then-else
            '{
              if (${ arg0.asExprOf[Boolean] }) ${ arg1.asExpr }
              else ${ arg2.asExpr }
            }
          case _ =>
            funcRealNameMap.get(funcName) match
              case Some(realFuncName) =>
                Select
                  .overloaded(arg0, realFuncName, Nil, funcTermParts.drop(1))
                  .asExpr
              case None =>
                val da = tpe.dealias
                if (da =:= tpe)
                  lambdaTypeToTermRecur(da, argTerm, argTypeParam).asExpr
                else
                  report.errorAndAbort(
                    s"Unsupported type function part ${tpe.show}"
                  )
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

  val condOpt: Option[Boolean] = condValueTpe.dealias match
    case ConstantType(BooleanConstant(cond)) => Some(cond)
    case _                                   => None

  def applyExpr(argsTerm: List[Term]): Expr[Unit] =
    def condExpr = lambdaTypeToTerm(argsTerm, condTpe)
      .asExprOf[Boolean]
    def msgExpr = lambdaTypeToTerm(argsTerm, msgTpe)
      .asExprOf[String]
    condOpt match
      case Some(cond) =>
        if (cond)
          '{} // the condition is satisfied, hence the apply method does nothing
        else
          msgValueTpe.dealias match
            case ConstantType(StringConstant(msg)) =>
              if (warn)
                report.warning(msg)
                '{}
              else '{ compiletime.error(${ Expr(msg) }) }
            case _ =>
              if (warn)
                '{ println($msgExpr) }
              else
                '{ throw new IllegalArgumentException($msgExpr) }
      case _ =>
        if (warn)
          '{ if (! $condExpr) println($msgExpr) }
        else
          '{ if (! $condExpr) throw new IllegalArgumentException($msgExpr) }
    end match
  end applyExpr
end MacroClass

trait Check1[
    Wide,
    Cond[T <: Wide] <: Boolean,
    Msg[T <: Wide] <: String
]:
  type Check[T <: Wide] = Check1.Check[Wide, T, Cond, Msg, Cond[T], Msg[T], false]
  type CheckNUB[T] = Check1.CheckNUB[Wide, T, Cond, Msg, false]
  type Warn[T <: Wide] = Check1.Check[Wide, T, Cond, Msg, Cond[T], Msg[T], true]
  type WarnNUB[T] = Check1.CheckNUB[Wide, T, Cond, Msg, true]
  inline def apply(arg: Wide): Unit = compiletime.summonInline[Check[Wide]]
end Check1

trait UBound[UB, T]:
  type Out <: UB
protected sealed trait UBoundLP:
  given [UB, C]: UBound[UB, C] with
    type Out = UB
object UBound extends UBoundLP:
  type Aux[UB, T, O <: UB] = UBound[UB, T] { type Out = O }
  given [UB, T <: UB]: UBound[UB, T] with
    type Out = T

object Check1:
  trait CheckNUB[
      Wide,
      T,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      Warn <: Boolean
  ]:
    def apply(arg: Wide): Unit

  object CheckNubOK extends CheckNUB[Any, Any, Nothing, Nothing, Boolean]:
    def apply(arg: Any): Unit = {}

  protected trait CheckNUBLP:
    given [
        Wide,
        T,
        TUB <: Wide,
        Cond[T <: Wide] <: Boolean,
        Msg[T <: Wide] <: String,
        Warn <: Boolean
    ](using
        ub: UBound.Aux[Wide, T, TUB],
        check: Check[Wide, TUB, Cond, Msg, Cond[TUB], Msg[TUB], Warn]
    ): CheckNUB[Wide, T, Cond, Msg, Warn] with
      def apply(arg: Wide): Unit = check(arg)

  object CheckNUB extends CheckNUBLP:
    inline given ok[
        Wide,
        T,
        TUB <: Wide,
        Cond[T <: Wide] <: Boolean,
        Msg[T <: Wide] <: String,
        Warn <: Boolean
    ](using inline ub: UBound.Aux[Wide, T, TUB])(using
        inline check: Cond[TUB] =:= true
    ): CheckNUB[Wide, T, Cond, Msg, Warn] =
      CheckNubOK.asInstanceOf[CheckNUB[Wide, T, Cond, Msg, Warn]]

  trait Check[
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ]:
    def apply(arg: Wide): Unit

  object CheckOK extends Check[Any, Any, Nothing, Nothing, Boolean, String, Boolean]:
    def apply(arg: Any): Unit = {}

  inline given [
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ]: Check[Wide, T, Cond, Msg, CondValue, MsgValue, Warn] =
    ${ checkMacro[Wide, T, Cond, Msg, CondValue, MsgValue, Warn] }

  final def checkMacro[
      Wide,
      T <: Wide,
      Cond[T <: Wide] <: Boolean,
      Msg[T <: Wide] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ](using
      Quotes,
      Type[Wide],
      Type[T],
      Type[Cond],
      Type[Msg],
      Type[CondValue],
      Type[MsgValue],
      Type[Warn]
  ): Expr[Check[Wide, T, Cond, Msg, CondValue, MsgValue, Warn]] =
    import quotes.reflect.*
    val condTpe = TypeRepr.of[Cond]
    val msgTpe = TypeRepr.of[Msg]
    val condValueTpe = TypeRepr.of[CondValue]
    val msgValueTpe = TypeRepr.of[MsgValue]
    val ConstantType(BooleanConstant(warn)) = TypeRepr.of[Warn]: @unchecked
    val mc =
      new MacroClass[quotes.type](using quotes)(
        condTpe, msgTpe, condValueTpe, msgValueTpe, warn
      )
    if (mc.condOpt == Some(true))
      '{ CheckOK.asInstanceOf[Check[Wide, T, Cond, Msg, CondValue, MsgValue, Warn]] }
    else
      '{
        new Check[Wide, T, Cond, Msg, CondValue, MsgValue, Warn]:
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
    Check2.Check[Wide1, Wide2, T1, T2, Cond, Msg, Cond[T1, T2], Msg[T1, T2], false]
  type CheckNUB[T1, T2] =
    Check2.CheckNUB[Wide1, Wide2, T1, T2, Cond, Msg, false]
  type Warn[T1 <: Wide1, T2 <: Wide2] =
    Check2.Check[Wide1, Wide2, T1, T2, Cond, Msg, Cond[T1, T2], Msg[T1, T2], true]
  type WarnNUB[T1, T2] =
    Check2.CheckNUB[Wide1, Wide2, T1, T2, Cond, Msg, true]
  inline def apply(arg1: Wide1, arg2: Wide2): Unit =
    compiletime.summonInline[Check[Wide1, Wide2]]
end Check2

object Check2:
  trait CheckNUB[
      Wide1,
      Wide2,
      T1,
      T2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      Warn <: Boolean
  ]:
    def apply(arg1: Wide1, arg2: Wide2): Unit

  object CheckNUBOK extends CheckNUB[Any, Any, Any, Any, Nothing, Nothing, Boolean]:
    def apply(arg1: Any, arg2: Any): Unit = {}

  protected trait CheckNUBLP:
    given [
        Wide1,
        Wide2,
        T1,
        T2,
        TUB1 <: Wide1,
        TUB2 <: Wide2,
        Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
        Msg[T1 <: Wide1, T2 <: Wide2] <: String,
        Warn <: Boolean
    ](using
        ub1: UBound.Aux[Wide1, T1, TUB1],
        ub2: UBound.Aux[Wide2, T2, TUB2],
        check: Check[Wide1, Wide2, TUB1, TUB2, Cond, Msg, Cond[TUB1, TUB2], Msg[TUB1, TUB2], Warn]
    ): CheckNUB[Wide1, Wide2, T1, T2, Cond, Msg, Warn] with
      def apply(arg1: Wide1, arg2: Wide2): Unit = check(arg1, arg2)
    end given
  end CheckNUBLP

  object CheckNUB extends CheckNUBLP:
    inline given ok[
        Wide1,
        Wide2,
        T1,
        T2,
        TUB1 <: Wide1,
        TUB2 <: Wide2,
        Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
        Msg[T1 <: Wide1, T2 <: Wide2] <: String,
        Warn <: Boolean
    ](using inline ub1: UBound.Aux[Wide1, T1, TUB1], inline ub2: UBound.Aux[Wide2, T2, TUB2])(using
        inline check: Cond[TUB1, TUB2] =:= true
    ): CheckNUB[Wide1, Wide2, T1, T2, Cond, Msg, Warn] =
      CheckNUBOK.asInstanceOf[CheckNUB[Wide1, Wide2, T1, T2, Cond, Msg, Warn]]
  end CheckNUB

  trait Check[
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ]:
    def apply(arg1: Wide1, arg2: Wide2): Unit

  object CheckOK extends Check[Any, Any, Any, Any, Nothing, Nothing, Boolean, String, Boolean]:
    def apply(arg1: Any, arg2: Any): Unit = {}

  inline given [
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ]: Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue, Warn] =
    ${ checkMacro[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue, Warn] }

  final def checkMacro[
      Wide1,
      Wide2,
      T1 <: Wide1,
      T2 <: Wide2,
      Cond[T1 <: Wide1, T2 <: Wide2] <: Boolean,
      Msg[T1 <: Wide1, T2 <: Wide2] <: String,
      CondValue <: Boolean,
      MsgValue <: String,
      Warn <: Boolean
  ](using
      Quotes,
      Type[Wide1],
      Type[Wide2],
      Type[T1],
      Type[T2],
      Type[Cond],
      Type[Msg],
      Type[CondValue],
      Type[MsgValue],
      Type[Warn]
  ): Expr[Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue, Warn]] =
    import quotes.reflect.*
    val condTpe = TypeRepr.of[Cond]
    val msgTpe = TypeRepr.of[Msg]
    val condValueTpe = TypeRepr.of[CondValue]
    val msgValueTpe = TypeRepr.of[MsgValue]
    val ConstantType(BooleanConstant(warn)) = TypeRepr.of[Warn]: @unchecked
    val mc =
      new MacroClass[quotes.type](using quotes)(
        condTpe, msgTpe, condValueTpe, msgValueTpe, warn
      )
    if (mc.condOpt == Some(true))
      '{ CheckOK.asInstanceOf[Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue, Warn]] }
    else
      '{
        new Check[Wide1, Wide2, T1, T2, Cond, Msg, CondValue, MsgValue, Warn]:
          def apply(arg1: Wide1, arg2: Wide2): Unit = ${
            mc.applyExpr(List('arg1.asTerm, 'arg2.asTerm))
          }
      }
  end checkMacro
end Check2
