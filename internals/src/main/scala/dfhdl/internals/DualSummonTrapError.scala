package dfhdl.internals
import scala.quoted.*

trait DualSummonTrapError[L, R]:
  val valueL: Option[L]
  val valueR: Option[R]
object DualSummonTrapError:
  def apply[L, R](valueL0: Option[L], valueR0: Option[R]): DualSummonTrapError[L, R] =
    new DualSummonTrapError[L, R]:
      val valueL: Option[L] = valueL0
      val valueR: Option[R] = valueR0
  end apply
  transparent inline given [L, R]: DualSummonTrapError[L, R] = ${ fromMacroImpl[L, R] }
  def fromMacroImpl[L, R](using Quotes, Type[L], Type[R]): Expr[DualSummonTrapError[L, R]] =
    import quotes.reflect.*
    ControlledMacroError.activate()
    enum Result[T] derives CanEqual:
      case Success[T](value: Expr[T]) extends Result[T]
      case PriorityError[T](msg: String) extends Result[T]
      case NotFoundError[T](msg: String) extends Result[T]
    import Result.*
    def getResult[T](using Type[T]): Result[T] =
      Implicits.search(TypeRepr.of[T]) match
        case iss: ImplicitSearchSuccess =>
          Success(iss.tree.asExprOf[T])
        case isf: ImplicitSearchFailure =>
          val lastError = ControlledMacroError.getLastError
          if (lastError.nonEmpty) PriorityError[T](lastError)
          else NotFoundError[T](isf.explanation)
      end match
    end getResult
    val resultL = getResult[L]
    val resultR = getResult[R]
    ControlledMacroError.deactivate()
    (resultL, resultR) match
      case (Success(valueL), Success(valueR)) => '{ apply[L, R](Some($valueL), Some($valueR)) }
      case (Success(valueL), _)               => '{ apply[L, R](Some($valueL), None) }
      case (_, Success(valueR))               => '{ apply[L, R](None, Some($valueR)) }
      case (PriorityError(msgL), _)           => '{ compiletime.error(${ Expr(msgL) }) }
      case (_, PriorityError(msgR))           => '{ compiletime.error(${ Expr(msgR) }) }
      case (NotFoundError(msgL), _)           => '{ compiletime.error(${ Expr(msgL) }) }
    end match
  end fromMacroImpl
end DualSummonTrapError
