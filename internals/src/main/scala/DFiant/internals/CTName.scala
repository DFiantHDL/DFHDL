package DFiant.internals
import scala.quoted.*

trait CTName:
  type Out <: String
  val value: Out
object CTName:
  transparent inline given CTName = ${ getName }
  def getName(using Quotes): Expr[CTName] =
    import quotes.reflect.*
    val nameConst = StringConstant(Symbol.spliceOwner.owner.name.toString)
    val nameTpe = ConstantType(nameConst).asTypeOf[String]
    val nameExpr = Literal(nameConst).asExprOf[String]
    '{
      new CTName:
        type Out = nameTpe.Underlying
        val value: Out = ${ nameExpr }
    }
