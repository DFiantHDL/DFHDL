package dfhdl.internals
import scala.quoted.*

//returns empty string for anonymous names
trait CTName:
  type Out <: String
  val value: Out
object CTName:
  transparent inline given CTName = ${ getName }
  def getName(using Quotes): Expr[CTName] =
    import quotes.reflect.*
    val nameStr = Symbol.spliceOwner.owner.name.toString
    val nameFix = if (nameStr.startsWith("<")) "" else nameStr
    val nameConst = StringConstant(nameFix)
    val nameTpe = ConstantType(nameConst).asTypeOf[String]
    val nameExpr = Literal(nameConst).asExprOf[String]
    '{
      new CTName:
        type Out = nameTpe.Underlying
        val value: Out = ${ nameExpr }
    }
