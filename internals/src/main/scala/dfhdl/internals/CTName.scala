package dfhdl.internals
import scala.quoted.*

//returns empty string for anonymous names
trait CTName:
  type Out <: String
  val value: Out
object CTName:
  transparent inline given CTName = ${ getName }
  def apply[N <: String](name: N): CTName { type Out = N } = new CTName:
    type Out = N
    val value: Out = name
  def getName(using Quotes): Expr[CTName] =
    import quotes.reflect.*
    val nameStr = Symbol.spliceOwner.owner.name.toString
    val nameFix = if (nameStr.startsWith("<")) "" else nameStr
    val nameConst = StringConstant(nameFix)
    val nameType = ConstantType(nameConst).asTypeOf[String]
    val nameExpr = Literal(nameConst).asExprOf[String]
    '{ CTName[nameType.Underlying]($nameExpr) }
end CTName
