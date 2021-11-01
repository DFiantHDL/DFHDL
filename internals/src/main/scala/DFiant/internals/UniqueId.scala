package DFiant.internals
import scala.quoted.*

trait UniqueId:
  type Out <: Int
object UniqueId:
  private var uniqueId = 0
  transparent inline given UniqueId = ${ uniqueMacro }
  def uniqueMacro(using Quotes): Expr[UniqueId] =
    import quotes.reflect.*
    val tp = ConstantType(IntConstant(uniqueId)).asTypeOf[Int]
    uniqueId += 1
    '{
      new UniqueId:
        type Out = tp.Underlying
    }
