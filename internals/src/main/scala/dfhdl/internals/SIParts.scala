package dfhdl.internals
import scala.quoted.*

// P is a tuple representation of the SC parts
trait SIParts[P <: Tuple](parts: P)

object SIParts:
  def scMacro[SI[_ <: Tuple]](sc: Expr[StringContext])(using
      Quotes,
      Type[SI]
  ): Expr[Any] =
    import quotes.reflect.*
    val '{ StringContext(${ Varargs(args) }*) } = sc
    val tplExpr = Expr.ofTupleFromSeq(args)
    val tplTpe = tplExpr.asTerm.tpe
    val tplType = tplTpe.asTypeOf[Tuple]
    val AppliedType(siTpe, _) = TypeRepr.of[SI[Tuple]]
    val siSym = siTpe.typeSymbol
    val siTree =
      New(siTpe.asTypeTree)
        .select(siSym.primaryConstructor)
        .appliedToType(tplTpe)
        .appliedTo(tplExpr.asTerm)
    siTree.asExpr
  end scMacro
  def tupleToExprs[P <: Tuple](parts: Expr[P])(using
      Quotes,
      Type[P]
  ): Seq[Expr[Any]] =
    import quotes.reflect.*
    TypeRepr.of[P].getTupleArgs.zipWithIndex.map {
      case (ConstantType(c), _) => Literal(c).asExpr
      case (t, i) =>
        val tType = t.asTypeOf[Any]
        '{
          ${ parts }.productElement(${ Expr(i) }).asInstanceOf[tType.Underlying]
        }
    }
  end tupleToExprs
end SIParts
