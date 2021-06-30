package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*

opaque type DFTuple[T] <: DFType.Of[ir.DFTuple] = DFType.Of[ir.DFTuple]
extension [T](dfType: DFTuple[T])
  def fieldList: List[DFType] = dfType.asIR.fieldList.asInstanceOf[List[DFType]]
object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val fieldList: List[ir.DFType] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        //TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(x => DFType(x).asIR)
    ir.DFTuple(fieldList).asInstanceOf[DFTuple[T]]

  type Token[T] = DFToken.Of[DFTuple[T]]
  object Token:
    protected[core] def apply[T](
        dfType: DFTuple[T],
        data: List[DFToken]
    ): Token[T] =
      ir.DFToken(dfType.asIR, data).asInstanceOf[Token[T]]

    trait Creator[T, V <: NonEmptyTuple]:
      def apply(
          fieldList: List[DFType],
          tokenTupleValues: List[Any]
      ): List[DFToken]
    object Creator:
      inline given [T, V <: NonEmptyTuple]: Creator[T, V] = ${
        createMacro[T, V]
      }
      import DFType.TC.MacroOps.*
      def createMacro[T, V <: NonEmptyTuple](using
          Quotes,
          Type[T],
          Type[V]
      ): Expr[Creator[T, V]] =
        def applyExpr[T, V](
            fieldListExpr: Expr[List[DFType]],
            tokenTupleValuesExpr: Expr[List[Any]]
        )(using Quotes, Type[T], Type[V]): Expr[List[DFToken]] =
          import quotes.reflect.*
          val AppliedType(fun, tArgs) = TypeRepr.of[T]
          val AppliedType(_, vArgs) = TypeRepr.of[V]
          if (tArgs.length == vArgs.length)
            val exprs =
              tArgs.zipWithIndex.lazyZip(vArgs).map { case ((t, i), v) =>
                val vTpe = v.asTypeOf[Any]
                val dfTypeTpe = t.dfTypeTpe.get.asTypeOf[DFType]
                val iExpr = Literal(IntConstant(i)).asExprOf[Int]
                '{
                  val tc = compiletime
                    .summonInline[
                      DFToken.TC[dfTypeTpe.Underlying, vTpe.Underlying]
                    ]
                  val dfType =
                    $fieldListExpr
                      .apply($iExpr)
                      .asInstanceOf[dfTypeTpe.Underlying]
                  val value =
                    $tokenTupleValuesExpr
                      .apply($iExpr)
                      .asInstanceOf[vTpe.Underlying]
                  tc.apply(dfType, value)
                }
              }
            '{ List(${ Varargs(exprs) }*) }
          else
            report.error(
              s"DFType tuple length (${tArgs.length}) and token value tuple length (${vArgs.length}) do not match."
            )
            '{ ??? }
        import quotes.reflect.*
        '{
          new Creator[T, V]:
            def apply(
                fieldList: List[DFType],
                tokenTupleValues: List[Any]
            ): List[DFToken] = ${
              applyExpr[T, V]('fieldList, 'tokenTupleValues)
            }
        }
