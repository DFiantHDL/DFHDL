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

  trait TCZipper[
      T <: NonEmptyTuple,
      V <: NonEmptyTuple,
      O,
      TC[T <: DFType, V] <: GeneralTC[T, V, O]
  ]:
    def apply(
        fieldList: List[DFType],
        tokenTupleValues: List[Any]
    ): List[O]
  object TCZipper:
    inline given [
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFType, V] <: GeneralTC[T, V, O]
    ]: TCZipper[T, V, O, TC] = ${
      zipperMacro[T, V, O, TC]
    }
    import DFType.TC.MacroOps.*
    def zipperMacro[
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFType, V] <: GeneralTC[T, V, O]
    ](using
        Quotes,
        Type[T],
        Type[V],
        Type[O],
        Type[TC]
    ): Expr[TCZipper[T, V, O, TC]] =
      def applyExpr[T, V](
          fieldListExpr: Expr[List[DFType]],
          tokenTupleValuesExpr: Expr[List[Any]]
      )(using Quotes, Type[T], Type[V], Type[O], Type[TC]): Expr[List[O]] =
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
                    TC[dfTypeTpe.Underlying, vTpe.Underlying]
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
            s"DFType tuple length (${tArgs.length}) and value tuple length (${vArgs.length}) do not match."
          )
          '{ ??? }
        end if
      end applyExpr
      import quotes.reflect.*
      '{
        new TCZipper[T, V, O, TC]:
          def apply(
              fieldList: List[DFType],
              tokenTupleValues: List[Any]
          ): List[O] = ${
            applyExpr[T, V]('fieldList, 'tokenTupleValues)
          }
      }
    end zipperMacro
  end TCZipper

  type Token[T] = DFToken.Of[DFTuple[T]]
  object Token:
    protected[core] def apply[T](
        dfType: DFTuple[T],
        data: List[DFToken]
    ): Token[T] =
      ir.DFToken(dfType.asIR, data).asInstanceOf[Token[T]]

    object TC:
      import DFToken.TC
      transparent inline given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple
      ](using
          creator: TCZipper[T, V, DFToken, TC]
      ): TC[DFTuple[T], V] = new TC[DFTuple[T], V]:
        type Out = DFTuple[T] <> TOKEN
        def apply(dfType: DFTuple[T], value: V): Out =
          DFTuple.Token[T](dfType, creator(dfType.fieldList, value.toList))
    end TC
  end Token

  object DFValTC:
    import DFVal.TC
    transparent inline given DFTupleArg[T <: NonEmptyTuple, R <: NonEmptyTuple]
        : TC[DFTuple[T], R] = ${ DFTupleArgMacro[T, R] }
    def DFTupleArgMacro[T <: NonEmptyTuple, R <: NonEmptyTuple](using
        Quotes,
        Type[T],
        Type[R]
    ): Expr[TC[DFTuple[T], R]] =
      import quotes.reflect.*
      val tTpe = TypeRepr.of[T]
      val rTpe = TypeRepr.of[R]
      println(tTpe)
      println(rTpe)
      '{
        new TC[DFTuple[T], R]:
          type Out = DFTuple[T]
          def apply(dfType: DFTuple[T], value: R): DFValOf[Out] = ???
      }
  end DFValTC
end DFTuple
