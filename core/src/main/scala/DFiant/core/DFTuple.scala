package DFiant.core
import DFiant.compiler.ir
import ir.DFVal.Func.{Op => FuncOp}
import DFiant.internals.*
import scala.quoted.*

type DFTuple[T] = DFStruct[DFTuple.Fields[T]]
object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val fieldList: List[DFTypeAny] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        //TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(x => DFType(x))
    DFStruct(Fields[T](fieldList))

  final case class Fields[T](fieldList: List[DFTypeAny]) extends DFFields:
    override lazy val typeName: String = ir.DFStruct.ReservedTupleName
    fieldList.zipWithIndex.foreach((f, i) => createField(f, (i + 1).toString))

  extension [T](dfType: DFTuple[T])
    def fieldList: List[DFTypeAny] =
      dfType.asIR.fieldMap.values.map(_.asFE[DFTypeAny]).toList

  trait TCZipper[
      T <: NonEmptyTuple,
      V <: NonEmptyTuple,
      O,
      TC[T <: DFTypeAny, V] <: GeneralTC[T, V, O]
  ]:
    def apply(
        fieldList: List[DFTypeAny],
        tokenTupleValues: List[Any]
    ): List[O]
  object TCZipper:
    inline given [
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: GeneralTC[T, V, O]
    ]: TCZipper[T, V, O, TC] = ${
      zipperMacro[T, V, O, TC]
    }
    import DFType.TC.MacroOps.*
    def zipperMacro[
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: GeneralTC[T, V, O]
    ](using
        Quotes,
        Type[T],
        Type[V],
        Type[O],
        Type[TC]
    ): Expr[TCZipper[T, V, O, TC]] =
      def applyExpr[T, V](
          fieldListExpr: Expr[List[DFTypeAny]],
          tokenTupleValuesExpr: Expr[List[Any]]
      )(using Quotes, Type[T], Type[V], Type[O], Type[TC]): Expr[List[O]] =
        import quotes.reflect.*
        val AppliedType(fun, tArgs) = TypeRepr.of[T]
        val AppliedType(_, vArgs) = TypeRepr.of[V]
        if (tArgs.length == vArgs.length)
          val exprs =
            tArgs.zipWithIndex.lazyZip(vArgs).map { case ((t, i), v) =>
              val vTpe = v.asTypeOf[Any]
              val dfTypeTpe = t.dfTypeTpe.get.asTypeOf[DFTypeAny]
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
              fieldList: List[DFTypeAny],
              tokenTupleValues: List[Any]
          ): List[O] = ${
            applyExpr[T, V]('fieldList, 'tokenTupleValues)
          }
      }
    end zipperMacro
  end TCZipper

  type Token[T] = DFToken[DFTuple[T]]
  object Token:
    protected[core] def apply[T](
        dfType: DFTuple[T],
        data: List[DFTokenAny]
    ): Token[T] =
      ir.DFToken(dfType.asIR, data).asTokenOf[DFTuple[T]]

    object TC:
      import DFToken.TC
      given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, V, DFTokenAny, TC]
      ): TC[DFTuple[T], ValueOf[V]] with
        def apply(dfType: DFTuple[T], value: ValueOf[V]): Out =
          DFTuple.Token[T](dfType, zipper(dfType.fieldList, value.value.toList))
    end TC
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, R, DFValAny, TC],
          dfc: DFC
      ): TC[DFTuple[T], ValueOf[R]] =
        new TC[DFTuple[T], ValueOf[R]]:
          type TType = DFTuple[T]
          def apply(dfType: DFTuple[T], value: ValueOf[R]): DFValOf[TType] =
            val dfVals =
              zipper(dfType.fieldList, value.value.toList)
            DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)

    end TC
  end Val
end DFTuple
