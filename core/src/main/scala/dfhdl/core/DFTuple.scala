package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*

type DFTuple[+T <: NonEmptyTuple] = DFStruct[T @uncheckedVariance]
object DFTuple:
  private[core] def apply[T <: NonEmptyTuple](t: NonEmptyTuple): DFTuple[T] =
    val tList = t.toList
    val fieldList: List[DFTypeAny] = tList.map(x => DFType(x))
    apply[T](fieldList)
  private[core] def apply[T <: NonEmptyTuple](
      fieldList: List[DFTypeAny]
  ): DFTuple[T] =
    DFStruct[T]("", (1 to fieldList.length).map(i => s"_$i").toList, fieldList)
  private[core] def unapply(t: NonEmptyTuple): Option[DFTuple[NonEmptyTuple]] =
    val tList = t.toList
    val fieldList: List[DFTypeAny] = tList.flatMap {
      case DFType(x) => Some(x)
      case _         => None
    }
    if (fieldList.length == tList.length) Some(apply[NonEmptyTuple](fieldList))
    else None

  extension [T <: NonEmptyTuple](dfType: DFTuple[T])
    def fieldList: List[DFTypeAny] =
      dfType.asIR.fieldMap.values.map(_.asFE[DFTypeAny]).toList

  trait TCZipper[
      T <: NonEmptyTuple,
      V <: NonEmptyTuple,
      O,
      TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
  ]:
    def apply(
        fieldList: List[DFTypeAny],
        tokenTupleValues: List[Any]
    ): List[O]
  object TCZipper:
    transparent inline given [
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
    ]: TCZipper[T, V, O, TC] = ${
      zipperMacro[T, V, O, TC]
    }
    def zipperMacro[
        T <: NonEmptyTuple,
        V <: NonEmptyTuple,
        O,
        TC[T <: DFTypeAny, V] <: TCConv[T, V, O]
    ](using
        Quotes,
        Type[T],
        Type[V],
        Type[O],
        Type[TC]
    ): Expr[TCZipper[T, V, O, TC]] =
      def applyExpr[T <: NonEmptyTuple, V <: NonEmptyTuple](
          fieldListExpr: Expr[List[DFTypeAny]],
          tokenTupleValuesExpr: Expr[List[Any]]
      )(using Quotes, Type[T], Type[V], Type[O], Type[TC]): Expr[List[O]] =
        import quotes.reflect.*
        val tArgs = TypeRepr.of[T].getTupleArgs
        val vArgs = TypeRepr.of[V].getTupleArgs
        if (tArgs.length == vArgs.length)
          val exprs =
            tArgs.zipWithIndex.lazyZip(vArgs).map { case ((t, i), v) =>
              val vTpe = v.asTypeOf[Any]
              val dfTypeTpe: Type[DFTypeAny] = t.asTypeOf[Any] match
                case '[DFValOf[t]] => TypeRepr.of[t].asTypeOf[DFTypeAny]
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
                tc.conv(dfType, value)
              }
            }
          '{ List(${ Varargs(exprs) }*) }
        else
          errorExpr(
            s"DFType tuple length (${tArgs.length}) and value tuple length (${vArgs.length}) do not match."
          )
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

  type Token[+T <: NonEmptyTuple] = DFToken[DFTuple[T]]
  object Token:
    protected[core] def apply[T <: NonEmptyTuple](
        dfType: DFTuple[T],
        data: List[Any]
    ): Token[T] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFTuple[T]]

    object TC:
      import DFToken.TC
      given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, V, DFTokenAny, TC]
      ): TC[DFTuple[T], V] with
        def conv(dfType: DFTuple[T], value: V): Out =
          DFTuple.Token[T](
            dfType,
            zipper(dfType.fieldList, value.toList).map(_.asIR.data)
          )
    end TC

    object Compare:
      import DFToken.Compare
      given DFTupleTokenFromTuple[
          T <: NonEmptyTuple,
          V <: NonEmptyTuple,
          Op <: FuncOp,
          C <: Boolean
      ](using
          zipper: TCZipper[T, V, DFTokenAny, [T <: DFTypeAny, R] =>> Compare[T, R, Op, C]]
      ): Compare[DFTuple[T], V, Op, C] with
        def conv(dfType: DFTuple[T], value: V): Out =
          DFTuple.Token[T](
            dfType,
            zipper(dfType.fieldList, value.toList).map(_.asIR.data)
          )
    end Compare

    object Ops:
      import CompanionsDFBits.BitIndex
      extension [T <: NonEmptyTuple](t: DFToken[DFTuple[T]])
        def apply[I <: Int](i: Inlined[I])(using
            check: BitIndex.Check[I, Tuple.Size[T]],
            size: ValueOf[Tuple.Size[T]]
        ): DFToken[DFType.FromDFVal[Tuple.Elem[T, I]]] =
          check(i, size)
          selectRuntime[DFType.FromDFVal[Tuple.Elem[T, I]]](t.wide, i)
      private def selectRuntime[T <: DFTypeAny](
          token: Token[NonEmptyTuple],
          idx: Int
      ): DFToken[T] =
        val dfType = token.dfType.fieldList(idx).asIR
        val data = token.data(idx)
        ir.DFToken.forced(dfType, data).asTokenOf[T]
      extension [T <: NonEmptyTuple](t: DFToken[DFTuple[T]])
        // TODO: workaround compiler issue that inline does not obey covariance
        private def wide: Token[NonEmptyTuple] =
          t.asInstanceOf[Token[NonEmptyTuple]]

//      extension [T1 <: DFTypeAny](t: Token[Tuple1[DFValOf[T1]]])
//        inline def _1: DFToken[T1] =
//          selectRuntime[T1](t.wide, 0)
//      extension [T1 <: DFTypeAny, T2 <: DFTypeAny](
//          t: Token[(DFValOf[T1], DFValOf[T2])]
//      )
//        inline def _1: DFToken[T1] =
//          selectRuntime[T1](t.wide, 0)
//        inline def _2: DFToken[T2] =
//          selectRuntime[T2](t.wide, 1)
//      extension [T1 <: DFTypeAny, T2 <: DFTypeAny, T3 <: DFTypeAny](
//          t: Token[(DFValOf[T1], DFValOf[T2], DFValOf[T3])]
//      )
//        inline def _1: DFToken[T1] =
//          selectRuntime[T1](t.wide, 0)
//        inline def _2: DFToken[T2] =
//          selectRuntime[T2](t.wide, 1)
//        inline def _3: DFToken[T3] =
//          selectRuntime[T3](t.wide, 2)
    end Ops

  end Token

  object Val:
    private[core] def unapply(
        tuple: Tuple
    )(using DFC): Option[DFValOf[DFTuple[NonEmptyTuple]]] =
      val dfVals = tuple.toList.flatMap {
        case DFVal.OrTupleOrStruct(dfVal) => Some(dfVal)
        case _                            => None
      }
      if (tuple.size == dfVals.length)
        val dfType = DFTuple[NonEmptyTuple](dfVals.map(_.dfType))
        Some(DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize))
      else None
    object TC:
      import DFVal.TC
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple
      ](using
          zipper: TCZipper[T, R, DFValAny, TC],
          dfc: DFC
      ): TC[DFTuple[T], R] with
        def conv(dfType: DFTuple[T], value: R): Out =
          val dfVals =
            zipper(dfType.fieldList, value.toList)
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
    end TC

    object Compare:
      import DFVal.Compare
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple,
          Op <: FuncOp,
          C <: Boolean
      ](using
          zipper: TCZipper[T, R, DFValAny, [T <: DFTypeAny, R] =>> Compare[T, R, Op, C]],
          dfc: DFC
      ): Compare[DFTuple[T], R, Op, C] with
        def conv(dfType: DFTuple[T], value: R): Out =
          val dfVals =
            zipper(dfType.fieldList, value.toList)
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
    end Compare

    object Ops:
      import CompanionsDFBits.BitIndex
      extension [T <: NonEmptyTuple, M <: ModifierAny](t: DFVal[DFTuple[T], M])
        def apply[I <: Int](i: Inlined[I])(using
            dfc: DFC,
            check: BitIndex.Check[I, Tuple.Size[T]],
            size: ValueOf[Tuple.Size[T]]
        ): DFVal[DFType.FromDFVal[Tuple.Elem[T, I]], M] = trydf {
          check(i, size)
          applyForced[DFType.FromDFVal[Tuple.Elem[T, I]]](i)
        }
        private[core] def applyForced[OT <: DFTypeAny](i: Int)(using
            dfc: DFC
        ): DFVal[OT, M] = DFVal.Alias
          .SelectField(t, s"_${i + 1}")
          .asIR
          .asVal[OT, M]
      end extension
//      extension [T1 <: DFTypeAny, M <: ModifierAny](
//          t: DFVal[DFTuple[Tuple1[DFValOf[T1]]], M]
//      )
//        inline def _1(using DFC): DFVal[T1, M] =
//          t.applyForced[T1](0)
//      extension [T1 <: DFTypeAny, T2 <: DFTypeAny, M <: ModifierAny](
//          t: DFVal[DFTuple[(DFValOf[T1], DFValOf[T2])], M]
//      )
//        inline def _1(using DFC): DFVal[T1, M] =
//          t.applyForced[T1](0)
//        inline def _2(using DFC): DFVal[T2, M] =
//          t.applyForced[T2](1)
//      extension [
//          T1 <: DFTypeAny,
//          T2 <: DFTypeAny,
//          T3 <: DFTypeAny,
//          M <: ModifierAny
//      ](t: DFVal[DFTuple[(DFValOf[T1], DFValOf[T2], DFValOf[T3])], M])
//        inline def _1(using DFC): DFVal[T1, M] =
//          t.applyForced[T1](0)
//        inline def _2(using DFC): DFVal[T2, M] =
//          t.applyForced[T2](1)
//        inline def _3(using DFC): DFVal[T3, M] =
//          t.applyForced[T3](2)
    end Ops
  end Val
end DFTuple
