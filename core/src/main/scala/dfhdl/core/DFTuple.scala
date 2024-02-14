package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*
import scala.NonEmptyTuple

type DFTuple[+T <: NonEmptyTuple] = DFStruct[T @uncheckedVariance]
object DFTuple:
  private[core] def apply[T <: NonEmptyTuple](t: NonEmptyTuple)(using DFC): DFTuple[T] =
    val tList = t.toList
    val fieldList: List[DFTypeAny] = tList.map(x => DFType(x))
    apply[T](fieldList)
  private[core] def apply[T <: NonEmptyTuple](
      fieldList: List[DFTypeAny]
  ): DFTuple[T] = ir.DFTuple(fieldList.map(_.asIR)).asFE[DFTuple[T]]
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
    type OutP
    def apply(
        fieldList: List[DFTypeAny],
        tupleValues: List[Any]
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
          tupleValuesExpr: Expr[List[Any]]
      )(using Quotes, Type[T], Type[V], Type[O], Type[TC]): Expr[List[O]] =
        import quotes.reflect.*
        val tArgs = TypeRepr.of[T].getTupleArgs
        val vArgs = TypeRepr.of[V].getTupleArgs
        if (tArgs.length == vArgs.length)
          val exprs =
            tArgs.zipWithIndex.lazyZip(vArgs).map { case ((t, i), v) =>
              val vType = v.asTypeOf[Any]
              val dfTypeTpe: Type[DFTypeAny] = t.asTypeOf[Any] match
                case '[DFValOf[t]] => TypeRepr.of[t].asTypeOf[DFTypeAny]
              val iExpr = Literal(IntConstant(i)).asExprOf[Int]
              '{
                val tc = compiletime.summonInline[
                  TC[dfTypeTpe.Underlying, vType.Underlying]
                ]
                val dfType =
                  $fieldListExpr
                    .apply($iExpr)
                    .asInstanceOf[dfTypeTpe.Underlying]
                val value =
                  $tupleValuesExpr
                    .apply($iExpr)
                    .asInstanceOf[vType.Underlying]
                tc.conv(dfType, value)(using compiletime.summonInline[DFC])
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
      val pType = TypeRepr.of[V].isConstTpe.asTypeOf[Any]
      '{
        new TCZipper[T, V, O, TC]:
          type OutP = pType.Underlying
          def apply(
              fieldList: List[DFTypeAny],
              tupleValues: List[Any]
          ): List[O] = ${
            applyExpr[T, V]('fieldList, 'tupleValues)
          }
      }
    end zipperMacro
  end TCZipper

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
      given DFTupleFromTuple[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple,
          Z <: TCZipper[T, R, DFValAny, TC]
      ](using
          zipper: Z
      ): TC[DFTuple[T], R] with
        type OutP = zipper.OutP
        def conv(dfType: DFTuple[T], value: R)(using DFC): Out =
          val dfVals =
            zipper(dfType.fieldList, value.toList)
          // reconstructing the Tuple DFType, in case fields could be DFNothing from conversions
          val fixedDFType = DFTuple(dfVals.map(_.dfType))
          // normally would have used `.asValTP`, but this triggers a compiler crash
          // https://github.com/lampepfl/dotty/issues/17326
          DFVal.Func(fixedDFType, FuncOp.++, dfVals).asInstanceOf[Out]
      end DFTupleFromTuple
      given DFTupleFromDFTuple[
          T <: NonEmptyTuple,
          RT <: NonEmptyTuple,
          RP,
          R <: DFValTP[DFTuple[RT], RP]
      ](using
          zipper: TCZipper[T, RT, DFValAny, TC]
      ): TC[DFTuple[T], R] with
        type OutP = RP
        def conv(dfType: DFTuple[T], value: R)(using DFC): Out =
          assert(dfType == value.dfType, "Tuple fields do not match.")
          value.asValTP[DFTuple[T], RP]
      end DFTupleFromDFTuple
    end TC

    object Compare:
      import DFVal.Compare
      given DFTupleArg[
          T <: NonEmptyTuple,
          R <: NonEmptyTuple,
          Op <: FuncOp,
          C <: Boolean,
          Z <: TCZipper[T, R, DFValAny, [T <: DFTypeAny, R] =>> Compare[T, R, Op, C]]
      ](using
          zipper: Z
      ): Compare[DFTuple[T], R, Op, C] with
        type OutP = zipper.OutP
        def conv(dfType: DFTuple[T], value: R)(using DFC): Out =
          val dfVals =
            zipper(dfType.fieldList, value.toList)
          DFVal.Func(dfType, FuncOp.++, dfVals).asInstanceOf[Out]
    end Compare

    object Ops:
      import DFBits.BitIndex
      extension [T <: NonEmptyTuple, M <: ModifierAny](dfTupleVal: DFVal[DFTuple[T], M])
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
          .SelectField(dfTupleVal, s"_${i + 1}")
          .asIR
          .asVal[OT, M]
        def toScalaTuple(using dfc: DFC, size: ValueOf[Tuple.Size[T]]): T =
          given DFC = dfc.anonymize
          val elements = Array.tabulate(size)(applyForced)
          Tuple.fromArray(elements).asInstanceOf[T]
        transparent inline def asScalaTuple(using dfc: DFC): NonEmptyTuple =
          ${ asScalaTupleMacro('dfTupleVal, 'dfc) }
      end extension

      protected[core] def asScalaTupleMacro[T <: NonEmptyTuple, M <: ModifierAny](using
          Quotes,
          Type[T],
          Type[M]
      )(
          dfTupleVal: Expr[DFVal[DFTuple[T], M]],
          dfc: Expr[DFC]
      ): Expr[NonEmptyTuple] =
        import quotes.reflect.*
        val tTpe = TypeRepr.of[T]
        val tplTypeArgs = tTpe.dealias.getTupleArgs
        val tplApply = Symbol.requiredMethod(s"scala.Tuple${tplTypeArgs.length}.apply")
        def applyForcedTerm(idx: Int): Term =
          tplTypeArgs(idx).asTypeOf[Any] match
            case '[DFValOf[t]] =>
              '{ ${ dfTupleVal }.applyForced[t](${ Expr(idx) })(using $dfc) }.asTerm
        val tplTermArgs = List.tabulate(tplTypeArgs.length)(applyForcedTerm)
        Ref(tplApply)
          .appliedToTypes(tplTypeArgs)
          .appliedToArgs(tplTermArgs)
          .asExprOf[NonEmptyTuple]
      end asScalaTupleMacro

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
