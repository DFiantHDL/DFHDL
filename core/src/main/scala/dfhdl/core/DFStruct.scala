package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.quoted.*
import collection.immutable.ListMap
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.unchecked.uncheckedVariance

type FieldsOrTuple = DFStruct.Fields | NonEmptyTuple
type DFStruct[+F <: FieldsOrTuple] =
  DFType[ir.DFStruct, Args1[F @uncheckedVariance]]
object DFStruct:
  abstract class Fields extends Product with Serializable
  private[core] def apply[F <: FieldsOrTuple](
      name: String,
      fieldMap: ListMap[String, DFTypeAny]
  ): DFStruct[F] =
    ir.DFStruct(name, fieldMap.map((n, t) => (n, t.asIR))).asFE[DFStruct[F]]
  private[core] def apply[F <: FieldsOrTuple](
      name: String,
      fieldNames: List[String],
      fieldTypes: List[DFTypeAny]
  ): DFStruct[F] =
    apply[F](name, ListMap(fieldNames.lazyZip(fieldTypes).toSeq*))
  private[core] def apply[F <: FieldsOrTuple](product: F): DFStruct[F] =
    unapply(
      product
    ).get.asInstanceOf[DFStruct[F]]
  private[core] def unapply(
      product: FieldsOrTuple
  ): Option[DFStruct[FieldsOrTuple]] =
    val fieldTypes = product.productIterator.flatMap {
      case dfVal: DFValAny => Some(dfVal.dfType)
      case _               => None
    }.toList
    if (fieldTypes.length == product.productIterator.size)
      val fieldNames = product.productElementNames.toList
      Some(DFStruct(product.productPrefix, fieldNames, fieldTypes))
    else None

  inline given apply[F <: FieldsOrTuple]: DFStruct[F] = ${ dfTypeMacro[F] }
  def dfTypeMacro[F <: FieldsOrTuple](using
      Quotes,
      Type[F]
  ): Expr[DFStruct[F]] =
    import quotes.reflect.*
    val fTpe = TypeRepr.of[F]
    val (structName, fields) = fTpe.asTypeOf[Any] match
      case '[NonEmptyTuple] =>
        val args = fTpe.getTupleArgs
        (
          ir.DFTuple.structName(args.length),
          args.zipWithIndex.map((t, i) => (ir.DFTuple.fieldName(i), t.asTypeOf[Any]))
        )
      case _ =>
        val clsSym = fTpe.classSymbol.get
        (
          clsSym.name.toString,
          clsSym.caseFields.view
            .map(m => (m.name.toString, fTpe.memberType(m).asTypeOf[Any]))
        )
    val fieldErrors = fields.filter {
      case (_, '[DFValOf[t]]) => false
      case _                  => true
    }.toList
    if (fieldErrors.isEmpty)
      val fieldNames: List[Expr[String]] = fields.map((n, _) => Expr(n)).toList
      val fieldTypes: List[Expr[DFTypeAny]] = fields.collect { case (_, '[DFValOf[t]]) =>
        '{ compiletime.summonInline[t] }
      }.toList
      val fieldNamesExpr = Varargs(fieldNames)
      val fieldTypesExpr = Varargs(fieldTypes)
      val nameExpr = Expr(structName)
      '{
        DFStruct
          .apply[F]($nameExpr, List($fieldNamesExpr*), List($fieldTypesExpr*))
      }
    else
      val fieldTypesStr = fieldErrors
        .map { case (n, '[t]) =>
          s"${n}: ${TypeRepr.of[t].showType}"
        }
        .mkString("\n")
      val intro = if (structName.isEmpty) "tuple" else s"struct `$structName`"
      val msg =
        s"""The $intro has invalid DFHDL value field types. 
           |A valid field type is in the form of [DFType] <> VAL.
           |The following fields do not match this pattern:
           |$fieldTypesStr""".stripMargin
      '{ compiletime.error(${ Expr(msg) }) }
    end if
  end dfTypeMacro

  protected trait SameFields[T <: Fields, RF <: Fields]:
    def check(dfType: DFStruct[T], argType: DFStruct[RF]): Unit =
      if (dfType != argType)
        throw new IllegalArgumentException(
          s"""Mismatch in structure fields.
             |The applied value type is:
             |$argType
             |The receiver type is:
             |$dfType""".stripMargin
        )
  inline given [L <: Fields, R <: Fields]: SameFields[L, R] = ${ sfMacro[L, R] }
  protected def sfMacro[L <: Fields: Type, R <: Fields: Type](using
      Quotes
  ): Expr[SameFields[L, R]] =
    import quotes.reflect.*
    val tpeL = TypeRepr.of[L]
    val tpeR = TypeRepr.of[R]
    def sameTypes(tpeL: TypeRepr, tpeR: TypeRepr): Boolean =
      // if the types match according to scala, then we return true
      if (tpeL =:= tpeR) return true
      (tpeL, tpeR) match
        // if both types are constants, then they must return the same value
        case (ConstantType(l), ConstantType(r)) => return l equals r
        case _                                  => // continue
      (tpeL.asTypeOf[Any], tpeR.asTypeOf[Any]) match
        case ('[Int], '[Int])                 => true
        case ('[Boolean], '[Boolean])         => true
        case ('[DFBoolOrBit], '[DFBoolOrBit]) => true
        case ('[DFBits[lw]], '[DFBits[rw]])   => sameTypes(TypeRepr.of[lw], TypeRepr.of[rw])
        case ('[DFDecimal[ls, lw, lf]], '[DFDecimal[rs, rw, rf]]) =>
          sameTypes(TypeRepr.of[ls], TypeRepr.of[rs]) &&
          sameTypes(TypeRepr.of[lw], TypeRepr.of[rw]) &&
          sameTypes(TypeRepr.of[lf], TypeRepr.of[rf])
        case ('[DFEnum[le]], '[DFEnum[re]]) => sameTypes(TypeRepr.of[le], TypeRepr.of[re])
        case ('[DFVector[lt, ld]], '[DFVector[rt, rd]]) =>
          sameTypes(TypeRepr.of[lt], TypeRepr.of[rt]) &&
          sameTypes(TypeRepr.of[ld], TypeRepr.of[rd])
        case ('[DFOpaque[lt]], '[DFOpaque[rt]]) => sameTypes(TypeRepr.of[lt], TypeRepr.of[rt])
        case ('[DFStruct[lt]], '[DFStruct[rt]]) => sameTypes(TypeRepr.of[lt], TypeRepr.of[rt])
        case ('[Fields], '[Fields]) =>
          val symL = tpeL.typeSymbol
          val symR = tpeR.typeSymbol
          if (!(symL equals symR)) return false
          val fieldsL = symL.fieldMembers.map(tpeL.memberType)
          val fieldsR = symR.fieldMembers.map(tpeR.memberType)
          fieldsL.lazyZip(fieldsR).forall((l, r) => sameTypes(l, r))
        case ('[DFValOf[l]], '[DFValOf[r]]) => sameTypes(TypeRepr.of[l], TypeRepr.of[r])
        case _                              => false
      end match
    end sameTypes
    if (sameTypes(tpeL, tpeR)) '{ new SameFields[L, R] {} }
    else
      val msg =
        s"Mismatch structure value type `${tpeR.showType}` for DFHDL receiver structure type `${tpeL.showType}`."
      '{ compiletime.error(${ Expr(msg) }) }
  end sfMacro

  type Token[+F <: FieldsOrTuple] = DFToken[DFStruct[F]]
  object Token:
    def apply[F <: FieldsOrTuple](dfType: DFStruct[F], value: Fields): Token[F] =
      val data = value.productIterator.map { case dfVal: DFVal[?, ?] =>
        dfVal.asIR match
          case ir.DFVal.Const(token, _, _, _) => token.data
          case v =>
            throw new IllegalArgumentException(
              s"Tokens must only be constant but found the value: ${v}"
            )
      }.toList
      ir.DFToken.forced(dfType.asIR, data).asTokenOf[DFStruct[F]]
    object TC:
      import DFToken.TC
      given DFStructTokenFromCC[
          F <: Fields,
          RF <: Fields
      ](using sf: SameFields[F, RF]): TC[DFStruct[F], RF] with
        def conv(dfType: DFStruct[F], value: RF)(using Ctx): Out =
          sf.check(dfType, DFStruct(value))
          Token(dfType, value)
      given DFStructTokenFromStruct[
          F <: Fields,
          RF <: Fields,
          V <: Token[RF]
      ](using sf: SameFields[F, RF]): TC[DFStruct[F], V] with
        def conv(dfType: DFStruct[F], value: V)(using Ctx): Out =
          sf.check(dfType, value.dfType)
          value.asTokenOf[DFStruct[F]]
    end TC

    object Compare:
      import DFToken.Compare
      given DFStructTokenFromCC[
          F <: Fields,
          RF <: Fields,
          Op <: FuncOp,
          C <: Boolean
      ](using sf: SameFields[F, RF]): Compare[DFStruct[F], RF, Op, C] with
        def conv(dfType: DFStruct[F], value: RF)(using Ctx): Out =
          sf.check(dfType, DFStruct(value))
          Token(dfType, value)
      given DFStructTokenFromStruct[
          F <: Fields,
          RF <: Fields,
          R <: Token[RF],
          Op <: FuncOp,
          C <: Boolean
      ](using sf: SameFields[F, RF]): Compare[DFStruct[F], R, Op, C] with
        def conv(dfType: DFStruct[F], value: R)(using Ctx): Out =
          sf.check(dfType, value.dfType)
          value.asTokenOf[DFStruct[F]]
    end Compare

    trait Refiner[T <: FieldsOrTuple]:
      type Out <: DFToken[DFStruct[T]]
    object Refiner:
      transparent inline given [T <: FieldsOrTuple]: Refiner[T] = ${ refineMacro[T] }
      def refineMacro[T <: FieldsOrTuple](using Quotes, Type[T]): Expr[Refiner[T]] =
        import quotes.reflect.*
        val tokenTpe = TypeRepr.of[DFToken[DFStruct[T]]]
        val tTpe = TypeRepr.of[T]
        val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
          case '[NonEmptyTuple] =>
            tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
              f.asTypeOf[Any] match
                case '[DFValOf[t]] =>
                  (s"_${i + 1}", TypeRepr.of[DFToken[t]])
            )
          case _ => ???

        val refined = fields.foldLeft(tokenTpe) { case (r, (n, t)) => Refinement(r, n, t) }
        val refinedType = refined.asTypeOf[DFToken[DFStruct[T]]]
        '{
          new Refiner[T]:
            type Out = refinedType.Underlying
        }
      end refineMacro
    end Refiner
  end Token

  object Val:
    private[core] def unapply(
        fields: FieldsOrTuple
    )(using DFC): Option[DFValOf[DFStruct[FieldsOrTuple]]] =
      fields match
        case DFStruct(dfType) =>
          val dfVals = fields.productIterator.map { case dfVal: DFValAny =>
            dfVal
          }.toList
          Some(DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize))
        case _ => None
    object TC:
      import DFVal.TC
      given DFStructValFromCC[
          F <: Fields,
          RF <: Fields
      ](using sf: SameFields[F, RF]): TC[DFStruct[F], RF] with
        type OutP = Any
        def conv(dfType: DFStruct[F], value: RF)(using Ctx): Out =
          sf.check(dfType, DFStruct(value))
          val dfVals = value.productIterator.map { case dfVal: DFVal[?, ?] =>
            dfVal
          }.toList
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      given DFStructValFromStruct[
          F <: Fields,
          RF <: Fields,
          RP,
          V <: DFValTP[DFStruct[RF], RP]
      ](using sf: SameFields[F, RF]): TC[DFStruct[F], V] with
        type OutP = RP
        def conv(dfType: DFStruct[F], value: V)(using Ctx): Out =
          sf.check(dfType, value.dfType)
          value.asValTP[DFStruct[F], RP]
    end TC
    object Compare:
      import DFVal.Compare
      given DFStructArgCC[
          F <: Fields,
          RF <: Fields,
          Op <: FuncOp,
          C <: Boolean
      ](using sf: SameFields[F, RF]): Compare[DFStruct[F], RF, Op, C] with
        def conv(dfType: DFStruct[F], value: RF)(using Ctx): Out =
          sf.check(dfType, DFStruct(value))
          val dfVals = value.productIterator.map { case dfVal: DFVal[?, ?] =>
            dfVal
          }.toList
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      given DFStructArgStruct[
          F <: Fields,
          RF <: Fields,
          R <: DFValOf[DFStruct[RF]],
          Op <: FuncOp,
          C <: Boolean
      ](using sf: SameFields[F, RF]): Compare[DFStruct[F], R, Op, C] with
        def conv(dfType: DFStruct[F], value: R)(using Ctx): Out =
          sf.check(dfType, value.dfType)
          value.asValOf[DFStruct[F]]
    end Compare

    trait Refiner[T <: FieldsOrTuple, A, I, P]:
      type Out <: DFVal[DFStruct[T], Modifier[A, Any, I, P]]
    object Refiner:
      transparent inline given [T <: FieldsOrTuple, A, I, P]: Refiner[T, A, I, P] = ${
        refineMacro[T, A, I, P]
      }
      def refineMacro[T <: FieldsOrTuple, A, I, P](using
          Quotes,
          Type[T],
          Type[A],
          Type[I],
          Type[P]
      ): Expr[Refiner[T, A, I, P]] =
        import quotes.reflect.*
        val dfValTpe = TypeRepr.of[DFVal[DFStruct[T], Modifier[A, Any, I, P]]]
        val tTpe = TypeRepr.of[T]
        val fields: List[(String, TypeRepr)] = tTpe.asTypeOf[Any] match
          case '[NonEmptyTuple] =>
            tTpe.getTupleArgs.zipWithIndex.map((f, i) =>
              f.asTypeOf[Any] match
                case '[DFValOf[t]] =>
                  (s"_${i + 1}", TypeRepr.of[DFVal[t, Modifier[A, Any, I, P]]])
            )
          case _ =>
            val clsSym = tTpe.classSymbol.get
            clsSym.caseFields.map(m =>
              tTpe.memberType(m).asTypeOf[Any] match
                case '[DFValOf[t]] =>
                  (m.name.toString, TypeRepr.of[DFVal[t, Modifier[A, Any, I, P]]])
            )

        val refined = fields.foldLeft(dfValTpe) { case (r, (n, t)) =>
          Refinement(r, n, t)
        }
        val refinedType = refined.asTypeOf[DFVal[DFStruct[T], Modifier[A, Any, I, P]]]
        '{
          new Refiner[T, A, I, P]:
            type Out = refinedType.Underlying
        }
      end refineMacro
    end Refiner
  end Val
end DFStruct
