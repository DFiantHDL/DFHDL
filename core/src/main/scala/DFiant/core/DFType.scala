package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap
import DFOpaque.Abstract as DFOpaqueA
import reflect.EnumCompanion

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T) extends AnyVal:
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType, Args]
val NoType = new DFType[ir.NoType.type, NoArgs](ir.NoType)

object DFType:
  type Of[T <: Supported] <: DFTypeAny = T match
    case DFTypeAny                => T <:! DFTypeAny
    case DFEncoding               => DFEnum[T]
    case reflect.EnumCompanion[t] => Of[t]
    case DFOpaqueA                => DFOpaque[T]
    case NonEmptyTuple            => DFTuple[Tuple.Map[T, JUSTVAL]]
    case Product                  => DFStruct[T]
  type FromDFVal[T] <: DFTypeAny = T match
    case DFValOf[t] => t

  def of[T <: Supported](t: T): Of[T] = DFType(t).asInstanceOf[Of[T]]
  private[core] def apply(t: Any): DFTypeAny =
    t match
      case dfType: DFTypeAny         => dfType
      case tuple: NonEmptyTuple      => DFTuple(tuple)
      case tfe: DFOpaque.Frontend[_] => DFOpaque(tfe)
      // TODO: need to add proper upper-bound if fixed in Scalac
      // see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)
  extension [T <: ir.DFType, A <: Args](dfType: DFType[T, A])
    def asIR: T = dfType.value
    def codeString(using printer: Printer): String = printer.csDFType(asIR)
  extension (dfType: ir.DFType)
    def asFE[T <: DFTypeAny]: T = new DFType(dfType).asInstanceOf[T]
  transparent inline implicit def conv[T <: Supported](inline t: T)(implicit
      tc: TC[T]
  ): DFTypeAny = tc(t)
  export DFDecimal.Extensions.*
  export DFBoolOrBit.given
  export DFBits.given
  export DFDecimal.given
  export DFEnum.given
  export DFStruct.given

  given [T <: DFTypeAny]: CanEqual[T, T] = CanEqual.derived

  type Supported = DFTypeAny | DFEncoding | AnyRef
  object Ops:
    extension [T <: Supported](t: T)(using tc: DFType.TC[T])
      def <>[M <: ir.DFVal.Modifier](modifier: M)(using
          DFC
      ): DFVal[tc.Type, M] = DFVal.Dcl(tc(t), modifier)
      def token[V](tokenValue: Exact[V])(using
          tokenTC: DFToken.TC[tc.Type, V]
      ): tokenTC.Out = tokenTC(tc(t), tokenValue)
      def const[V](tokenValue: Exact[V])(using
          DFToken.TC[tc.Type, V]
      )(using DFC): DFValOf[tc.Type] =
        DFVal.Const(token(tokenValue), named = true)
    end extension
  end Ops

  trait TC[-T]:
    type Type <: DFTypeAny
    def apply(t: T): Type
  object TC:
    transparent inline given ofDFType[T <: DFTypeAny]: TC[T] = new TC[T]:
      type Type = T
      def apply(t: T): Type = t

    transparent inline given ofOpaque[T <: DFTypeAny, TFE <: DFOpaque.Frontend[
      T
    ]]: TC[TFE] = new TC[TFE]:
      type Type = DFOpaque[TFE]
      def apply(t: TFE): Type = DFOpaque(t)

    transparent inline given ofEnumCompanion[E <: DFEncoding]
        : TC[EnumCompanion[E]] = new TC[EnumCompanion[E]]:
      type Type = DFEnum[E]
      def apply(t: EnumCompanion[E]): Type = DFEnum[E](t)

    transparent inline given ofStructCompanion[F <: AnyRef](using
        cc: CaseClass[F]
    )(using dfType: DFStruct[cc.CC]): TC[F] = new TC[F]:
      type Type = DFStruct[cc.CC]
      def apply(t: F): Type = dfType

    transparent inline given ofTuple[T <: NonEmptyTuple]: TC[T] = ${
      ofTupleMacro[T]
    }
    def ofTupleMacro[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val tTpe = TypeRepr.of[T]
      val AppliedType(fun, args) = tTpe
      val tcTrees = args.map(t =>
        Implicits.search(TypeRepr.of[TC].appliedTo(t)) match
          case iss: ImplicitSearchSuccess =>
            iss.tree
          case isf: ImplicitSearchFailure =>
            report.errorAndAbort(isf.explanation)
      )
      val tcList = '{
        List(${ Varargs(tcTrees.map(_.asExpr)) }*).asInstanceOf[List[TC[Any]]]
      }
      val tpes = tcTrees
        .map(_.tpe.asTypeOf[Any] match
          case '[TC[t] { type Type = z }] => TypeRepr.of[z]
        )
        .map(t => TypeRepr.of[DFValOf].appliedTo(t))
      def applyExpr(t: Expr[T]): Expr[List[DFTypeAny]] =
        '{
          val tList = $t.toList.asInstanceOf[List[Any]]
          $tcList.lazyZip(tList).map((tc, t) => tc(t)).toList
        }
      val tplTpe = fun.appliedTo(tpes)
      val tplType = tplTpe.asTypeOf[NonEmptyTuple]
      '{
        new TC[T]:
          type Type = DFTuple[tplType.Underlying]
          def apply(t: T): Type =
            DFTuple[tplType.Underlying](${ applyExpr('t) })
      }
    end ofTupleMacro

    object MacroOps:
      extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
        def dfTypeTpe: Option[quotes.reflect.TypeRepr] =
          import quotes.reflect.*
          val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
//          val fieldsTpe = TypeRepr.of[DFFields]
          tpe.asTypeOf[Any] match
            case '[NonEmptyTuple] =>
              if (tpe.getTupleArgs.forall(_.dfTypeTpe.nonEmpty))
                Some(TypeRepr.of[DFTuple].appliedTo(tpe)) // TODO: this is wrong
              else None
            case '[DFTypeAny] =>
              Some(tpe)
            case '[DFOpaque.Abstract] =>
              Some(TypeRepr.of[DFOpaque].appliedTo(tpe))
//            case t if t <:< fieldsTpe =>
//              Some(TypeRepr.of[DFStruct].appliedTo(t))
            case _ =>
              tpe.dealias match
                case t @ DFEnum(_) =>
                  Some(TypeRepr.of[DFEnum].appliedTo(t))
                case t =>
                  None
          end match
    end MacroOps

//    import MacroOps.*
//    transparent inline given ofAnyRef[T <: AnyRef]: TC[T] = ${ tcMacro[T] }
//    def tcMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
//      import quotes.reflect.*
//      val tTpe = TypeRepr.of[T]
//      val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
////      val fieldTpe = TypeRepr.of[DFField[_]]
////      val fieldsTpe = TypeRepr.of[DFFields]
//      def checkSupported(tTpe: TypeRepr): Unit =
//        if (tTpe.dfTypeTpe.isEmpty)
//          report.error(
//            s"Unsupported dataflow type can be found for: ${tTpe.show}"
//          )
//
//      checkSupported(tTpe)
//      tTpe.dealias match
//        case t if t <:< nonEmptyTupleTpe =>
//          '{
//            new TC[T]:
//              type Type = DFTuple[NonEmptyTuple]
//              def apply(t: T): Type = DFTuple[NonEmptyTuple](???)
//          }
//        case DFEnum(entries) =>
//          val clsType = entries.head.asTypeOf[DFEncoding]
//          '{
//            new TC[T]:
//              type Type = DFEnum[clsType.Underlying]
//              def apply(t: T): Type = DFEnum[clsType.Underlying](t)
//          }
//      end match
//    end tcMacro
  end TC
end DFType

extension [T](t: T)(using tc: DFType.TC[T]) def dfType: tc.Type = tc(t)

extension [T <: DFTypeAny](
    token: DFToken[T]
) def dfType: T = token.asIR.dfType.asFE[T]

extension [T <: DFTypeAny, M <: ir.DFVal.Modifier](
    dfVal: DFVal[T, M]
) def dfType: T = dfVal.asIR.dfType.asFE[T]
