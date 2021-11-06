package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

sealed trait Args
sealed trait NoArgs extends Args
sealed trait Args1[T1] extends Args
sealed trait Args2[T1, T2] extends Args
sealed trait Args3[T1, T2, T3] extends Args

final class DFType[+T <: ir.DFType, +A <: Args](val value: T) extends AnyVal:
  override def toString: String = value.toString
type DFTypeAny = DFType[ir.DFType, Args]

object DFType:
  private[core] def apply(t: Any): DFTypeAny =
    t match
      case dfType: DFTypeAny         => dfType
      case tuple: NonEmptyTuple      => DFTuple(tuple)
      case tfe: DFOpaque.Frontend[_] => DFOpaque(tfe)
      case fields: DFFields          => DFStruct(fields)
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

  given [T <: DFTypeAny]: CanEqual[T, T] = CanEqual.derived

  type Supported = AnyRef | DFTypeAny
  object Ops:
    extension [T](t: T)(using tc: TC[T])
      def <>[M <: ir.DFVal.Modifier](modifier: M)(using
          DFC
      ): DFVal[tc.Type, M] =
        DFVal.Dcl(tc(t), modifier)
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
    given ofDFType[T <: DFTypeAny]: TC[T] with
      type Type = T
      def apply(t: T): Type = t
    given ofOpaque[T <: DFTypeAny, TFE <: DFOpaque.Frontend[T]]: TC[TFE] with
      type Type = DFOpaque[TFE]
      def apply(t: TFE): Type = DFOpaque(t)
    given ofDFFields[T <: DFFields]: TC[T] with
      type Type = DFStruct[T]
      def apply(t: T): Type = DFStruct[T](t)
    object MacroOps:
      extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
        def dfTypeTpe: Option[quotes.reflect.TypeRepr] =
          import quotes.reflect.*
          val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
          val fieldsTpe = TypeRepr.of[DFFields]
          tpe.dealias match
            case applied: AppliedType if applied <:< nonEmptyTupleTpe =>
              if (applied.args.forall(_.dfTypeTpe.nonEmpty))
                Some(TypeRepr.of[DFTuple].appliedTo(applied))
              else None
            case t if t <:< TypeRepr.of[DFTypeAny] =>
              Some(t)
            case t if t <:< TypeRepr.of[DFOpaque.Abstract] =>
              Some(TypeRepr.of[DFOpaque].appliedTo(t))
            case t if t <:< fieldsTpe =>
              Some(TypeRepr.of[DFStruct].appliedTo(t))
            case t @ DFEnum(_) =>
              Some(TypeRepr.of[DFEnum].appliedTo(t))
            case t =>
              None
          end match
    end MacroOps

    import MacroOps.*
    transparent inline given ofAnyRef[T <: AnyRef]: TC[T] = ${ tcMacro[T] }
    def tcMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
      import quotes.reflect.*
      val tTpe = TypeRepr.of[T]
      val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
      val fieldTpe = TypeRepr.of[DFField[_]]
      val fieldsTpe = TypeRepr.of[DFFields]
      def checkSupported(tTpe: TypeRepr): Unit =
        if (tTpe.dfTypeTpe.isEmpty)
          report.error(
            s"Unsupported dataflow type can be found for: ${tTpe.show}"
          )

      checkSupported(tTpe)
      tTpe.dealias match
        case t if t <:< nonEmptyTupleTpe =>
          '{
            new TC[T]:
              type Type = DFTuple[T]
              def apply(t: T): Type = DFTuple[T](t)
          }
        case DFEnum(entries) =>
          val clsType = entries.head.asTypeOf[DFEncoding]
          '{
            new TC[T]:
              type Type = DFEnum[clsType.Underlying]
              def apply(t: T): Type = DFEnum[clsType.Underlying](t)
          }
      end match
    end tcMacro
  end TC
end DFType

extension [T](t: T)(using tc: DFType.TC[T]) def dfType: tc.Type = tc(t)

extension [T <: DFTypeAny](
    token: DFToken[T]
) def dfType: T = token.asIR.dfType.asFE[T]

extension [T <: DFTypeAny, M <: ir.DFVal.Modifier](
    dfVal: DFVal[T, M]
) def dfType: T = dfVal.asIR.dfType.asFE[T]
