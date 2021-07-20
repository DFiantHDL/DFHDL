package DFiant.core
import DFiant.compiler.printing.Printer
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

opaque type DFType = ir.DFType
object DFType:
  extension (dfType: ir.DFType)
    def asFE[T <: DFType]: T = dfType.asInstanceOf[T]
  extension (dfType: DFType)
    def asIR: ir.DFType = dfType
    def codeString(using printer: Printer): String = printer.csDFType(asIR)

  opaque type Of[+T <: ir.DFType] <: DFType = T
  object Of:
    extension [T <: ir.DFType](of: Of[T]) def asIR: T = of

  type Supported = AnyRef | DFType
  private[core] def apply(t: Any): DFType =
    t match
      case dfType: ir.DFType    => dfType
      case tuple: NonEmptyTuple => DFTuple(tuple)
      case fields: DFFields     => DFStruct(fields)
      //TODO: need to add proper upper-bound if fixed in Scalac
      //see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)
  object Ops:
    extension [T](t: T)(using tc: TC[T])
      def dfType: tc.Type = tc(t)
      def width(using w: Width[T]): Inlined.Int[w.Out] =
        Inlined.Int.forced[w.Out](dfType.asIR.width)
      def <>[M <: DFVal.Modifier](modifier: M)(using DFC): DFVal[tc.Type, M] =
        DFVal.Dcl(tc(t), modifier)
      def token[V](tokenValue: Exact[V])(using
          tokenTC: DFToken.TC[tc.Type, V]
      ): tokenTC.Out = tokenTC(tc(t), tokenValue)
      def const[V](tokenValue: Exact[V])(using
          DFToken.TC[tc.Type, V]
      )(using DFC): DFValOf[tc.Type] =
        DFVal.Const(token(tokenValue), named = true)
//    extension [T <: NonEmptyTuple](t: T)(using tc: TC[T])

  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type
  object TC:
    given ofDFType[T <: DFType]: TC[T] with
      type Type = T
      def apply(t: T): Type = t
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
            case t if t <:< TypeRepr.of[DFBoolOrBit] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFBits] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFDecimal] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFVector] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFOpaque] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFTuple] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFEnum] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFStruct] =>
              Some(t)
            case t: AppliedType if t.tycon <:< TypeRepr.of[DFUnion] =>
              Some(t)
            case t if t <:< fieldsTpe =>
              Some(TypeRepr.of[DFStruct].appliedTo(t))
            case t if t <:< TypeRepr.of[DFType] =>
              Some(t)
            case t @ DFEnum(_) =>
              Some(TypeRepr.of[DFEnum].appliedTo(t))
            case t =>
              None

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
          val clsType = entries.head.asType
          clsType match
            case '[e] =>
              '{
                new TC[T]:
                  type Type = DFEnum[T, e]
                  def apply(t: T): Type = DFEnum[T, e](t)
              }
    end tcMacro
  end TC
