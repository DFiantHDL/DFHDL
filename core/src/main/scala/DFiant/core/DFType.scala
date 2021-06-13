package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

opaque type DFType = ir.DFType
//  type TokenData
//  protected[DFiant] def tokenDataToBits(
//      data: TokenData
//  ): (BitVector, BitVector) = ???
//  protected[DFiant] def tokenBitsToData(
//      valueBits: BitVector,
//      bubbleBits: BitVector
//  ): TokenData = ???
//  protected[DFiant] def tokenCodeString(data: TokenData)(using
//      Printer
//  ): String = ???
//  protected[DFiant] def tokenEquals(
//      lhs: DFToken[?],
//      rhs: DFToken[?]
//  ): DFBool.Token = ???
//  protected[DFiant] def tokenBubble: DFToken[?] = ???

object DFType:
  extension (dfType: DFType) def asIR: ir.DFType = dfType

  opaque type Of[+T <: ir.DFType] <: DFType = DFType
  object Of:
    extension [T <: ir.DFType](of: Of[T]) def asIR: T = of.asInstanceOf[T]

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
      def <>(dir: Int): Unit = {}

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
  transparent inline given ofAnyRef[T <: AnyRef]: TC[T] = ${ tcMacro[T] }
  def tcMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    val dfTypeTpe = TypeRepr.of[DFType]
    val nonEmptyTupleTpe = TypeRepr.of[NonEmptyTuple]
    val fieldTpe = TypeRepr.of[DFField[_]]
    val fieldsTpe = TypeRepr.of[DFFields]
    def checkSupported(tTpe: TypeRepr): Unit = {
      // println((tTpe.show, expr.show))
      tTpe match
        case t if t <:< dfTypeTpe =>
        case applied: AppliedType if applied <:< nonEmptyTupleTpe =>
          applied.args.foreach(checkSupported)
        case t if t <:< fieldsTpe =>
        case DFEnum(_)            =>
        case t =>
          report.error(s"Unsupported dataflow type can be found for: ${t.show}")
    }

    checkSupported(tTpe)
    tTpe match
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
