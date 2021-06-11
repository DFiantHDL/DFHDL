package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import compiletime.*
import scala.quoted.*
import collection.mutable
import collection.immutable.ListMap

type DFType = ir.DFType
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
  private[core] def apply(t: Any): DFType =
    t match
      case dfType: DFType       => dfType
      case tuple: NonEmptyTuple => DFTuple(tuple)
      case fields: DFFields     => DFStruct(fields)
      //TODO: need to add proper upper-bound if fixed in Scalac
      //see: https://contributors.scala-lang.org/t/missing-dedicated-class-for-enum-companions
      case enumCompanion: AnyRef => DFEnum(enumCompanion)

  def tcMacro[T <: AnyRef](using Quotes, Type[T]): Expr[TC[T]] =
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    val dfTypeTpe = TypeRepr.of[ir.DFType]
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

  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type

  given ofDFType[T <: DFType]: TC[T] with
    type Type = T
    def apply(t: T): Type = t
  given ofDFFields[T <: DFFields]: TC[T] with
    type Type = DFStruct[T]
    def apply(t: T): Type = DFStruct[T](t)
  transparent inline given ofAnyRef[T <: AnyRef]: TC[T] = ${ tcMacro[T] }

  type Supported = AnyRef //DFType | NonEmptyTuple
  object Ops:
    extension [T <: Supported](t: T)(using tc: TC[T])
      def dfType: tc.Type = tc(t)
      def width(using w: Width[T]): Inlined.Int[w.Out] =
        Inlined.Int.forced[w.Out](dfType.__width)
// transparent inline def X(inline cellDim: Int*): DFType =
//   x(dfType, cellDim: _*)
      inline def X(
          inline cellDim: Int
      ): DFVector[tc.Type, Tuple1[cellDim.type]] =
        DFVector(dfType, Tuple1(cellDim))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int
      ): DFVector[tc.Type, Tuple2[cellDim0.type, cellDim1.type]] =
        DFVector(dfType, Tuple2(cellDim0, cellDim1))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int,
          inline cellDim2: Int
      ): DFVector[tc.Type, Tuple3[
        cellDim0.type,
        cellDim1.type,
        cellDim2.type
      ]] =
        DFVector(dfType, Tuple3(cellDim0, cellDim1, cellDim2))
      def opaque(using
          meta: MetaContext,
          uniqueId: UniqueId
      ): DFOpaque[tc.Type, uniqueId.Out] =
        DFOpaque[tc.Type, uniqueId.Out](tc(t))
      def <>(dir: Int): Unit = {}

trait UniqueId:
  type Out
object UniqueId:
  transparent inline given UniqueId = ${ getUniqueId }
  var id = 0
  def getUniqueId(using Quotes): Expr[UniqueId] =
    import quotes.reflect.*
    val idTpe = ConstantType(IntConstant(id)).asType.asInstanceOf[Type[Int]]
    id = id + 1
    '{
      new UniqueId {
        type Out = idTpe.Underlying
      }
    }

// transparent inline def x[T <: DFType](
//     cellType: T,
//     inline cellDim: Int*
// ): DFType =
//   ${ xMacro('cellType, 'cellDim) }
// def xMacro[T <: DFType](cellType: Expr[T], cellDim: Expr[Seq[Int]])(using
//     Quotes,
//     Type[T]
// ): Expr[DFType] =
//   import quotes.reflect.*
//   val (tpe, tpl) = cellDim match
//     case Varargs(argExprs) =>
//       argExprs match
//         case arg :: Nil =>
//           println(arg)
//           val tp = ConstantType(IntConstant(5)).asType
//           (
//             TypeRepr.of[Tuple1[5]].asType.asInstanceOf[Type[Int]],
//             '{ Tuple1($arg) }
//           )
//   '{ new DFVector[T, tpe.Underlying]($cellType, $tpl) }
