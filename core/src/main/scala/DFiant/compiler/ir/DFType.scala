package DFiant.compiler
package ir
import DFiant.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag
sealed trait DFType extends Product, Serializable:
  val width: Int
object DFType:
  type Token = DFToken[DFType, Any]
  object Token:
    def bubble(dfType: DFType): Token = dfType match
      case t: DFBoolOrBit => DFBoolOrBit.Token.bubble(t)
      case t: DFBits      => DFBits.Token.bubble(t)
      case t: DFDecimal   => DFDecimal.Token.bubble(t)
      case t: DFEnum      => DFEnum.Token.bubble(t)
      case t: DFVector    => DFVector.Token.bubble(t)
      case t: DFOpaque    => DFOpaque.Token.bubble(t)
      case t: DFUnion     => DFUnion.Token.bubble(t)
      case t: DFStruct    => DFStruct.Token.bubble(t)
      case t: DFTuple     => DFTuple.Token.bubble(t)

  protected[ir] abstract class Companion[T <: DFType, D](
      bubbleCreate: T => D
  )(using ClassTag[T]):
    type Token = DFToken[T, D]
    object Token:
      type Data = D
      def apply(dfType: T, data: D): DFToken[T, D] = DFToken(dfType, data)
      def unapply(token: DFType.Token): Option[(T, D)] =
        token.dfType match
          case dt: T =>
            Some((dt, token.data.asInstanceOf[D]))
          case _ => None
      def bubble(dfType: T): DFToken[T, D] = apply(dfType, bubbleCreate(dfType))

//object DFToken:
//  sealed trait Optional extends DFToken:
//    type Data
//    val data: Option[Data]
//    final lazy val valueBits: BitVector =
//      data match
//        case Some(t) => dataToBitVector(t)
//        case None    => 0.toBitVector(width)
//    final lazy val bubbleBits: BitVector =
//      data match
//        case Some(t) => false.toBitVector(width)
//        case None    => true.toBitVector(width)
//    protected def dataToBitVector(data: Data): BitVector
//    protected def dataCodeString(data: Data)(using Printer): String
//    final def codeString(using Printer): String = data match {
//      case Some(t) => dataCodeString(t)
//      case None    => "?"
//    }

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  final val width = 1
object DFBoolOrBit
    extends DFType.Companion[DFBoolOrBit, Option[Boolean]](_ => None)

case object DFBool extends DFBoolOrBit
case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(val width: Int) extends DFType
object DFBits
    extends DFType.Companion[DFBits, (BitVector, BitVector)](dfType =>
      (BitVector.low(dfType.width), BitVector.high(dfType.width))
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    signed: Boolean,
    width: Int,
    fractionWidth: Int
) extends DFType:
  val magnitudeWidth: Int = width - fractionWidth

object DFDecimal extends DFType.Companion[DFDecimal, Option[BigInt]](_ => None)
/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    val name: String,
    val width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType

object DFEnum extends DFType.Companion[DFEnum, Option[BigInt]](_ => None)
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDims: List[Int]
) extends DFType:
  val width: Int = cellType.width * cellDims.reduce(_ * _)

object DFVector
    extends DFType.Companion[DFVector, Vector[DFType.Token]](dfType =>
      Vector.fill(dfType.cellDims.head)(DFType.Token.bubble(dfType.cellType))
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(name: String, actualType: DFType) extends DFType:
  final val width: Int = actualType.width

object DFOpaque
    extends DFType.Companion[DFOpaque, DFType.Token](dfType =>
      DFType.Token.bubble(dfType.actualType)
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion(fieldSet: ListSet[DFType]) extends DFType:
  val width: Int = fieldSet.head.width

object DFUnion
    extends DFType.Companion[DFUnion, DFType.Token](dfType =>
      DFType.Token.bubble(dfType.fieldSet.head)
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct(
    name: String,
    fieldMap: ListMap[String, DFType]
) extends DFType:
  val width: Int = fieldMap.values.map(_.width).sum

object DFStruct
    extends DFType.Companion[DFStruct, List[DFType.Token]](dfType =>
      dfType.fieldMap.values.map(DFType.Token.bubble).toList
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple(fieldList: List[DFType]) extends DFType:
  val width: Int = fieldList.view.map(_.width).sum

object DFTuple
    extends DFType.Companion[DFTuple, List[DFType.Token]](dfType =>
      dfType.fieldList.map(DFType.Token.bubble)
    )
/////////////////////////////////////////////////////////////////////////////
