package DFiant.compiler
package ir
import DFiant.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag
sealed trait DFType extends Product, Serializable:
  val width: Int
object DFType:
  type Token = DFToken[DFType, Any]
  extension (token: Token)
    def bits: DFBits.Token =
      token.dfType match
        case t: DFBoolOrBit =>
          DFBoolOrBit.Token.toBits(
            t,
            token.data.asInstanceOf[DFBoolOrBit.Token.Data]
          )
        case t: DFBits =>
          DFBits.Token.toBits(
            t,
            token.data.asInstanceOf[DFBits.Token.Data]
          )
        case t: DFDecimal =>
          DFDecimal.Token.toBits(
            t,
            token.data.asInstanceOf[DFDecimal.Token.Data]
          )
        case t: DFEnum =>
          DFEnum.Token.toBits(
            t,
            token.data.asInstanceOf[DFEnum.Token.Data]
          )
        case t: DFVector =>
          DFVector.Token.toBits(
            t,
            token.data.asInstanceOf[DFVector.Token.Data]
          )
        case t: DFOpaque =>
          DFOpaque.Token.toBits(
            t,
            token.data.asInstanceOf[DFOpaque.Token.Data]
          )
        case t: DFUnion =>
          DFUnion.Token.toBits(
            t,
            token.data.asInstanceOf[DFUnion.Token.Data]
          )
        case t: DFStruct =>
          DFStruct.Token.toBits(
            t,
            token.data.asInstanceOf[DFStruct.Token.Data]
          )
        case t: DFTuple =>
          DFTuple.Token.toBits(
            t,
            token.data.asInstanceOf[DFTuple.Token.Data]
          )
  extension (token: DFBits.Token)
    def as(dfType: DFType): DFType.Token =
      dfType match
        case t: DFBoolOrBit =>
          DFBoolOrBit.Token.fromBits(t, token.data)
        case t: DFBits =>
          DFBits.Token.fromBits(t, token.data)
        case t: DFDecimal =>
          DFDecimal.Token.fromBits(t, token.data)
        case t: DFEnum =>
          DFEnum.Token.fromBits(t, token.data)
        case t: DFVector =>
          DFVector.Token.fromBits(t, token.data)
        case t: DFOpaque =>
          DFOpaque.Token.fromBits(t, token.data)
        case t: DFUnion =>
          DFUnion.Token.fromBits(t, token.data)
        case t: DFStruct =>
          DFStruct.Token.fromBits(t, token.data)
        case t: DFTuple =>
          DFTuple.Token.fromBits(t, token.data)

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
      bubbleCreate: T => D,
      dataToBitsData: (T, D) => (BitVector, BitVector),
      bitsDataToData: (T, (BitVector, BitVector)) => D
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
      def toBits(dfType: T, data: D): DFBits.Token =
        DFBits.Token(DFBits(dfType.width), dataToBitsData(dfType, data))
      def fromBits(dfType: T, data: (BitVector, BitVector)): Token =
        Token(dfType, bitsDataToData(dfType, data))
  end Companion
end DFType

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  final val width = 1
object DFBoolOrBit
    extends DFType.Companion[DFBoolOrBit, Option[Boolean]](
      bubbleCreate = _ => None,
      dataToBitsData = (_, d) =>
        d match
          case Some(value) => (BitVector.bit(value), BitVector.low(1))
          case None        => (BitVector.low(1), BitVector.high(1))
      ,
      bitsDataToData = (_, d) =>
        if (d._2.isZeros) Some(!d._1.isZeros)
        else None
    )

case object DFBool extends DFBoolOrBit
case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(val width: Int) extends DFType
object DFBits
    extends DFType.Companion[DFBits, (BitVector, BitVector)](
      bubbleCreate = dfType =>
        (BitVector.low(dfType.width), BitVector.high(dfType.width)),
      dataToBitsData = (_, d) => d,
      bitsDataToData = (_, d) => d
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

object DFDecimal
    extends DFType.Companion[DFDecimal, Option[BigInt]](
      bubbleCreate = _ => None,
      dataToBitsData = (t, d) =>
        d match
          case Some(value) =>
            (value.toBitVector(t.width), BitVector.low(t.width))
          case None => (BitVector.low(t.width), BitVector.high(t.width))
      ,
      bitsDataToData = (t, d) =>
        if (d._2.isZeros)
          (t.signed, t.fractionWidth) match
            //DFUInt
            case (false, 0) => Some(d._1.toBigInt.asUnsigned(t.width))
            //DFSInt
            case (true, 0) => Some(d._1.toBigInt)
            //DFUFix/DFSFix
            case _ => ??? //not supported yet
        else None
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    val name: String,
    val width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType

object DFEnum
    extends DFType.Companion[DFEnum, Option[BigInt]](
      bubbleCreate = _ => None,
      dataToBitsData = (t, d) =>
        d match
          case Some(value) =>
            (value.toBitVector(t.width), BitVector.low(t.width))
          case None => (BitVector.low(t.width), BitVector.high(t.width))
      ,
      bitsDataToData = (t, d) =>
        if (d._2.isZeros) Some(d._1.toBigInt)
        else None
    )
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
    extends DFType.Companion[DFVector, Vector[DFType.Token]](
      bubbleCreate = dfType =>
        Vector.fill(dfType.cellDims.head)(DFType.Token.bubble(dfType.cellType)),
      dataToBitsData = (t, d) =>
        val vecs = d.map(_.bits).map(_.data).unzip
        (vecs._1.reduce(_ ++ _), vecs._2.reduce(_ ++ _)),
      bitsDataToData = (t, d) =>
        val cellWidth = t.cellType.width
        val seq =
          for (i <- 1 to t.width / cellWidth)
            yield DFBits
              .Token(
                DFBits(cellWidth),
                (
                  d._1.bitsWL(cellWidth, t.width - i * cellWidth),
                  d._2.bitsWL(cellWidth, t.width - i * cellWidth)
                )
              )
              .as(t.cellType)
        seq.toVector
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(name: String, actualType: DFType) extends DFType:
  final val width: Int = actualType.width

object DFOpaque
    extends DFType.Companion[DFOpaque, DFType.Token](
      bubbleCreate = dfType => DFType.Token.bubble(dfType.actualType),
      dataToBitsData = (t, d) => d.bits.data,
      bitsDataToData = (t, d) =>
        DFBits.Token(DFBits(t.width), d).as(t.actualType)
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion(fieldSet: ListSet[DFType]) extends DFType:
  val width: Int = fieldSet.head.width

object DFUnion
    extends DFType.Companion[DFUnion, DFType.Token](
      bubbleCreate = dfType => DFType.Token.bubble(dfType.fieldSet.head),
      dataToBitsData = (t, d) => d.bits.data,
      bitsDataToData = (t, d) =>
        DFBits.Token(DFBits(t.width), d).as(t.fieldSet.head)
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
    extends DFType.Companion[DFStruct, List[DFType.Token]](
      bubbleCreate = dfType =>
        dfType.fieldMap.values.map(DFType.Token.bubble).toList,
      dataToBitsData = (t, d) => d.map(_.bits.data).bitsConcat,
      bitsDataToData = (t, d) =>
        var relBitHigh: Int = t.width - 1
        t.fieldMap.values
          .map(fieldType =>
            val relWidth = fieldType.width
            val relBitLow = relBitHigh - relWidth
            relBitHigh = relBitLow - 1
            val valueBits = d._1.bitsWL(relWidth, relBitLow)
            val bubbleBits = d._2.bitsWL(relWidth, relBitLow)
            DFBits
              .Token(DFBits(relWidth), (valueBits, bubbleBits))
              .as(fieldType)
          )
          .toList
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple(fieldList: List[DFType]) extends DFType:
  val width: Int = fieldList.view.map(_.width).sum

object DFTuple
    extends DFType.Companion[DFTuple, List[DFType.Token]](
      bubbleCreate = dfType => dfType.fieldList.map(DFType.Token.bubble),
      dataToBitsData = (t, d) => d.map(_.bits.data).bitsConcat,
      bitsDataToData = (t, d) =>
        var relBitHigh: Int = t.width - 1
        t.fieldList
          .map(fieldType =>
            val relWidth = fieldType.width
            val relBitLow = relBitHigh - relWidth
            relBitHigh = relBitLow - 1
            val valueBits = d._1.bitsWL(relWidth, relBitLow)
            val bubbleBits = d._2.bitsWL(relWidth, relBitLow)
            DFBits
              .Token(DFBits(relWidth), (valueBits, bubbleBits))
              .as(fieldType)
          )
          .toList
    )
/////////////////////////////////////////////////////////////////////////////
