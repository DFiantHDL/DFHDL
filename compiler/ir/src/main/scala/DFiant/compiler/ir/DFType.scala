package DFiant.compiler
package ir
import DFiant.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag

sealed trait DFType extends Product, Serializable derives CanEqual:
  type Data
  val width: Int
  def createBubbleData: Data
  def isDataBubble(data: Data): Boolean
  def dataToBitsData(data: Data): (BitVector, BitVector)
  def bitsDataToData(data: (BitVector, BitVector)): Data

object DFType:
  type Aux[T <: DFType, Data0] = DFType { type Data = Data0 }

  protected[ir] abstract class Companion[T <: DFType, D](using ClassTag[T]):
    type Token = DFToken[T]
    object Token:
      type Data = D
      def apply(dfType: T, data: D): DFToken[T] =
        DFToken.forced(dfType, data)
      def unapply(token: DFTokenAny): Option[(T, D)] =
        token.dfType match
          case dt: T =>
            Some((dt, token.data.asInstanceOf[D]))
          case _ => None
    object Val:
      def unapply(arg: DFVal): Option[T] =
        arg.dfType match
          case dt: T => Some(dt)
          case _     => None
  end Companion
end DFType

sealed trait NamedDFType extends DFType:
  protected val name: String
  def getName(using getSet: MemberGetSet): String = getSet.getGlobalTag[NameTag](this) match
    case Some(NameTag(taggedName)) => taggedName
    case _                         => name
object NamedDFTypes:
  def unapply(dfVal: DFVal)(using MemberGetSet): Option[Set[NamedDFType]] =
    Flatten.unapply(dfVal.dfType)
  object Flatten:
    def unapply(dfType: DFType)(using MemberGetSet): Option[Set[NamedDFType]] =
      dfType match
        case dt: DFStruct =>
          val subNamedDFTypes = dt.fieldMap.values.flatMap {
            case Flatten(dfTypes) => dfTypes
            case _                => Set()
          }.toSet
          if (dt.isTuple) Some(subNamedDFTypes)
          else Some(subNamedDFTypes + dt)
        case dt: DFOpaque =>
          dt.actualType match
            case Flatten(dfTypes) => Some(dfTypes + dt)
            case _                => Some(Set(dt))
        case dt: NamedDFType => Some(Set(dt))
        case _               => None
  end Flatten
end NamedDFTypes

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  type Data = Option[Boolean]
  final val width = 1
  def createBubbleData: Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data): (BitVector, BitVector) = data match
    case Some(value) => (BitVector.bit(value), BitVector.low(1))
    case None        => (BitVector.low(1), BitVector.high(1))
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    if (data._2.isZeros) Some(!data._1.isZeros)
    else None

object DFBoolOrBit extends DFType.Companion[DFBoolOrBit, Option[Boolean]]

case object DFBool extends DFBoolOrBit
case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(width: Int) extends DFType:
  type Data = (BitVector, BitVector)
  def createBubbleData: Data = (BitVector.low(width), BitVector.high(width))
  def isDataBubble(data: Data): Boolean = !data._2.isZeros
  def dataToBitsData(data: Data): (BitVector, BitVector) = data
  def bitsDataToData(data: (BitVector, BitVector)): Data = data

object DFBits extends DFType.Companion[DFBits, (BitVector, BitVector)]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    signed: Boolean,
    width: Int,
    fractionWidth: Int
) extends DFType:
  type Data = Option[BigInt]
  val magnitudeWidth: Int = width - fractionWidth
  def createBubbleData: Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data): (BitVector, BitVector) = data match
    case Some(value) => (value.toBitVector(width), BitVector.low(width))
    case None        => (BitVector.low(width), BitVector.high(width))
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    if (data._2.isZeros)
      (signed, fractionWidth) match
        // DFUInt
        case (false, 0) => Some(data._1.toBigInt(false).asUnsigned(width))
        // DFSInt
        case (true, 0) => Some(data._1.toBigInt(true))
        // DFUFix/DFSFix
        case _ => ??? // not supported yet
    else None
end DFDecimal

object DFDecimal extends DFType.Companion[DFDecimal, Option[BigInt]]

object DFUInt:
  def apply(width: Int): DFDecimal = DFDecimal(false, width, 0)
  def unapply(arg: DFDecimal): Option[Int] =
    arg match
      case DFDecimal(false, width, 0) => Some(width)
      case _                          => None

object DFSInt:
  def apply(width: Int): DFDecimal = DFDecimal(true, width, 0)
  def unapply(arg: DFDecimal): Option[Int] =
    arg match
      case DFDecimal(true, width, 0) => Some(width)
      case _                         => None
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    protected val name: String,
    width: Int,
    entries: ListMap[String, BigInt]
) extends NamedDFType:
  type Data = Option[BigInt]
  def createBubbleData: Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data): (BitVector, BitVector) = data match
    case Some(value) => (value.toBitVector(width), BitVector.low(width))
    case None        => (BitVector.low(width), BitVector.high(width))
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    if (data._2.isZeros) Some(data._1.toBigInt(false))
    else None

object DFEnum extends DFType.Companion[DFEnum, Option[BigInt]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDims: List[Int]
) extends DFType:
  type Data = Vector[Any]
  val width: Int = cellType.width * cellDims.product
  def createBubbleData: Data = Vector.fill(cellDims.head)(
    cellType.createBubbleData
  )
  def isDataBubble(data: Data): Boolean =
    data.exists(x => cellType.isDataBubble(x.asInstanceOf[cellType.Data]))
  def dataToBitsData(data: Data): (BitVector, BitVector) =
    val vecs = data
      .map(d => cellType.dataToBitsData(d.asInstanceOf[cellType.Data]))
      .unzip
    (vecs._1.reduce(_ ++ _), vecs._2.reduce(_ ++ _))
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    val cellWidth = cellType.width
    val seq =
      for (i <- 1 to width / cellWidth)
        yield cellType.bitsDataToData(
          data._1.bitsWL(cellWidth, width - i * cellWidth),
          data._2.bitsWL(cellWidth, width - i * cellWidth)
        )
    seq.toVector
end DFVector

object DFVector extends DFType.Companion[DFVector, Vector[Any]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(protected val name: String, actualType: DFType) extends NamedDFType:
  type Data = Any
  final val width: Int = actualType.width
  def createBubbleData: Data = actualType.createBubbleData
  def isDataBubble(data: Data): Boolean =
    actualType.isDataBubble(data.asInstanceOf[actualType.Data])
  def dataToBitsData(data: Data): (BitVector, BitVector) =
    actualType.dataToBitsData(data.asInstanceOf[actualType.Data])
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    actualType.bitsDataToData(data)

object DFOpaque extends DFType.Companion[DFOpaque, Any]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct(
    protected val name: String,
    fieldMap: ListMap[String, DFType]
) extends NamedDFType:
  type Data = List[Any]
  val width: Int = fieldMap.values.map(_.width).sum
  def createBubbleData: Data = fieldMap.values.map(_.createBubbleData).toList
  def isDataBubble(data: Data): Boolean =
    (fieldMap.values lazyZip data).exists((ft, fd) => ft.isDataBubble(fd.asInstanceOf[ft.Data]))
  def dataToBitsData(data: Data): (BitVector, BitVector) =
    (fieldMap.values lazyZip data)
      .map((ft, fd) => ft.dataToBitsData(fd.asInstanceOf[ft.Data]))
      .bitsConcat
  def bitsDataToData(data: (BitVector, BitVector)): Data =
    var relBitHigh: Int = width - 1
    fieldMap.values
      .map(fieldType =>
        val relWidth = fieldType.width
        val relBitLow = relBitHigh - relWidth + 1
        relBitHigh = relBitLow - 1
        val valueBits = data._1.bitsWL(relWidth, relBitLow)
        val bubbleBits = data._2.bitsWL(relWidth, relBitLow)
        fieldType.bitsDataToData((valueBits, bubbleBits))
      )
      .toList
end DFStruct

object DFStruct extends DFType.Companion[DFStruct, List[Any]]:
  extension (dfType: DFStruct) def isTuple: Boolean = dfType.name.isEmpty
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// NoType
// ------
// This meant to be just a DFType placeholder where no type is actually
// useful. E.g., the return value of an IfElse block that has no valid
// dataflow return value.
/////////////////////////////////////////////////////////////////////////////
case object NoType extends DFType:
  type Data = Nothing
  final val width = 0
  def noTypeErr = throw new IllegalArgumentException(
    "Unexpected access to `NoType`"
  )
  def createBubbleData: Data = noTypeErr
  def isDataBubble(data: Data): Boolean = noTypeErr
  def dataToBitsData(data: Data): (BitVector, BitVector) = noTypeErr
  def bitsDataToData(data: (BitVector, BitVector)): Data = noTypeErr
/////////////////////////////////////////////////////////////////////////////
