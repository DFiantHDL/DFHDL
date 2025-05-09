package dfhdl.compiler
package ir
import dfhdl.internals.*

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag
import scala.util.boundary, boundary.break

sealed trait DFType extends Product, Serializable, HasRefCompare[DFType] derives CanEqual:
  type Data
  def width(using MemberGetSet): Int
  def createBubbleData(using MemberGetSet): Data
  def isDataBubble(data: Data): Boolean
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector)
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean
  lazy val getRefs: List[DFRef.TypeRef]

object DFType:
  type Aux[T <: DFType, Data0] = DFType { type Data = Data0 }

  protected[ir] abstract class Companion[T <: DFType, D](using ClassTag[T]):
    object Data:
      def unapply(dfTypeAndData: (DFType, Any)): Option[(T, D)] =
        dfTypeAndData match
          case (dt: T, data: D @unchecked) =>
            Some(dt, data)
          case _ => None
    object Val:
      def unapply(arg: DFVal): Option[T] =
        arg.dfType match
          case dt: T => Some(dt)
          case _     => None
  end Companion

  extension (dfType: DFType)
    def decompose[B <: DFType](pf: PartialFunction[DFType, B] = a => a)(using
        MemberGetSet
    ): ListSet[B] =
      val deps: Iterable[DFType] = dfType match
        case dt: DFStruct =>
          dt.fieldMap.values.flatMap(_.decompose(pf))
        case dt: DFOpaque =>
          dt.actualType.decompose(pf)
        case dt: DFVector =>
          dt.cellType.decompose(pf)
        case _ => Nil
      ListSet.from(deps.collect(pf) ++ List(dfType).collect(pf))

end DFType

sealed trait ComposedDFType extends DFType
sealed trait NamedDFType extends DFType, NamedGlobal
object NamedDFTypes:
  def unapply(dfVal: DFVal)(using MemberGetSet): Option[ListSet[NamedDFType]] =
    Flatten.unapply(dfVal.dfType)
  object Flatten:
    def unapply(dfType: DFType)(using MemberGetSet): Option[ListSet[NamedDFType]] =
      dfType match
        case dt: DFStruct =>
          val subNamedDFTypes = ListSet.from(dt.fieldMap.values.flatMap {
            case Flatten(dfTypes) => dfTypes
            case _                => Nil
          })
          Some(subNamedDFTypes + dt)
        case dt: DFOpaque =>
          dt.actualType match
            case Flatten(dfTypes) => Some(dfTypes + dt)
            case _                => Some(ListSet(dt))
        case dt: DFVector    => unapply(dt.cellType)
        case dt: NamedDFType => Some(ListSet(dt))
        case _               => None
  end Flatten
end NamedDFTypes

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  type Data = Option[Boolean]
  def width(using MemberGetSet): Int = 1
  def createBubbleData(using MemberGetSet): Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = data match
    case Some(value) => (BitVector.bit(value), BitVector.low(1))
    case None        => (BitVector.low(1), BitVector.high(1))
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    if (data._2.isZeros) Some(!data._1.isZeros)
    else None
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = this equals that
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = this equals that
  lazy val getRefs: List[DFRef.TypeRef] = Nil
  def copyWithNewRefs: this.type = this
end DFBoolOrBit

object DFBoolOrBit extends DFType.Companion[DFBoolOrBit, Option[Boolean]]

case object DFBool extends DFBoolOrBit
case object DFBit extends DFBoolOrBit
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(widthParamRef: IntParamRef) extends DFType:
  type Data = (BitVector, BitVector)
  def width(using MemberGetSet): Int = widthParamRef.getInt
  def createBubbleData(using MemberGetSet): Data = (BitVector.low(width), BitVector.high(width))
  def isDataBubble(data: Data): Boolean = !data._2.isZeros
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = data
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data = data
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFBits =>
      this.widthParamRef =~ that.widthParamRef
    case _ => false
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFBits =>
      this.widthParamRef.isSimilarTo(that.widthParamRef)
    case _ => false
  lazy val getRefs: List[DFRef.TypeRef] = widthParamRef.getRef.toList
  def copyWithNewRefs: this.type = copy(widthParamRef.copyAsNewRef).asInstanceOf[this.type]
end DFBits

object DFBits extends DFType.Companion[DFBits, (BitVector, BitVector)]:
  def apply(width: Int): DFBits = DFBits(IntParamRef(width))
  def dataFromBinString(
      bin: String
  ): Either[String, (BitVector, BitVector)] = boundary {
    val (valueBits, bubbleBits) =
      bin.foldLeft((BitVector.empty, BitVector.empty)) {
        case (t, '_' | ' ') => t // ignoring underscore or space
        case ((v, b), c) =>
          c match // bin mode
            case '?' => (v :+ false, b :+ true)
            case '0' => (v :+ false, b :+ false)
            case '1' => (v :+ true, b :+ false)
            case x =>
              break(Left(s"Found invalid binary character: $x"))
      }
    Right((valueBits, bubbleBits))
  }
  private val isHex = "[0-9a-fA-F]".r
  def dataFromHexString(
      hex: String
  ): Either[String, (BitVector, BitVector)] = boundary {
    val (valueBits, bubbleBits, binMode) =
      hex.foldLeft((BitVector.empty, BitVector.empty, false)) {
        case (t, '_' | ' ') => t // ignoring underscore or space
        case ((v, b, false), c) =>
          c match // hex mode
            case '{' => (v, b, true)
            case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
            case isHex() =>
              (
                v ++ BitVector.fromHex(c.toString).get,
                b ++ BitVector.low(4),
                false
              )
            case x =>
              break(Left(s"Found invalid hex character: $x"))
        case ((v, b, true), c) =>
          c match // bin mode
            case '}' => (v, b, false)
            case '?' => (v :+ false, b :+ true, true)
            case '0' => (v :+ false, b :+ false, true)
            case '1' => (v :+ true, b :+ false, true)
            case x =>
              break(Left(s"Found invalid binary character in binary mode: $x"))
      }
    if (binMode) Left(s"Missing closing braces of binary mode")
    else Right((valueBits, bubbleBits))
  }
end DFBits

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    signed: Boolean,
    widthParamRef: IntParamRef,
    fractionWidth: Int,
    // currently nativeType only applies when width is 32-bit and is indicating
    // an `Int` in DFHDL, an `integer` in VHDL, and `int` in Verilog
    nativeType: DFDecimal.NativeType
) extends DFType:
  type Data = Option[BigInt]
  def width(using MemberGetSet): Int = widthParamRef.getInt
  def magnitudeWidth(using MemberGetSet): Int = width - fractionWidth
  def isDFInt32: Boolean = this == DFInt32
  def createBubbleData(using MemberGetSet): Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = data match
    case Some(value) => (value.toBitVector(width), BitVector.low(width))
    case None        => (BitVector.low(width), BitVector.high(width))
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    if (data._2.isZeros)
      (signed, fractionWidth) match
        // DFUInt
        case (false, 0) => Some(data._1.toBigInt(false).asUnsigned(width))
        // DFSInt
        case (true, 0) => Some(data._1.toBigInt(true))
        // DFUFix/DFSFix
        case _ => ??? // not supported yet
    else None
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFDecimal =>
      this.signed == that.signed && this.widthParamRef =~ that.widthParamRef &&
      this.fractionWidth == that.fractionWidth && this.nativeType == that.nativeType
    case _ => false
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFDecimal =>
      this.signed == that.signed && this.widthParamRef.isSimilarTo(that.widthParamRef) &&
      this.fractionWidth == that.fractionWidth && this.nativeType == that.nativeType
    case _ => false
  lazy val getRefs: List[DFRef.TypeRef] = widthParamRef.getRef.toList
  def copyWithNewRefs: this.type =
    copy(widthParamRef = widthParamRef.copyAsNewRef).asInstanceOf[this.type]
end DFDecimal

object DFDecimal extends DFType.Companion[DFDecimal, Option[BigInt]]:
  enum NativeType derives CanEqual:
    case BitAccurate, Int32
  object NativeType:
    type BitAccurate = BitAccurate.type
    type Int32 = Int32.type
end DFDecimal

import DFDecimal.NativeType
import NativeType.*
object DFXInt:
  def apply(signed: Boolean, width: IntParamRef, nativeType: NativeType): DFDecimal =
    DFDecimal(signed, width, 0, nativeType)
  def unapply(dfType: DFDecimal): Option[(Boolean, IntParamRef, NativeType)] = dfType match
    case DFDecimal(signed, width, 0, nativeType) => Some(signed, width, nativeType)
    case _                                       => None

object DFUInt:
  def apply(width: IntParamRef): DFDecimal = DFDecimal(false, width, 0, BitAccurate)
  def unapply(arg: DFDecimal): Option[IntParamRef] =
    arg match
      case DFDecimal(false, width, 0, BitAccurate) => Some(width)
      case _                                       => None

object DFSInt:
  def apply(width: IntParamRef): DFDecimal = DFDecimal(true, width, 0, BitAccurate)
  def unapply(arg: DFDecimal): Option[IntParamRef] =
    arg match
      case DFDecimal(true, width, 0, BitAccurate) => Some(width)
      case _                                      => None

final val DFInt32 = ir.DFDecimal(true, ir.IntParamRef(32), 0, Int32)
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    protected val name: String,
    widthParam: Int,
    entries: ListMap[String, BigInt]
) extends NamedDFType:
  type Data = Option[BigInt]
  def width(using MemberGetSet): Int = widthParam
  def createBubbleData(using MemberGetSet): Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = data match
    case Some(value) => (value.toBitVector(width), BitVector.low(width))
    case None        => (BitVector.low(width), BitVector.high(width))
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    if (data._2.isZeros) Some(data._1.toBigInt(false))
    else None
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = this equals that
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = this equals that
  lazy val getRefs: List[DFRef.TypeRef] = Nil
  def copyWithNewRefs: this.type = this
end DFEnum

object DFEnum extends DFType.Companion[DFEnum, Option[BigInt]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDimParamRefs: List[IntParamRef]
) extends ComposedDFType:
  type Data = Vector[Any]
  def width(using MemberGetSet): Int = cellType.width * cellDimParamRefs.map(_.getInt).product
  // TODO: change for multidimensional arrays
  def length(using MemberGetSet): Int = cellDimParamRefs.head.getInt
  def createBubbleData(using MemberGetSet): Data = Vector.fill(length)(
    cellType.createBubbleData
  )
  def isDataBubble(data: Data): Boolean =
    data.exists(x => cellType.isDataBubble(x.asInstanceOf[cellType.Data]))
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) =
    val vecs = data
      .map(d => cellType.dataToBitsData(d.asInstanceOf[cellType.Data]))
      .unzip
    (vecs._1.reduce(_ ++ _), vecs._2.reduce(_ ++ _))
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    val cellWidth = cellType.width
    val seq =
      val bound = width / cellWidth
      for (i <- 1 to bound)
        yield cellType.bitsDataToData(
          data._1.bitsWL(cellWidth, width - i * cellWidth),
          data._2.bitsWL(cellWidth, width - i * cellWidth)
        )
    seq.toVector
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFVector =>
      this.cellType =~ that.cellType &&
      this.cellDimParamRefs.lazyZip(that.cellDimParamRefs).forall(_ =~ _)
    case _ => false
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFVector =>
      this.cellType.isSimilarTo(that.cellType) &&
      this.cellDimParamRefs.lazyZip(that.cellDimParamRefs).forall(_.isSimilarTo(_))
    case _ => false
  lazy val getRefs: List[DFRef.TypeRef] = cellType.getRefs ++ cellDimParamRefs.flatMap(_.getRef)
  def copyWithNewRefs: this.type = copy(
    cellType = cellType.copyWithNewRefs,
    cellDimParamRefs = cellDimParamRefs.map(_.copyAsNewRef)
  ).asInstanceOf[this.type]
end DFVector

object DFVector extends DFType.Companion[DFVector, Vector[Any]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(protected val name: String, id: DFOpaque.Id, actualType: DFType)
    extends NamedDFType,
      ComposedDFType:
  type Data = Any
  def width(using MemberGetSet): Int = actualType.width
  def isMagnet: Boolean = id match
    case _: DFOpaque.MagnetId => true
    case _                    => false
  def createBubbleData(using MemberGetSet): Data = actualType.createBubbleData
  def isDataBubble(data: Data): Boolean =
    actualType.isDataBubble(data.asInstanceOf[actualType.Data])
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) =
    actualType.dataToBitsData(data.asInstanceOf[actualType.Data])
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    actualType.bitsDataToData(data)
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFOpaque =>
      this.getName == that.getName && this.id == that.id &&
      this.actualType =~ that.actualType
    case _ => false
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFOpaque =>
      this.getName == that.getName && this.id == that.id &&
      this.actualType.isSimilarTo(that.actualType)
    case _ => false
  lazy val getRefs: List[DFRef.TypeRef] = actualType.getRefs
  def copyWithNewRefs: this.type = copy(
    actualType = actualType.copyWithNewRefs
  ).asInstanceOf[this.type]
end DFOpaque

object DFOpaque extends DFType.Companion[DFOpaque, Any]:
  trait Id extends Product, Serializable derives CanEqual
  // for types to auto-connected, like Clk and Rst
  trait MagnetId extends Id
  trait Clk extends MagnetId
  trait Rst extends MagnetId
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct(
    protected val name: String,
    fieldMap: ListMap[String, DFType]
) extends NamedDFType,
      ComposedDFType:
  type Data = List[Any]
  def getNameForced: String = name
  def width(using MemberGetSet): Int = fieldMap.values.map(_.width).sum
  def createBubbleData(using MemberGetSet): Data = fieldMap.values.map(_.createBubbleData).toList
  def isDataBubble(data: Data): Boolean =
    (fieldMap.values lazyZip data).exists((ft, fd) => ft.isDataBubble(fd.asInstanceOf[ft.Data]))
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) =
    (fieldMap.values lazyZip data)
      .map((ft, fd) => ft.dataToBitsData(fd.asInstanceOf[ft.Data]))
      .bitsConcat
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
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
  private var fieldPosMap: Map[String, Int] = Map()
  def fieldRelBitLow(fieldName: String)(using MemberGetSet): Int =
    if (fieldPosMap.isEmpty)
      var relBitHigh: Int = width - 1
      fieldPosMap = fieldMap.map((fieldName, fieldType) =>
        val relWidth = fieldType.width
        val relBitLow = relBitHigh - relWidth + 1
        relBitHigh = relBitLow - 1
        (fieldName, relBitLow)
      )
    fieldPosMap(fieldName)
  lazy val fieldIndexes: Map[String, Int] = fieldMap.keys.zipWithIndex.toMap
  def fieldIndex(fieldName: String): Int = fieldIndexes(fieldName)
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFStruct =>
      this.getName == that.getName &&
      this.fieldMap.lazyZip(that.fieldMap).forall { case ((fnL, ftL), (fnR, ftR)) =>
        fnL == fnR && ftL =~ ftR
      }
    case _ => false
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = that match
    case that: DFStruct =>
      this.getName == that.getName &&
      this.fieldMap.lazyZip(that.fieldMap).forall { case ((fnL, ftL), (fnR, ftR)) =>
        fnL == fnR && ftL.isSimilarTo(ftR)
      }
    case _ => false
  lazy val getRefs: List[DFRef.TypeRef] = fieldMap.values.flatMap(_.getRefs).toList
  def copyWithNewRefs: this.type = copy(
    fieldMap = ListMap.from(fieldMap.view.mapValues(_.copyWithNewRefs))
  ).asInstanceOf[this.type]
end DFStruct

object DFStruct extends DFType.Companion[DFStruct, List[Any]]:
  extension (dfType: DFStruct) def isTuple: Boolean = dfType.name.startsWith("DFTuple")
object DFTuple:
  def structName(length: Int): String = s"DFTuple$length"
  def fieldName(idx: Int): String = s"_${idx + 1}"
  def apply(fieldList: List[DFType]): DFStruct =
    DFStruct(
      structName(fieldList.length),
      ListMap.from(fieldList.view.zipWithIndex.map((f, i) => (fieldName(i), f)))
    )
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFDouble
/////////////////////////////////////////////////////////////////////////////
sealed trait DFDouble extends DFType:
  type Data = Option[Double]
  def width(using MemberGetSet): Int = 64
  def createBubbleData(using MemberGetSet): Data = None
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = data match
    case Some(value) =>
      (
        BitVector.fromLong(java.lang.Double.doubleToRawLongBits(value), size = 64),
        BitVector.low(64)
      )
    case None => (BitVector.low(64), BitVector.high(64))
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data =
    if (data._2.isZeros) Some(java.lang.Double.longBitsToDouble(data._1.toLong(signed = false)))
    else None
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = this equals that
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = this equals that
  lazy val getRefs: List[DFRef.TypeRef] = Nil
  def copyWithNewRefs: this.type = this
end DFDouble

case object DFDouble extends DFType.Companion[DFDouble, Option[Double]] with DFDouble
/////////////////////////////////////////////////////////////////////////////

sealed trait DFUnbounded extends DFType:
  def noTypeErr = throw new IllegalArgumentException(
    s"Unexpected access to $this data type"
  )
  def width(using MemberGetSet): Int = noTypeErr
  def dataToBitsData(data: Data)(using MemberGetSet): (BitVector, BitVector) = noTypeErr
  def bitsDataToData(data: (BitVector, BitVector))(using MemberGetSet): Data = noTypeErr
  protected def `prot_=~`(that: DFType)(using MemberGetSet): Boolean = this equals that
  def isSimilarTo(that: DFType)(using MemberGetSet): Boolean = this equals that
  lazy val getRefs: List[DFRef.TypeRef] = Nil
  def copyWithNewRefs: this.type = this

/////////////////////////////////////////////////////////////////////////////
// DFUnit
// ------
// This meant to be just a DFType placeholder where no type is actually
// useful. E.g., the return value of an IfElse block that has no valid
// DFHDL return value.
/////////////////////////////////////////////////////////////////////////////
sealed trait DFUnit extends DFUnbounded:
  type Data = Unit
  def isDataBubble(data: Data): Boolean = noTypeErr
  def createBubbleData(using MemberGetSet): Data = noTypeErr

case object DFUnit extends DFType.Companion[DFUnit, Unit] with DFUnit
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFNothing
// ------
// This is for a temporary placeholder where no actual value is available.
/////////////////////////////////////////////////////////////////////////////
sealed trait DFNothing extends DFUnbounded:
  type Data = Nothing
  def isDataBubble(data: Data): Boolean = noTypeErr
  def createBubbleData(using MemberGetSet): Data = noTypeErr
case object DFNothing extends DFType.Companion[DFNothing, Nothing] with DFNothing
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFPhysical
/////////////////////////////////////////////////////////////////////////////
final case class DFPhysical(unit: DFPhysical.Unit) extends DFUnbounded:
  //             value    scale
  type Data = (BigDecimal, Any)
  def isDataBubble(data: Data): Boolean = false
  def createBubbleData(using MemberGetSet): Data = noTypeErr

object DFPhysical extends DFType.Companion[DFPhysical, (BigDecimal, Any)]:
  sealed trait Unit extends Product, Serializable derives CanEqual
  object Unit:
    case object Time extends Unit:
      enum Scale derives CanEqual:
        case hr, min, sec, ms, us, ns, ps, fs
        def to_ps(value: BigDecimal): BigDecimal =
          this match
            case DFPhysical.Unit.Time.Scale.fs  => value / BigDecimal(1000)
            case DFPhysical.Unit.Time.Scale.ps  => value
            case DFPhysical.Unit.Time.Scale.ns  => value * BigDecimal(1000)
            case DFPhysical.Unit.Time.Scale.us  => value * BigDecimal(1000000)
            case DFPhysical.Unit.Time.Scale.ms  => value * BigDecimal(1000000000)
            case DFPhysical.Unit.Time.Scale.sec => value * BigDecimal(1000000000000L)
            case DFPhysical.Unit.Time.Scale.min => value * BigDecimal(60000000000000L)
            case DFPhysical.Unit.Time.Scale.hr  => value * BigDecimal(3600000000000000L)

    case object Number extends Unit
    case object Freq extends Unit:
      enum Scale derives CanEqual:
        case Hz, KHz, MHz, GHz
        def to_hz(value: BigDecimal): BigDecimal =
          this match
            case DFPhysical.Unit.Freq.Scale.Hz  => value
            case DFPhysical.Unit.Freq.Scale.KHz => value * BigDecimal(1000)
            case DFPhysical.Unit.Freq.Scale.MHz => value * BigDecimal(1000000)
            case DFPhysical.Unit.Freq.Scale.GHz => value * BigDecimal(1000000000)
        def to_ps(value: BigDecimal): BigDecimal =
          BigDecimal(1e12) / to_hz(value)
        def to_period(value: BigDecimal): (BigDecimal, Unit.Time.Scale) =
          val psVal = to_ps(value)
          if psVal < BigDecimal(1000) then (psVal, Unit.Time.Scale.ps)
          else if psVal < BigDecimal(1000000) then (psVal / 1000, Unit.Time.Scale.ns)
          else if psVal < BigDecimal(1000000000) then (psVal / 1000000, Unit.Time.Scale.us)
          else if psVal < 1000000000000L then (psVal / 1000000000L, Unit.Time.Scale.ms)
          else if psVal < 1000000000000000L then (psVal / 1000000000000L, Unit.Time.Scale.sec)
          else (psVal / 60000000000000L, Unit.Time.Scale.min)
      end Scale
    end Freq
  end Unit
end DFPhysical

val DFTime = DFPhysical(DFPhysical.Unit.Time)
val DFFreq = DFPhysical(DFPhysical.Unit.Freq)
val DFNumber = DFPhysical(DFPhysical.Unit.Number)
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFString
/////////////////////////////////////////////////////////////////////////////
sealed trait DFString extends DFUnbounded:
  type Data = Option[String]
  def isDataBubble(data: Data): Boolean = data.isEmpty
  def createBubbleData(using MemberGetSet): Data = None
case object DFString extends DFType.Companion[DFString, Option[String]] with DFString
/////////////////////////////////////////////////////////////////////////////
