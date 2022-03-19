package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*

trait AbstractTokenPrinter extends AbstractPrinter:
  val allowBitsBinModeInHex: Boolean
  val allowBitsExplicitWidth: Boolean
  def csDFBitBubbleChar: Char
  def csDFBitsBinFormat(binRep: String): String
  def csDFBitsHexFormat(hexRep: String): String
  def csDFBitsHexFormat(hexRep: String, width: Int): String
  final def csDFBitsData(dfType: DFBits, data: (BitVector, BitVector)): String =
    val valueBits: BitVector = data._1
    val bubbleBits: BitVector = data._2
    val width = dfType.width
    def binZip(v: BitVector, b: BitVector): String =
      v.toBin
        .zip(b.toBin)
        .map {
          case (_, '1')       => csDFBitBubbleChar
          case (zeroOrOne, _) => zeroOrOne
        }
        .mkString
    end binZip
    def hexZip(
        v: BitVector,
        b: BitVector
    ): Option[String] = Some(
      v.toHex
        .zip(b.toHex)
        .flatMap {
          case (_, 'F' | 'f')                  => s"$csDFBitBubbleChar"
          case (h, '0')                        => s"$h"
          case (h, b) if allowBitsBinModeInHex => s"{${binZip(BitVector(h), BitVector(b))}}"
          case _                               => return None
        }
        .mkString
    )
    end hexZip
    def toBinString: String = binZip(valueBits, bubbleBits)
    def toHexString: Option[String] =
      if (width % 4 == 0) hexZip(valueBits, bubbleBits)
      else
        val headWidth = width % 4
        val (headValue, theRestValue) = valueBits.splitAt(headWidth)
        val (headBubble, theRestBubble) = bubbleBits.splitAt(headWidth)
        val headOption =
          if (headBubble === BitVector.high(headWidth)) Some(s"$csDFBitBubbleChar")
          else hexZip(headValue.resize(4), headBubble.resize(4))
        val theRestOption = hexZip(theRestValue, theRestBubble)
        for (h <- headOption; tr <- theRestOption) yield h + tr
    end toHexString
    val binRep = csDFBitsBinFormat(toBinString)
    val hexRepOption = toHexString match
      case Some(v) if width % 4 == 0         => Some(csDFBitsHexFormat(v))
      case Some(v) if allowBitsExplicitWidth => Some(csDFBitsHexFormat(v, width))
      case _                                 => None
    // choosing the shorter representation for readability
    hexRepOption match
      case Some(hr) if hr.length < binRep.length => hr
      case _                                     => binRep
  end csDFBitsData
  def csDFBoolOrBitData(dfType: DFBoolOrBit, data: Option[Boolean]): String
  def csDFDecimalData(dfType: DFDecimal, data: Option[BigInt]): String
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt], pattern: Boolean): String
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String
  def csDFStructData(dfType: DFStruct, data: List[Any]): String
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String
  final def csDFToken(token: DFTokenAny, pattern: Boolean = false): String = token match
    case DFBits.Token(dt, data)      => csDFBitsData(dt, data)
    case DFBoolOrBit.Token(dt, data) => csDFBoolOrBitData(dt, data)
    case DFDecimal.Token(dt, data)   => csDFDecimalData(dt, data)
    case DFEnum.Token(dt, data)      => csDFEnumData(dt, data, pattern)
    case DFVector.Token(dt, data)    => csDFVectorData(dt, data)
    case DFOpaque.Token(dt, data)    => csDFOpaqueData(dt, data)
    case DFStruct.Token(dt, data)    => csDFStructData(dt, data)
    case x =>
      throw new IllegalArgumentException(
        s"Unexpected token found: $x"
      )
  final def csDFTokenSeq(tokenSeq: Seq[DFTokenAny]): String =
    tokenSeq.map(csDFToken(_)).mkStringBrackets
end AbstractTokenPrinter

protected trait DFTokenPrinter extends AbstractTokenPrinter:
  val allowBitsBinModeInHex: Boolean = true
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = '?'
  def csDFBitsBinFormat(binRep: String): String = s"""b"$binRep""""
  def csDFBitsHexFormat(hexRep: String): String = s"""h"$hexRep""""
  def csDFBitsHexFormat(hexRep: String, width: Int): String = s"""h"$width'$hexRep""""
  def csDFBoolOrBitData(dfType: DFBoolOrBit, data: Option[Boolean]): String =
    data match
      case Some(value) =>
        dfType match
          case DFBool => value.toString
          case DFBit  => if (value) "1" else "0"
      case None => "?"
  def csDFDecimalData(dfType: DFDecimal, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        if (dfType.fractionWidth == 0) // DFXInt
          val interpStr = if (dfType.signed) "sd" else "d"
          s"""$interpStr"${dfType.width}'$value""""
        else ??? // DFXFix
      case None => "?"
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt], pattern: Boolean): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        s"${dfType.getName}.${entryName}${if (pattern) "()" else ""}"
      case None => "?"
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    s"Vector${data.map(x => csDFToken(DFToken.forced(dfType.cellType, x))).mkStringBrackets}"
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    s"${csDFToken(DFToken.forced(dfType.actualType, data)).applyBrackets()}.as(${dfType.getName})"
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    if (dfType.getName.isEmpty)
      csDFTupleData(dfType.fieldMap.values.toList, data)
    else
      dfType.getName + dfType.fieldMap
        .lazyZip(data)
        .map { case ((n, t), d) =>
          s"$n = ${csDFToken(DFToken.forced(t, d))}"
        }
        .mkStringBrackets
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String =
    (dfTypes lazyZip data)
      .map((t, d) => csDFToken(DFToken.forced(t, d)))
      .mkStringBrackets
end DFTokenPrinter
