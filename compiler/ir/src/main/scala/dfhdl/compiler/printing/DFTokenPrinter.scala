package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*

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
    ): Option[String] =
      var err = false
      val ret = Some(
        v.toHex
          .zip(b.toHex)
          .flatMap {
            case (_, 'F' | 'f')                  => s"$csDFBitBubbleChar"
            case (h, '0')                        => s"$h"
            case (h, b) if allowBitsBinModeInHex => s"{${binZip(BitVector(h), BitVector(b))}}"
            case _ =>
              err = true
              ""
          }
          .mkString
      )
      if (err) None else ret
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
  def csDFBitFormat(bitRep: String): String
  def csDFBoolFormat(value: Boolean): String
  final def csDFBoolOrBitData(dfType: DFBoolOrBit, data: Option[Boolean]): String =
    data match
      case Some(value) =>
        dfType match
          case DFBool => csDFBoolFormat(value)
          case DFBit  => csDFBitFormat(if (value) "1" else "0")
      case None => csDFBitFormat(s"${csDFBitBubbleChar}")
  val allowDecimalBigInt: Boolean
  def csDFUIntFormatBig(value: BigInt, width: Int): String
  def csDFSIntFormatBig(value: BigInt, width: Int): String
  def csDFUIntFormatSmall(value: BigInt, width: Int): String
  def csDFSIntFormatSmall(value: BigInt, width: Int): String
  def csDFUIntTokenFromBits(csBits: String): String
  def csDFSIntTokenFromBits(csBits: String): String
  def csDFUIntBubble(width: Int): String
  def csDFSIntBubble(width: Int): String
  final protected def bubbleBits(width: Int): String =
    csDFBitsData(DFBits(width), (BitVector.low(width), BitVector.high(width)))

  final def csDFDecimalData(dfType: DFDecimal, data: Option[BigInt]): String =
    import dfType.width
    data match
      case Some(value) =>
        def csBits = csDFBitsData(DFBits(width), (value.toBitVector(width), BitVector.low(width)))
        if (dfType.fractionWidth == 0) // DFXInt
          // if the language supports big integers (with explicit widths) we can simply display the values
          if (allowDecimalBigInt)
            if (dfType.signed) csDFSIntFormatBig(value, width)
            else csDFUIntFormatBig(value, width)
          // otherwise, we need to reply on small value representation or cast a bits representation
          // for big integers
          else if (dfType.signed)
            if (value.bitsWidth(true) < 31) csDFSIntFormatSmall(value, width)
            else csDFSIntTokenFromBits(csBits)
          else if (value.bitsWidth(false) < 31) csDFUIntFormatSmall(value, width)
          else csDFUIntTokenFromBits(csBits)
        else ??? // DFXFix
      case None =>
        if (dfType.signed) csDFSIntBubble(width = width)
        else csDFUIntBubble(width = width)
    end match
  end csDFDecimalData
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String
  def csDFStructData(dfType: DFStruct, data: List[Any]): String
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String
  def csDFUnitData(dfType: DFUnit, data: Unit): String
  final def csDFToken(token: DFTokenAny): String = token match
    case DFBits.Token(dt, data)      => csDFBitsData(dt, data)
    case DFBoolOrBit.Token(dt, data) => csDFBoolOrBitData(dt, data)
    case DFDecimal.Token(dt, data)   => csDFDecimalData(dt, data)
    case DFEnum.Token(dt, data)      => csDFEnumData(dt, data)
    case DFVector.Token(dt, data)    => csDFVectorData(dt, data)
    case DFOpaque.Token(dt, data)    => csDFOpaqueData(dt, data)
    case DFStruct.Token(dt, data)    => csDFStructData(dt, data)
    case DFUnit.Token(dt, data)      => csDFUnitData(dt, data)
    case x =>
      throw new IllegalArgumentException(
        s"Unexpected token found: $x"
      )
  final def csDFTokenSeq(tokenSeq: Seq[DFTokenAny]): String =
    tokenSeq.map(csDFToken).mkStringBrackets
end AbstractTokenPrinter

protected trait DFTokenPrinter extends AbstractTokenPrinter:
  val allowBitsBinModeInHex: Boolean = true
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = '?'
  def csDFBitsBinFormat(binRep: String): String = s"""b"$binRep""""
  def csDFBitsHexFormat(hexRep: String): String = s"""h"$hexRep""""
  def csDFBitsHexFormat(hexRep: String, width: Int): String = s"""h"$width'$hexRep""""
  def csDFBoolFormat(value: Boolean): String = value.toString()
  def csDFBitFormat(bitRep: String): String = bitRep
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: Int): String = s"""d"$width'$value""""
  def csDFSIntFormatBig(value: BigInt, width: Int): String = s"""sd"$width'$value""""
  def csDFUIntFormatSmall(value: BigInt, width: Int): String = value.toString
  def csDFSIntFormatSmall(value: BigInt, width: Int): String = value.toString
  def csDFUIntTokenFromBits(csBits: String): String = s"$csBits.uint"
  def csDFSIntTokenFromBits(csBits: String): String = s"$csBits.sint"
  def csDFUIntBubble(width: Int): String = "?"
  def csDFSIntBubble(width: Int): String = "?"
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        s"${dfType.getName}.${entryName}"
      case None => "?"
  val maxVectorDisplay: Int = 64
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    given CanEqual[Any, Any] = CanEqual.derived
    if (data.allElementsAreEqual)
      s"all(${csDFToken(DFToken.forced(dfType.cellType, data.head))})"
    else if (data.length > maxVectorDisplay)
      "<vector data length over max `maxVectorDisplay` in printer>"
    else
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
  def csDFUnitData(dfType: DFUnit, data: Unit): String = "()"
end DFTokenPrinter
