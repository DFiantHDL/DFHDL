package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*

trait AbstractDataPrinter extends AbstractPrinter:
  val allowBitsBinModeInHex: Boolean
  val allowBitsBubbleInHex: Boolean
  val allowBitsExplicitWidth: Boolean
  def csDFBitBubbleChar: Char
  def csDFBitsBinFormat(binRep: String): String
  def csDFBitsHexFormat(hexRep: String): String
  def csDFBitsHexFormat(hexRep: String, actualWidth: Int, width: IntParamRef): String
  final def csDFBitsData(dfType: DFBits, data: (BitVector, BitVector)): String =
    val valueBits: BitVector = data._1
    val bubbleBits: BitVector = data._2
    import dfType.{width, widthParamRef}

    def binZip(v: BitVector, b: BitVector): String =
      v.toBin.zip(b.toBin)
        .map {
          case (_, '1')       => csDFBitBubbleChar
          case (zeroOrOne, _) => zeroOrOne
        }
        .mkString
    end binZip
    def hexZip(v: BitVector, b: BitVector): Option[String] =
      var err = false
      val ret = Some(
        v.toHex.zip(b.toHex)
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
      // if bubbles in hex are not supported and the data has bubbles
      // then there is no valid hex representation
      if (!allowBitsBubbleInHex && !bubbleBits.isZeros) None
      else if (width % 4 == 0) hexZip(valueBits, bubbleBits)
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
    widthParamRef match
      case _: DFRefAny =>
        // use minimal hex value representation
        val actualWidth = valueBits.lengthOfValue.toInt
        val rem = actualWidth % 4
        val actualWidthDiv4 = if (rem == 0) actualWidth else actualWidth + (4 - rem)
        val hexStr =
          hexZip(valueBits.resize(actualWidthDiv4), bubbleBits.resize(actualWidthDiv4)).get
        csDFBitsHexFormat(hexStr, actualWidth, widthParamRef)
      case _ =>
        val binRep = csDFBitsBinFormat(toBinString)
        val hexRepOption = toHexString match
          case Some(v) if width % 4 == 0 => Some(csDFBitsHexFormat(v))
          case Some(v) if allowBitsExplicitWidth =>
            Some(csDFBitsHexFormat(v, binRep.length, widthParamRef))
          case _ => None
        // choosing the shorter representation for readability
        hexRepOption match
          case Some(hr) if hr.length < binRep.length => hr
          case _                                     => binRep
    end match
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
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String
  def csDFUIntFormatSmall(value: BigInt, width: Int): String
  def csDFSIntFormatSmall(value: BigInt, width: Int): String
  def csDFUIntDataFromBits(csBits: String): String
  def csDFSIntDataFromBits(csBits: String): String
  def csDFUIntBubble(width: Int): String
  def csDFSIntBubble(width: Int): String
  final protected def bubbleBits(width: Int): String =
    csDFBitsData(DFBits(width), (BitVector.low(width), BitVector.high(width)))

  final def csDFDecimalData(dfType: DFDecimal, data: Option[BigInt]): String =
    import dfType.{width, widthParamRef}
    data match
      case Some(value) =>
        def csBits = csDFBitsData(DFBits(width), (value.toBitVector(width), BitVector.low(width)))
        if (dfType.fractionWidth == 0) // DFXInt
          // native integers are printed as they are (this assumes in all backends integers are printed the same)
          if (dfType.isDFInt32) value.toString()
          // if the language supports big integers (with explicit widths) we can simply display the values
          else if (allowDecimalBigInt || widthParamRef.isRef)
            if (dfType.signed) csDFSIntFormatBig(value, widthParamRef)
            else csDFUIntFormatBig(value, widthParamRef)
          // otherwise, we need to reply on small value representation or cast a bits representation
          // for big integers
          else if (dfType.signed)
            if (value.bitsWidth(true) < 31) csDFSIntFormatSmall(value, width)
            else csDFSIntDataFromBits(csBits)
          else if (value.bitsWidth(false) < 31) csDFUIntFormatSmall(value, width)
          else csDFUIntDataFromBits(csBits)
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
  def csDFDoubleData(dfType: DFDouble, data: Option[Double]): String
  final def csConstData(dfType: DFType, data: Any): String = (dfType, data) match
    case DFBits.Data(dt, data)      => csDFBitsData(dt, data)
    case DFBoolOrBit.Data(dt, data) => csDFBoolOrBitData(dt, data)
    case DFDecimal.Data(dt, data)   => csDFDecimalData(dt, data)
    case DFDouble.Data(dt, data)    => csDFDoubleData(dt, data)
    case DFEnum.Data(dt, data)      => csDFEnumData(dt, data)
    case DFVector.Data(dt, data)    => csDFVectorData(dt, data)
    case DFOpaque.Data(dt, data)    => csDFOpaqueData(dt, data)
    case DFStruct.Data(dt, data) if dt.isTuple && tupleSupportEnable =>
      csDFTupleData(dt.fieldMap.values.toList, data)
    case DFStruct.Data(dt, data) => csDFStructData(dt, data)
    case DFUnit.Data(dt, data)   => csDFUnitData(dt, data)
    case x =>
      throw new IllegalArgumentException(
        s"Unexpected data found: $x"
      )
end AbstractDataPrinter

protected trait DFDataPrinter extends AbstractDataPrinter:
  val allowBitsBinModeInHex: Boolean = true
  val allowBitsBubbleInHex: Boolean = true
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = '?'
  def csDFBitsBinFormat(binRep: String): String = s"""b"$binRep""""
  def csDFBitsHexFormat(hexRep: String): String = s"""h"$hexRep""""
  def csWidthInterp(width: IntParamRef): String = width match
    case int: Int => int.toString
    case _        => s"$${${width.refCodeString}}"
  def csDFBitsHexFormat(hexRep: String, actualWidth: Int, width: IntParamRef): String =
    s"""h"${csWidthInterp(width)}'$hexRep""""
  def csDFBoolFormat(value: Boolean): String = value.toString()
  def csDFBitFormat(bitRep: String): String = bitRep
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String =
    s"""d"${csWidthInterp(width)}'$value""""
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String =
    s"""sd"${csWidthInterp(width)}'$value""""
  def csDFUIntFormatSmall(value: BigInt, width: Int): String = value.toString
  def csDFSIntFormatSmall(value: BigInt, width: Int): String = value.toString
  def csDFUIntDataFromBits(csBits: String): String = s"$csBits.uint"
  def csDFSIntDataFromBits(csBits: String): String = s"$csBits.sint"
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
    if (data.length > maxVectorDisplay)
      "<vector data length over max `maxVectorDisplay` in printer>"
    else
      data.view.map(csConstData(dfType.cellType, _)).toList
        .csList(s"DFVector(${printer.csDFVector(dfType, typeCS = false)})(", ",", ")")
    end if
  end csDFVectorData
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    s"${csConstData(dfType.actualType, data).applyBrackets()}.as(${dfType.getName})"
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    dfType.getName + dfType.fieldMap
      .lazyZip(data)
      .map { case ((n, t), d) =>
        s"$n = ${csConstData(t, d)}"
      }
      .mkStringBrackets
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String =
    (dfTypes lazyZip data)
      .map((t, d) => csConstData(t, d))
      .mkStringBrackets
  def csDFUnitData(dfType: DFUnit, data: Unit): String = "()"
  def csDFDoubleData(dfType: DFDouble, data: Option[Double]): String =
    data match
      case Some(value) => value.toString
      case None        => "?"
end DFDataPrinter
