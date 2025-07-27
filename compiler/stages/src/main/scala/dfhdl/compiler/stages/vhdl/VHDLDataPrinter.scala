package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VHDLDataPrinter extends AbstractDataPrinter:
  type TPrinter <: VHDLPrinter
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsBubbleInHex: Boolean =
    printer.dialect match
      case VHDLDialect.v93 => false
      case _               => true
  val allowBitsExplicitWidth: Boolean =
    printer.dialect match
      case VHDLDialect.v93 => false
      case _               => true
  val allowDecimalBigInt: Boolean = true
  val allowDecimalSyntax: Boolean =
    printer.dialect match
      case VHDLDialect.v93 => false
      case _               => true
  def csDFBitBubbleChar: Char = '-'
  def csDFBitsBinFormat(binRep: String): String = s""""$binRep""""
  def csDFBitsHexFormat(hexRep: String): String = s"""x"$hexRep""""
  def csDFBitsHexFormat(hexRep: String, actualWidth: Int, width: IntParamRef): String =
    if (width.isRef)
      s"""resize(x"$hexRep", ${width.refCodeString})"""
    else
      s"""${width.refCodeString.applyBrackets()}x"$hexRep""""
  def csDFBoolFormat(value: Boolean): String = value.toString()
  def csDFBitFormat(bitRep: String): String = s"'$bitRep'"
  def csDecimalFormat(value: BigInt): String =
    if (allowDecimalSyntax) s"""d"$value""""
    else s"""x"${value.toString(16)}""""
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String =
    if (allowDecimalSyntax)
      if (width.isRef)
        s"""resize(d"$value", ${width.refCodeString})"""
      else
        s"""${width.refCodeString.applyBrackets()}d"$value""""
    else
      val intRepIsValid = value.bitsWidth(false) < 31
      if (intRepIsValid) s"""to_unsigned($value, ${width.refCodeString})"""
      else s"""unsigned'(resize(x"${value.toString(16)}", ${width.refCodeString}))"""
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String =
    if (allowDecimalSyntax)
      if (width.isRef) s"""resize(d"$value", ${width.refCodeString})"""
      else
        val csWidth = width.refCodeString.applyBrackets()
        if (value >= 0) s"""${csWidth}d"$value""""
        else s"""-${csWidth}d"${-value}""""
    else
      val intRepIsValid = value.bitsWidth(true) < 31
      if (intRepIsValid) s"""to_signed($value, ${width.refCodeString})"""
      else if (value >= 0)
        s"""signed'(resize(x"${value.toString(16)}", ${width.refCodeString}))"""
      else
        s"""-signed'(resize(x"${(-value).toString(16)}", ${width.refCodeString}))"""

  def csDFUIntFormatSmall(value: BigInt, width: Int): String = s"to_unsigned($value, $width)"
  def csDFSIntFormatSmall(value: BigInt, width: Int): String = s"to_signed($value, $width)"
  def csDFUIntDataFromBits(csBits: String): String = s"""unsigned'($csBits)"""
  def csDFSIntDataFromBits(csBits: String): String = s"""signed'($csBits)"""
  def csDFUIntBubble(width: Int): String = csDFUIntDataFromBits(bubbleBits(width))
  def csDFSIntBubble(width: Int): String = csDFSIntDataFromBits(bubbleBits(width))
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        s"${dfType.name}_${entryName}"
      case None => "?"
  def csDFVectorElemCS(elemCS: List[String]): String =
    elemCS.view.zipWithIndex.map((x, i) =>
      s"${i.toPaddedString(elemCS.length - 1, padWithZeros = false)} => $x"
    ).toList.csList()
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    csDFVectorElemCS(data.view.map(csConstData(dfType.cellType, _)).toList)
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    csConstData(dfType.actualType, data)
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    printer.csDFStructTypeName(dfType) + dfType.fieldMap
      .lazyZip(data)
      .map { case ((n, t), d) =>
        s"$n = ${csConstData(t, d)}"
      }
      .mkStringBrackets
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String = printer.unsupported
  def csDFUnitData(dfType: DFUnit, data: Unit): String = printer.unsupported
  def csDFDoubleData(dfType: DFDouble, data: Option[Double]): String =
    data match
      case Some(value) => value.toString
      case None        => "?"
  def csDFTimeData(data: TimeNumber): String =
    s"${csBigDecimalData(data.value)} ${data.unit}"
  def csDFFreqData(data: FreqNumber): String = printer.unsupported
  def csDFNumberData(data: LiteralNumber): String = printer.unsupported
  def scalaToVHDLString(str: String): String =
    str.view.map {
      case '"'  => "\"\""
      case '\t' => "\" & HT & \""
      case '\n' => "\" & LF & \""
      case '\r' => "\" & CR & \""
      case c    => c.toString
    }.mkString("\"", "", "\"")
  def csDFStringData(dfType: DFString, data: Option[String]): String =
    data match
      case Some(value) => scalaToVHDLString(value)
      case None        => "\"\""
end VHDLDataPrinter
