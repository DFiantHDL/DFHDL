package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VerilogDataPrinter extends AbstractDataPrinter:
  type TPrinter <: VerilogPrinter
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsBubbleInHex: Boolean = true
  val allowBitsExplicitWidth: Boolean = true
  val allowDecimalBigInt: Boolean = true
  val allowWidthCastSyntax: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csDFBitBubbleChar: Char = '?'
  def csDFBitsBinFormat(binRep: String): String = s"""${binRep.length}'b$binRep"""
  def csDFBitsHexFormat(hexRep: String): String = s"""${hexRep.length * 4}'h$hexRep"""
  def csDFBitsHexFormat(hexRep: String, actualWidth: Int, width: IntParamRef): String =
    val csWidth = width.refCodeString.applyBrackets()
    if (width.isRef)
      s"`TO_VEC_HEX($hexRep, $actualWidth, $csWidth)"
    else s"""${csWidth}'h$hexRep"""
  def csDFBoolFormat(value: Boolean): String = if (value) "1" else "0"
  def csDFBitFormat(bitRep: String): String = csDFBitsBinFormat(bitRep)
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String =
    val csWidth = width.refCodeString.applyBrackets()
    if (width.isRef)
      if (value.isValidInt && allowWidthCastSyntax) s"""${csWidth}'($value)"""
      else
        val actualWidth = value.bitsWidth(false)
        if (allowWidthCastSyntax)
          s"""${csWidth}'(${actualWidth}'d$value)"""
        else
          s"`TO_UNSIGNED($value, $actualWidth, $csWidth)"
    else s"""${csWidth}'d$value"""
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String =
    val csWidth = width.refCodeString.applyBrackets()
    if (width.isRef)
      if (value.isValidInt && allowWidthCastSyntax) s"""${csWidth}'($value)"""
      else
        val actualWidth = value.bitsWidth(true)
        if (allowWidthCastSyntax)
          if (value >= 0) s"""${csWidth}'($actualWidth'sd$value)"""
          else s"""${csWidth}'(-$actualWidth'sd${-value})"""
        else if (value >= 0)
          s"`TO_UNSIGNED($value, $actualWidth, $csWidth)"
        else
          s"`TO_SIGNED_NEG(${-value}, $actualWidth, $csWidth)"
    else if (value >= 0) s"""$csWidth'sd$value"""
    else s"""-$csWidth'sd${-value}"""
  end csDFSIntFormatBig
  def csDFUIntFormatSmall(value: BigInt, width: Int): String =
    csDFUIntFormatBig(value, IntParamRef(width))
  def csDFSIntFormatSmall(value: BigInt, width: Int): String =
    csDFSIntFormatBig(value, IntParamRef(width))
  def csDFUIntDataFromBits(csBits: String): String = s"""$$unsigned($csBits)"""
  def csDFSIntDataFromBits(csBits: String): String = s"""$$signed($csBits)"""
  def csDFUIntBubble(width: Int): String = bubbleBits(width)
  def csDFSIntBubble(width: Int): String = csDFSIntDataFromBits(bubbleBits(width))
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        val verilogDefine = if (printer.allowTypeDef) "" else "`"
        s"$verilogDefine${dfType.getName}_${entryName}"
      case None => "?"
  val maxElementsPerLine = 64
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    data.view.map(csConstData(dfType.cellType, _)).toList.csList("'{", ",", "}")
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    csConstData(dfType.actualType, data)
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    dfType.fieldMap
      .lazyZip(data)
      .map { case ((n, t), d) =>
        s"$n: ${csConstData(t, d)}"
      }
      .mkString("'{", ", ", "}")
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String = printer.unsupported
  def csDFUnitData(dfType: DFUnit, data: Unit): String = printer.unsupported
  def csDFDoubleData(dfType: DFDouble, data: Option[Double]): String =
    data match
      case Some(value) => value.toString
      case None        => "?"
end VerilogDataPrinter
