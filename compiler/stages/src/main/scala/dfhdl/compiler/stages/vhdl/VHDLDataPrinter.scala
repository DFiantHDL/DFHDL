package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VHDLDataPrinter extends AbstractDataPrinter:
  type TPrinter <: VHDLPrinter
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsExplicitWidth: Boolean = true
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
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: IntParamRef): String =
    if (width.isRef)
      s"""resize(d"$value", ${width.refCodeString})"""
    else
      s"""${width.refCodeString.applyBrackets()}d"$value""""
  def csDFSIntFormatBig(value: BigInt, width: IntParamRef): String =
    if (width.isRef) s"""resize(d"$value", ${width.refCodeString})"""
    else
      val csWidth = width.refCodeString.applyBrackets()
      if (value >= 0) s"""${csWidth}d"$value""""
      else s"""-${csWidth}d"${-value}""""
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
        s"${dfType.getName}_${entryName}"
      case None => "?"
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    given CanEqual[Any, Any] = CanEqual.derived
    val csData =
      data.view.zipWithIndex.map((x, i) =>
        s"${i.toPaddedString(data.length - 1, padWithZeros = false)} => ${csConstData(dfType.cellType, x)}"
      )
    csData.toList.csList()
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
end VHDLDataPrinter
