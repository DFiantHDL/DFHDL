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
  def csDFBitsHexFormat(hexRep: String, width: IntParamRef): String =
    s"""${width.refCodeString.applyBrackets()}x"$hexRep""""
  def csDFBoolFormat(value: Boolean): String = value.toString()
  def csDFBitFormat(bitRep: String): String = s"'$bitRep'"
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: Int): String = s"""${width}d"$value""""
  def csDFSIntFormatBig(value: BigInt, width: Int): String =
    if (value >= 0) s"""${width}d"$value""""
    else s"""-${width}d"${-value}""""
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
        s"${dfType.getName}.${entryName}"
      case None => "?"
  def csDFVectorData(dfType: DFVector, data: Vector[Any]): String =
    s"Vector${data.map(x => csConstData(dfType.cellType, x)).mkStringBrackets}"
  def csDFOpaqueData(dfType: DFOpaque, data: Any): String =
    s"${csConstData(dfType.actualType, data).applyBrackets()}.as(${dfType.getName})"
  def csDFStructData(dfType: DFStruct, data: List[Any]): String =
    dfType.getName + dfType.fieldMap
      .lazyZip(data)
      .map { case ((n, t), d) =>
        s"$n = ${csConstData(t, d)}"
      }
      .mkStringBrackets
  def csDFTupleData(dfTypes: List[DFType], data: List[Any]): String = printer.unsupported
  def csDFUnitData(dfType: DFUnit, data: Unit): String = printer.unsupported
end VHDLDataPrinter
