package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*

protected trait VerilogTokenPrinter extends AbstractTokenPrinter:
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = 'x'
  def csDFBitsBinFormat(binRep: String): String = s"""${binRep.length}'b$binRep"""
  def csDFBitsHexFormat(hexRep: String): String = s"""${hexRep.length * 4}'h$hexRep"""
  def csDFBitsHexFormat(hexRep: String, width: Int): String = s"""${width}'h$hexRep"""
  def csDFBitFormat(bitRep: String): String = csDFBitsBinFormat(bitRep)
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: Int): String = s"""${width}'d$value"""
  def csDFSIntFormatBig(value: BigInt, width: Int): String =
    if (value >= 0) csDFUIntFormatBig(value, width)
    else s"-${csDFUIntFormatBig(-value, width)}"
  def csDFUIntFormatSmall(value: BigInt, width: Int): String = csDFUIntFormatBig(value, width)
  def csDFSIntFormatSmall(value: BigInt, width: Int): String = csDFSIntFormatBig(value, width)
  def csDFUIntTokenFromBits(csBits: String): String = s"""$$unsigned($csBits)"""
  def csDFSIntTokenFromBits(csBits: String): String = s"""$$signed($csBits)"""
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String =
    data match
      case Some(value) =>
        val entryName = dfType.entries.find(_._2 == value).get._1
        s"${dfType.getName}.${entryName}"
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
end VerilogTokenPrinter
