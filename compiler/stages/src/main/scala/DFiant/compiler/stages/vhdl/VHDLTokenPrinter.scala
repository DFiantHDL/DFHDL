package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*
import DFVal.*

protected trait VHDLTokenPrinter extends AbstractTokenPrinter:
  val allowBitsBinModeInHex: Boolean = false
  val allowBitsExplicitWidth: Boolean = true
  def csDFBitBubbleChar: Char = '-'
  def csDFBitsBinFormat(binRep: String): String = s""""$binRep""""
  def csDFBitsHexFormat(hexRep: String): String = s"""x"$hexRep""""
  def csDFBitsHexFormat(hexRep: String, width: Int): String = s"""${width}x"$hexRep""""
  def csDFBitFormat(bitRep: String): String = s"'$bitRep'"
  val allowDecimalBigInt: Boolean = true
  def csDFUIntFormatBig(value: BigInt, width: Int): String = s"""${width}d"$value""""
  def csDFSIntFormatBig(value: BigInt, width: Int): String =
    if (value >= 0) s"""${width}d"$value""""
    else s"""-${width}d"${-value}""""
  def csDFUIntFormatSmall(value: BigInt, width: Int): String = s"to_unsigned($value, $width)"
  def csDFSIntFormatSmall(value: BigInt, width: Int): String = s"to_signed($value, $width)"
  def csDFUIntTokenFromBits(csBits: String): String = s"""unsigned'($csBits)"""
  def csDFSIntTokenFromBits(csBits: String): String = s"""signed'($csBits)"""
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
end VHDLTokenPrinter
