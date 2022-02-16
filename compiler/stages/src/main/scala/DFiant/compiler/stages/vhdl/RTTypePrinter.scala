package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*

protected trait RTTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "DFBool"
    case DFBit  => "DFBit"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    if (typeCS) s"DFBits[${dfType.width}]"
    else s"DFBits(${dfType.width})"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"DFUInt$ob$width$cb"
      case (true, 0)  => s"DFSInt$ob$width$cb"
      case (false, _) => s"DFUFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"DFSFix$ob$magnitudeWidth, $fractionWidth$cb"

  def csDFEnumDcl(dfType: DFEnum): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(dfType.width), Some(v))})"
        )
        .mkString("\n")
        .indent()
    s"enum ${enumName}(val value: DFUInt[${dfType.width}] <> TOKEN) extends DFEnum.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    s"${csDFType(cellType, typeCS)}.X${cellDims.mkStringBrackets}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"object ${dfType.getName} extends DFOpaque(${csDFType(dfType.actualType)})"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.getName
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}: ${csDFType(t, typeCS = true)} <> VAL")
      .mkString("\n")
      .indent(2)
    s"final case class ${dfType.getName}(\n$fields\n) extends DFStruct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    if (dfType.getName.isEmpty)
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    else dfType.getName
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
end RTTypePrinter
