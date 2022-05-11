package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*

trait AbstractTypePrinter extends AbstractPrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String
  def csDFBits(dfType: DFBits, typeCS: Boolean): String
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String
  final def csNamedDFTypeDcl(dfType: NamedDFType): String =
    dfType match
      case dt: DFEnum   => csDFEnumDcl(dt)
      case dt: DFOpaque => csDFOpaqueDcl(dt)
      case dt: DFStruct => csDFStructDcl(dt)
  final def csGlobalTypeDcls: String =
    getSet.designDB.getGlobalNamedDFTypes.toList
      .sortBy(_.getName) // we sort the declarations by name, to have compilation consistency
      .map(printer.csNamedDFTypeDcl)
      .mkString("\n")
  final def csLocalTypeDcls(design: DFDesignBlock): String =
    getSet.designDB
      .getLocalNamedDFTypes(design)
      .toList
      .sortBy(_.getName) // we sort the declarations by name, to have compilation consistency
      .map(printer.csNamedDFTypeDcl)
      .mkString("\n")
  def csDFEnumDcl(dfType: DFEnum): String
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String
  def csDFVector(dfType: DFVector, typeCS: Boolean): String
  def csDFOpaqueDcl(dfType: DFOpaque): String
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String
  def csDFStructDcl(dfType: DFStruct): String
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String
  final def csDFType(dfType: DFType, typeCS: Boolean = false): String = dfType match
    case dt: DFBoolOrBit => csDFBoolOrBit(dt, typeCS)
    case dt: DFBits      => csDFBits(dt, typeCS)
    case dt: DFDecimal   => csDFDecimal(dt, typeCS)
    case dt: DFEnum      => csDFEnum(dt, typeCS)
    case dt: DFVector    => csDFVector(dt, typeCS)
    case dt: DFOpaque    => csDFOpaque(dt, typeCS)
    case dt: DFStruct    => csDFStruct(dt, typeCS)
    case NoType          => NoType.noTypeErr
end AbstractTypePrinter

protected trait DFTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "Boolean"
    case DFBit  => "Bit"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    if (typeCS) s"Bits[${dfType.width}]"
    else s"Bits(${dfType.width})"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"UInt$ob$width$cb"
      case (true, 0)  => s"SInt$ob$width$cb"
      case (false, _) => s"UFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"SFix$ob$magnitudeWidth, $fractionWidth$cb"

  def csDFEnumDcl(dfType: DFEnum): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(dfType.width), Some(v))})"
        )
        .mkString("\n")
        .indent
    s"enum ${enumName}(val value: ${csDFDecimal(DFUInt(dfType.width), true)} <> TOKEN) extends Enum.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val dimStr = if (cellDims.size == 1) cellDims.head.toString else cellDims.mkStringBrackets
    s"${csDFType(cellType, typeCS)} X $dimStr"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"object ${dfType.getName} extends Opaque(${csDFType(dfType.actualType)})"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.getName
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}: ${csDFType(t, typeCS = true)} <> VAL")
      .mkString("\n")
      .indent(2)
    s"final case class ${dfType.getName}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    if (dfType.getName.isEmpty)
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    else dfType.getName
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
end DFTypePrinter
