package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*

protected trait VHDLTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "boolean"
    case DFBit  => "std_logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"std_logic_vector(${dfType.width - 1} downto 0)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"unsigned(${width - 1} downto 0)"
      case (true, 0)  => s"signed(${width - 1} downto 0)"
      case (false, _) => ???
      case (true, _)  => ???

  def csDFEnumDcl(dfType: DFEnum): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(dfType.width), Some(v))})"
        )
        .mkString("\n")
        .indent
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
end VHDLTypePrinter
