package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

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
        .hindent
    s"enum ${enumName}(val value: UInt[${dfType.width}] <> TOKEN) extends Encode.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    s"${csDFType(cellType, typeCS)}.X${cellDims.mkStringBrackets}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"case class ${dfType.getName}() extends Opaque(${csDFType(dfType.actualType)})"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.getName
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}: ${csDFType(t, typeCS = true)} <> VAL")
      .mkString("\n")
      .hindent(2)
    s"final case class ${dfType.getName}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    if (dfType.getName.isEmpty)
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    else dfType.getName
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
end VHDLTypePrinter
