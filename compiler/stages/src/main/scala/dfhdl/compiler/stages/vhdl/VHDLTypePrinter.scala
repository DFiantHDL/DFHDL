package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

protected trait VHDLTypePrinter extends AbstractTypePrinter:
  type TPrinter <: VHDLPrinter
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "boolean"
    case DFBit  => "std_logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"std_logic_vector(${dfType.widthParamRef.uboundCS} downto 0)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"unsigned(${widthParamRef.uboundCS} downto 0)"
      case (true, 0)  => s"signed(${widthParamRef.uboundCS} downto 0)"
      case (false, _) => ???
      case (true, _)  => ???

  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(dfType.width), Some(v))})"
        )
        .mkString("\n")
        .hindent
    s"enum ${enumName}(val value: UInt[${dfType.width}] <> CONST) extends Encode.Manual(${dfType.width}):\n$entries"
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
    dfType.getName
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VHDLTypePrinter
