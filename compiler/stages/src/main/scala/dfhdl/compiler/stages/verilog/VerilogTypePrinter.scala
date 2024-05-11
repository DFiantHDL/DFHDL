package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

protected trait VerilogTypePrinter extends AbstractTypePrinter:
  type TPrinter <: VerilogPrinter
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = "logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"logic [${dfType.widthParamRef.uboundCS}:0]"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"logic [${dfType.widthParamRef.uboundCS}:0]"
      case (true, 0) =>
        if (dfType.isDFInt32) "int"
        else s"logic signed [${dfType.widthParamRef.uboundCS}:0]"
      case (false, _) => ???
      case (true, _)  => ???

  def csDFEnumTypeName(dfType: DFEnum): String = s"t_enum_${dfType.getName}"
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) => s"${enumName}_$n = $v")
        .mkString(",\n")
    // TODO: quartus seems to not accept an explicit size, so we drop it locally where it's not required.
    // Globally, size is required (at least for verilator linter), so we need to drop enumeration altogether
    // in such a case (change to a vector and list of constants) and then remove the special case handling
    // here.
    val explicitWidth = if (global) s" [${dfType.width - 1}:0]" else ""
    s"typedef enum$explicitWidth {\n${entries.hindent}\n} ${csDFEnumTypeName(dfType)};"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = csDFEnumTypeName(dfType)
  def csDFVectorRanges(dfType: DFType): String =
    dfType match
      case vec: DFVector => s" [0:${vec.cellDims.head - 1}]${csDFVectorRanges(vec.cellType)}"
      case _             => ""
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    s"${csDFType(cellType, typeCS)}"
  def csDFOpaqueTypeName(dfType: DFOpaque): String = s"t_opaque_${dfType.getName}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"typedef ${csDFType(dfType.actualType, typeCS = true)} ${csDFOpaqueTypeName(dfType)}${csDFVectorRanges(dfType.actualType)};"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = csDFOpaqueTypeName(dfType)
  def csDFStructTypeName(dfType: DFStruct): String = s"t_struct_${dfType.getName}"
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${csDFType(t, typeCS = true)} $n${csDFVectorRanges(t)};")
      .mkString("\n")
      .hindent
    s"typedef struct packed {\n$fields\n} ${csDFStructTypeName(dfType)};"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String = csDFStructTypeName(dfType)
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VerilogTypePrinter
