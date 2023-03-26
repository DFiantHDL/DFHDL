package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

protected trait VerilogTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = "logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"logic [${dfType.width - 1}:0]"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"logic [${dfType.width - 1}:0]"
      case (true, 0)  => s"logic signed [${dfType.width - 1}:0]"
      case (false, _) => ???
      case (true, _)  => ???

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
    s"typedef enum$explicitWidth {\n${entries.hindent}\n} ${enumName};"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVectorRanges(dfType: DFType): String =
    dfType match
      case vec: DFVector => s" [0:${vec.cellDims.head - 1}]${csDFVectorRanges(vec.cellType)}"
      case _             => ""
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    s"${csDFType(cellType, typeCS)}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"case class ${dfType.getName}() extends Opaque(${csDFType(dfType.actualType)})"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.getName
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${csDFType(t, typeCS = true)} $n${csDFVectorRanges(t)};")
      .mkString("\n")
      .hindent
    s"typedef struct packed {\n$fields\n} ${dfType.getName};"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    if (dfType.getName.isEmpty)
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    else dfType.getName
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
end VerilogTypePrinter
