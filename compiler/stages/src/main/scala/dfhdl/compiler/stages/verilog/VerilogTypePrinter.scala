package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

protected trait VerilogTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = ""
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"[${dfType.width - 1}:0]"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"[${dfType.width - 1}:0]"
      case (true, 0)  => s"signed [${dfType.width - 1}:0]"
      case (false, _) => ???
      case (true, _)  => ???

  def csDFEnumDcl(dfType: DFEnum): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) => s"${enumName}_$n=$v")
        .mkString(", ")
    s"typedef enum {$entries} ${enumName};"
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
      .indent(2)
    s"final case class ${dfType.getName}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    if (dfType.getName.isEmpty)
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    else dfType.getName
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
end VerilogTypePrinter
