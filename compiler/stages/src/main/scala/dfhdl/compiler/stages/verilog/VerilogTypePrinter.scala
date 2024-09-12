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
  val intTypeIsSupported: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  val allowSignedKeywordAndOps: Boolean =
    printer.dialect match
      case VerilogDialect.v95 => false
      case _                  => true
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val signedKeyword = if (allowSignedKeywordAndOps) "signed " else ""
    (signed, fractionWidth) match
      case (false, 0) => s"logic [${dfType.widthParamRef.uboundCS}:0]"
      case (true, 0) =>
        if (dfType.isDFInt32)
          if (intTypeIsSupported) "int"
          else "integer"
        else s"logic $signedKeyword[${dfType.widthParamRef.uboundCS}:0]"
      case (false, _) => ???
      case (true, _)  => ???

  val allowTypeDef: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csDFEnumTypeName(dfType: DFEnum): String =
    if (allowTypeDef) s"t_enum_${dfType.getName}"
    else csDFBits(DFBits(dfType.width), false)
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    if (allowTypeDef)
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
    else
      dfType.entries.view
        .map((n, v) => s"parameter ${enumName}_$n = $v;")
        .mkString("\n")
    end if
  end csDFEnumDcl

  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = csDFEnumTypeName(dfType)
  def csDFVectorRanges(dfType: DFType): String =
    dfType match
      case vec: DFVector =>
        s" [0:${vec.cellDimParamRefs.head.uboundCS}]${csDFVectorRanges(vec.cellType)}"
      case _ => ""
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
