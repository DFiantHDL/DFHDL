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
    // fixed-point (fractionWidth != 0) types come from the `ufix`/`sfix` macro (the single
    // control point for the range convention), with `M` magnitude bits and `F` fraction
    // bits. `sfix` carries the `signed` keyword itself, so it is not repeated here; on
    // Verilog-95 (no `signed` keyword) signed values use `sfix_v95` and apply signedness at
    // the operation sites, like `SInt`. Integers keep the plain `[magnitude-1:0]` range.
    if (fractionWidth != 0)
      val csMag = magnitudeWidthParamRef.refCodeString
      val macroName =
        if (!signed) "ufix"
        else if (allowSignedKeywordAndOps) "sfix"
        else "sfix_v95"
      s"logic `$macroName($csMag, $fractionWidth)"
    else if (signed)
      if (dfType.isDFInt32)
        if (intTypeIsSupported) "int"
        else "integer"
      else s"logic $signedKeyword[${magnitudeWidthParamRef.uboundCS}:0]"
    else s"logic [${magnitudeWidthParamRef.uboundCS}:0]"
  end csDFDecimal
  val allowDynamicString: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csDFString(dfType: DFString, typeCS: Boolean): String =
    if (allowDynamicString) "string"
    else ""

  val allowTypeDef: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csGlobalTypeFuncDcls: String =
    getSet.designDB.getGlobalNamedDFTypes.view.collect { case dfType: DFEnum =>
      csDFEnumToStringFuncDcl(dfType)
    }.mkString("\n")
  def csDFEnumTypeName(dfType: DFEnum): String =
    if (allowTypeDef) s"t_enum_${dfType.name}"
    else csDFBits(DFBits(dfType.widthIntOpt.get), false)
  def csDFEnumToStringFuncDcl(dfType: DFEnum): String =
    val enumName = dfType.name
    val maxCharWidth = dfType.entries.view.keys.map(_.length).max + enumName.length + 1
    val funcName = s"${enumName}_to_string"
    val cases =
      dfType.entries.view
        .map((n, v) => s"`${enumName}_${n}: $funcName = \"${enumName}_${n}\";")
        .mkString("\n").hindent(2)
    // workaround for verilator bug: https://github.com/verilator/verilator/issues/6893
    s"""|function [8*${maxCharWidth}:1] $funcName;
        |  /* verilator lint_off UNUSEDSIGNAL */
        |  input [${dfType.widthIntOpt.get - 1}:0] value;
        |  case (value)
        |${cases}
        |    default: $funcName = "?";
        |  endcase
        |  /* verilator lint_on UNUSEDSIGNAL */
        |endfunction""".stripMargin
  end csDFEnumToStringFuncDcl
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.name
    if (allowTypeDef)
      val entries =
        dfType.entries.view
          .map((n, v) => s"${enumName}_$n = $v")
          .mkString(",\n")
      // TODO: quartus seems to not accept an explicit size Globally
      val explicitWidth = s" logic [${dfType.widthIntOpt.get - 1}:0]"
      s"typedef enum$explicitWidth {\n${entries.hindent}\n} ${csDFEnumTypeName(dfType)};"
    else
      dfType.entries.view
        .map((n, v) => s"`define ${enumName}_$n $v")
        .mkString("", "\n", "\n") + (if (global) "" else csDFEnumToStringFuncDcl(dfType))
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
  def csDFOpaqueTypeName(dfType: DFOpaque): String = s"t_opaque_${dfType.name}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"typedef ${csDFType(dfType.actualType, typeCS = true)} ${csDFOpaqueTypeName(dfType)}${csDFVectorRanges(dfType.actualType)};"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = csDFOpaqueTypeName(dfType)
  def csDFStructTypeName(dfType: DFStruct): String = s"t_struct_${dfType.name}"
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${csDFType(t, typeCS = true)} $n${csDFVectorRanges(t)};")
      .mkString("\n")
      .hindent
    s"typedef struct packed {\n$fields\n} ${csDFStructTypeName(dfType)};"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String = csDFStructTypeName(dfType)
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFDouble(): String = "real"
  def csDFTime(dfType: DFTime, typeCS: Boolean): String = printer.unsupported
  def csDFFreq(dfType: DFFreq, typeCS: Boolean): String = printer.unsupported
  def csDFNumber(dfType: DFNumber, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VerilogTypePrinter
