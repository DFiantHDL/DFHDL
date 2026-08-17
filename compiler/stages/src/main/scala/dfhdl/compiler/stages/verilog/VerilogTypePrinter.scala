package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

protected trait VerilogTypePrinter extends AbstractTypePrinter:
  type TPrinter <: VerilogPrinter
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = "logic"
  def csDFBits(dfType: DFBitsWL, typeCS: Boolean): String =
    s"logic [${dfType.widthParamRef.hboundCS(dfType.lowIdxRef)}:${dfType.lowIdxRef.refCodeString}]"
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
  // `pkg::` qualification of a packaged type's name, dropped inside its own package file
  protected def pkgQualifier(dfType: NamedDFType): String =
    printer.typePlacementOf(dfType) match
      case Some(pkg) if !printer.currentPackage.contains(pkg) => s"$pkg::"
      case _                                                  => ""
  def csDFEnumTypeName(dfType: DFEnum): String =
    if (allowTypeDef) s"${pkgQualifier(dfType)}${dfType.name}"
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
  // Whether this vector prints as a PACKED array. Requires a SystemVerilog dialect, and an
  // integral scalar cell type: packed dimensions apply only to single-bit types, enums, packed
  // structs/unions, and other packed arrays (IEEE 1800-2017 7.4.1). Integer atom types (`int`),
  // `real`, `string`, and time values cannot form packed arrays, so vectors over them keep the
  // unpacked representation regardless of usage. SIGNED cells (`SInt`, signed fixed-point) are
  // also kept unpacked: an element select of an (anonymous-typed) packed array is a part-select,
  // which is always unsigned, so the cell signedness would be lost (a future dedicated stage may
  // lift this restriction, e.g. via named signed element types per IEEE 1800-2017 7.4.3). This
  // is a TYPE property, so every value of the same vector type agrees on it and
  // mixed-representation connections can never print.
  def supportsPackedVector(dfType: DFVector): Boolean =
    printer.supportPackedArrays && {
      def packable(cellType: DFType): Boolean = cellType match
        case _: DFBoolOrBit | _: DFBitsWL | _: DFEnum => true
        case dec: DFDecimal                           => !dec.isDFInt32 && !dec.signed
        case _: DFStruct                              => true
        case vec: DFVector                            => packable(vec.cellType)
        case op: DFOpaque                             => packable(op.actualType)
        case _                                        => false
      packable(dfType.cellType)
    }
  // the innermost non-vector cell type, whose width is the packed<->DFHDL bit-order reversal
  // grouping of the streaming casts
  def vectorScalarCellType(dfType: DFVector): DFType =
    dfType.cellType match
      case vec: DFVector => vectorScalarCellType(vec)
      case cellType      => cellType
  // The after-the-name array ranges of the UNPACKED representation (ascending). Under the
  // SystemVerilog dialects a packed-capable vector carries its dimensions in the type itself
  // (see `csDFVector`), so this yields nothing for it; the pre-SystemVerilog dialects (and
  // non-integral cell types) keep all dimensions here.
  def csDFVectorRanges(dfType: DFType): String =
    dfType match
      case vec: DFVector if !supportsPackedVector(vec) =>
        s" [0:${vec.cellDimParamRefs.head.uboundCS}]${csDFVectorRanges(vec.cellType)}"
      case _ => ""
  // the descending packed dimensions of this vector, outermost first (`[N-1:0][M-1:0]...`)
  private def csDFVectorPackedDims(dfType: DFType): String =
    dfType match
      case vec: DFVector =>
        s"[${vec.cellDimParamRefs.head.uboundCS}:0]${csDFVectorPackedDims(vec.cellType)}"
      case _ => ""
  // The complete packed-array type: the scalar cell's base keyword/name, then the vector
  // dimensions (descending, outermost first), then the cell's own packed dimensions. Only
  // unsigned decimal cells reach the DFDecimal branch: signed cells never pack (see
  // `supportsPackedVector`).
  private def csDFVectorPacked(dfType: DFVector): String =
    val dims = csDFVectorPackedDims(dfType)
    vectorScalarCellType(dfType) match
      case _: DFBoolOrBit => s"logic $dims"
      case cell: DFBitsWL =>
        s"logic $dims[${cell.widthParamRef.hboundCS(cell.lowIdxRef)}:${cell.lowIdxRef.refCodeString}]"
      case cell: DFDecimal =>
        import cell.*
        if (fractionWidth != 0)
          s"logic $dims`ufix(${magnitudeWidthParamRef.refCodeString}, $fractionWidth)"
        else s"logic $dims[${magnitudeWidthParamRef.uboundCS}:0]"
      case cell: DFEnum   => s"${csDFEnumTypeName(cell)} $dims"
      case cell: DFStruct => s"${csDFStructTypeName(cell)} $dims"
      case cell: DFOpaque => s"${csDFOpaqueTypeName(cell)} $dims"
      case _              => printer.unsupported
  end csDFVectorPacked
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    if (supportsPackedVector(dfType)) csDFVectorPacked(dfType)
    else s"${csDFType(cellType, typeCS)}"
  def csDFOpaqueTypeName(dfType: DFOpaque): String =
    s"${pkgQualifier(dfType)}${dfType.name}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"typedef ${csDFType(dfType.actualType, typeCS = true)} ${csDFOpaqueTypeName(dfType)}${csDFVectorRanges(dfType.actualType)};"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = csDFOpaqueTypeName(dfType)
  def csDFStructTypeName(dfType: DFStruct): String =
    s"${pkgQualifier(dfType)}${dfType.name}"
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
