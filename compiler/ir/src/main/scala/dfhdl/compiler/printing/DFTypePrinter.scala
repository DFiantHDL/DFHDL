package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*

trait AbstractTypePrinter extends AbstractPrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String
  def csDFBits(dfType: DFBits, typeCS: Boolean): String
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String
  final def csNamedDFTypeDcl(dfType: NamedDFType, global: Boolean): String =
    dfType match
      case dt: DFEnum   => csDFEnumDcl(dt, global)
      case dt: DFOpaque => csDFOpaqueDcl(dt)
      case dt: DFStruct => csDFStructDcl(dt)
  final def csGlobalConstDcls: String =
    printer.csDFMembers(getSet.designDB.membersGlobals)
  final def csGlobalTypeDcls: String =
    getSet.designDB.getGlobalNamedDFTypes.view
      .filter {
        // show tuple structures only if tuple support is disabled
        case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
        case _                                                        => true
      }
      .map(x => printer.csNamedDFTypeDcl(x, global = true))
      .mkString("\n").emptyOr(x => s"$x\n")
  final def csLocalTypeDcls(design: DFDesignBlock): String =
    getSet.designDB.getLocalNamedDFTypes(design).view
      .filter {
        // show tuple structures only if tuple support is disabled
        case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
        case _                                                        => true
      }
      .map(x => printer.csNamedDFTypeDcl(x, global = false))
      .mkString("\n")
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String
  def csDFVector(dfType: DFVector, typeCS: Boolean): String
  def csDFOpaqueDcl(dfType: DFOpaque): String
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String
  def csDFStructDcl(dfType: DFStruct): String
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String
  final def csDFType(dfType: DFType, typeCS: Boolean = false): String = dfType match
    case dt: DFBoolOrBit => csDFBoolOrBit(dt, typeCS)
    case dt: DFBits      => csDFBits(dt, typeCS)
    case dt: DFDecimal   => csDFDecimal(dt, typeCS)
    case dt: DFEnum      => csDFEnum(dt, typeCS)
    case dt: DFVector    => csDFVector(dt, typeCS)
    case dt: DFOpaque    => csDFOpaque(dt, typeCS)
    case dt: DFStruct if dt.isTuple && tupleSupportEnable =>
      csDFTuple(dt.fieldMap.values.toList, typeCS)
    case dt: DFStruct  => csDFStruct(dt, typeCS)
    case dt: DFUnit    => csDFUnit(dt, typeCS)
    case dt: DFNothing => ???
end AbstractTypePrinter

protected trait DFTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "Boolean"
    case DFBit  => "Bit"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    if (typeCS) s"Bits[${dfType.widthParamRef.refCodeString}]"
    else s"Bits(${dfType.widthParamRef.refCodeString})"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"UInt$ob$width$cb"
      case (true, 0) =>
        if (dfType.isDFInt32) "Int"
        else s"SInt$ob$width$cb"
      case (false, _) => s"UFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"SFix$ob$magnitudeWidth, $fractionWidth$cb"

  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(dfType.width), Some(v))})"
        )
        .mkString("\n")
        .hindent
    s"enum ${enumName}(val value: ${csDFDecimal(DFUInt(dfType.width), true)} <> CONST) extends Encode.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val dimStr = if (cellDims.size == 1) cellDims.head.toString else cellDims.mkStringBrackets
    s"${csDFType(cellType, typeCS)} X $dimStr"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"case class ${dfType.getName}() extends Opaque(${csDFType(dfType.actualType)})"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.getName
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}${csDFValType(t)}")
      .mkString("\n")
      .hindent(2)
    s"final case class ${dfType.getName}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    dfType.getName
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = "Unit"
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
  def csDFValType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> VAL"
  def csDFValConstType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> CONST"
end DFTypePrinter
