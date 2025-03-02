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
  private def isInt32Val(member: DFMember): Boolean =
    member match
      case dfVal: DFVal =>
        dfVal.dfType match
          case DFInt32 => true
          case _       => false
      case _ => false
  final def csGlobalConstIntDcls: String =
    printer.csDFMembers(getSet.designDB.membersGlobals.filter(isInt32Val))
  final def csGlobalConstNonIntDcls: String =
    printer.csDFMembers(getSet.designDB.membersGlobals.filterNot(isInt32Val))
  final def csGlobalTypeDcls: String =
    getSet.designDB.getGlobalNamedDFTypes.view
      .filter {
        // show tuple structures only if tuple support is disabled
        case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
        // skipping unknown clock and reset definitions (they are unknown because
        // they lack additional name suffix that belongs to their configuration)
        case DFOpaque("Clk", _: DFOpaque.Clk, _) => false
        case DFOpaque("Rst", _: DFOpaque.Rst, _) => false
        case _                                   => true
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
  def csDFDouble(): String
  
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
    case DFDouble      => csDFDouble()
    case dt: DFNothing => ???
end AbstractTypePrinter

protected trait DFTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "Boolean"
    case DFBit  => "Bit"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    val csWidth = dfType.widthParamRef.refCodeString(typeCS)
    if (typeCS) s"Bits[$csWidth]"
    else s"Bits($csWidth)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val csWidth = dfType.widthParamRef.refCodeString(typeCS)
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"UInt$ob$csWidth$cb"
      case (true, 0) =>
        if (dfType.isDFInt32) "Int"
        else s"SInt$ob$csWidth$cb"
      case (false, _) => s"UFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"SFix$ob$magnitudeWidth, $fractionWidth$cb"

  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(IntParamRef(dfType.width)), Some(v))})"
        )
        .mkString("\n")
        .hindent
    s"enum ${enumName}(val value: ${csDFDecimal(DFUInt(IntParamRef(dfType.width)), true)} <> CONST) extends Encoded.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.getName
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val dimStr =
      if (cellDimParamRefs.size == 1) cellDimParamRefs.head.refCodeString(typeCS).applyBrackets()
      else cellDimParamRefs.map(_.refCodeString(typeCS)).mkStringBrackets
    s"${csDFType(cellType, typeCS)} X $dimStr"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    val csActualType = csDFType(dfType.actualType)
    val extendee = dfType.id match
      case _: DFOpaque.Clk      => s"Clk"
      case _: DFOpaque.Rst      => s"Rst"
      case _: DFOpaque.MagnetId => s"Magnet($csActualType)"
      case _                    => s"Opaque($csActualType)"
    s"case class ${dfType.getName}() extends $extendee"
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
  def csDFDouble(): String = "Double"
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
  def csDFValType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> VAL"
  def csDFValConstType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> CONST"
end DFTypePrinter
