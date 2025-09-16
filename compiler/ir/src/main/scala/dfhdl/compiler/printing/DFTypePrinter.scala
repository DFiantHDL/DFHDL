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
        case DFOpaque(name = "Clk", kind = DFOpaque.Kind.Clk) => false
        case DFOpaque(name = "Rst", kind = DFOpaque.Kind.Rst) => false
        case _                                                => true
      }
      .map(x => printer.csNamedDFTypeDcl(x, global = true))
      .mkString("\n")
  final def csLocalTypeDcls(design: DFDesignBlock): String =
    getSet.designDB.getLocalNamedDFTypes(design).view
      .filter {
        // show tuple structures only if tuple support is disabled
        case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
        // skipping unknown clock and reset definitions (they are unknown because
        // they lack additional name suffix that belongs to their configuration)
        case DFOpaque(name = "Clk", kind = DFOpaque.Kind.Clk) => false
        case DFOpaque(name = "Rst", kind = DFOpaque.Kind.Rst) => false
        case _                                                => true
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
  def csDFTime(dfType: DFTime, typeCS: Boolean): String
  def csDFFreq(dfType: DFFreq, typeCS: Boolean): String
  def csDFNumber(dfType: DFNumber, typeCS: Boolean): String
  def csDFString(dfType: DFString, typeCS: Boolean): String

  final def csDFType(dfType: DFType, typeCS: Boolean = false): String = dfType match
    case dt: DFBoolOrBit                                  => csDFBoolOrBit(dt, typeCS)
    case dt: DFBits                                       => csDFBits(dt, typeCS)
    case dt: DFDecimal                                    => csDFDecimal(dt, typeCS)
    case dt: DFEnum                                       => csDFEnum(dt, typeCS)
    case dt: DFVector                                     => csDFVector(dt, typeCS)
    case dt: DFOpaque                                     => csDFOpaque(dt, typeCS)
    case dt: DFStruct if dt.isTuple && tupleSupportEnable =>
      csDFTuple(dt.fieldMap.values.toList, typeCS)
    case dt: DFStruct  => csDFStruct(dt, typeCS)
    case dt: DFUnit    => csDFUnit(dt, typeCS)
    case DFDouble      => csDFDouble()
    case dt: DFTime    => csDFTime(dt, typeCS)
    case dt: DFFreq    => csDFFreq(dt, typeCS)
    case dt: DFNumber  => csDFNumber(dt, typeCS)
    case dt: DFString  => csDFString(dt, typeCS)
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
      case (true, 0)  =>
        if (dfType.isDFInt32) "Int"
        else s"SInt$ob$csWidth$cb"
      case (false, _) => s"UFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"SFix$ob$magnitudeWidth, $fractionWidth$cb"
  def csDFString(dfType: DFString, typeCS: Boolean): String = "String"
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.name
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(IntParamRef(dfType.width)), Some(v))})"
        )
        .mkString("\n")
        .hindent
    s"enum ${enumName}(val value: ${csDFDecimal(DFUInt(IntParamRef(dfType.width)), true)} <> CONST) extends Encoded.Manual(${dfType.width}):\n$entries"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.name
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val dimStr =
      if (cellDimParamRefs.size == 1) cellDimParamRefs.head.refCodeString(typeCS).applyBrackets()
      else cellDimParamRefs.map(_.refCodeString(typeCS)).mkStringBrackets
    s"${csDFType(cellType, typeCS)} X $dimStr"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    val csActualType = csDFType(dfType.actualType)
    val extendee = dfType.kind match
      case DFOpaque.Kind.Clk    => s"Clk"
      case DFOpaque.Kind.Rst    => s"Rst"
      case DFOpaque.Kind.Magnet => s"Magnet($csActualType)"
      case _                    => s"Opaque($csActualType)"
    s"case class ${dfType.name}() extends $extendee"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.name
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}${csDFValType(t)}")
      .mkString("\n")
      .hindent(2)
    s"final case class ${dfType.name}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    dfType.name
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = "Unit"
  def csDFDouble(): String = "Double"
  def csDFTime(dfType: DFTime, typeCS: Boolean): String = "Time"
  def csDFFreq(dfType: DFFreq, typeCS: Boolean): String = "Freq"
  def csDFNumber(dfType: DFNumber, typeCS: Boolean): String = "Number"
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
  def csDFValType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> VAL"
  def csDFValConstType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> CONST"
end DFTypePrinter
