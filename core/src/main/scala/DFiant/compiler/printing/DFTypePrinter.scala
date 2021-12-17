package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*

protected trait DFTypePrinter extends AbstractPrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "DFBool"
    case DFBit  => "DFBit"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    if (typeCS) s"DFBits[${dfType.width}]"
    else s"DFBits(${dfType.width})"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"DFUInt$ob$width$cb"
      case (true, 0)  => s"DFSInt$ob$width$cb"
      case (false, _) => s"DFUFix$ob$magnitudeWidth, $fractionWidth$cb"
      case (true, _)  => s"DFSFix$ob$magnitudeWidth, $fractionWidth$cb"

  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = dfType.name
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    s"${csDFType(cellType, typeCS)}.X${cellDims.mkStringBrackets}"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = dfType.name
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String = dfType.name match
    case DFStruct.ReservedTupleName =>
      csDFTuple(dfType.fieldMap.values.toList, typeCS)
    case n => n
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets

  def csDFType(dfType: DFType, typeCS: Boolean = false): String = dfType match
    case dt: DFBoolOrBit => csDFBoolOrBit(dt, typeCS)
    case dt: DFBits      => csDFBits(dt, typeCS)
    case dt: DFDecimal   => csDFDecimal(dt, typeCS)
    case dt: DFEnum      => csDFEnum(dt, typeCS)
    case dt: DFVector    => csDFVector(dt, typeCS)
    case dt: DFOpaque    => csDFOpaque(dt, typeCS)
    case dt: DFStruct    => csDFStruct(dt, typeCS)
    case NoType          => NoType.noTypeErr
end DFTypePrinter
