package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*

protected trait DFTypePrinter extends AbstractPrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit): String = dfType match
    case DFBool => "DFBool"
    case DFBit  => "DFBit"
  def csDFBits(dfType: DFBits): String = s"DFBits(${dfType.width})"
  def csDFDecimal(dfType: DFDecimal): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"DFUInt($width)"
      case (true, 0)  => s"DFSInt($width)"
      case (false, _) => s"DFUFix($magnitudeWidth, $fractionWidth)"
      case (true, _)  => s"DFSFix($magnitudeWidth, $fractionWidth)"

  def csDFEnum(dfType: DFEnum): String = dfType.name
  def csDFVector(dfType: DFVector): String =
    import dfType.*
    s"${csDFType(cellType)}.X${cellDims.mkStringBrackets}"
  def csDFOpaque(dfType: DFOpaque): String = dfType.name
  def csDFStruct(dfType: DFStruct): String = dfType.name match
    case DFStruct.ReservedTupleName => csDFTuple(dfType.fieldMap.values.toList)
    case n                          => n
  def csDFTuple(fieldList: List[DFType]): String =
    fieldList.view.map(csDFType).mkStringBrackets

  def csDFType(dfType: DFType): String = dfType match
    case dt: DFBoolOrBit => csDFBoolOrBit(dt)
    case dt: DFBits      => csDFBits(dt)
    case dt: DFDecimal   => csDFDecimal(dt)
    case dt: DFEnum      => csDFEnum(dt)
    case dt: DFVector    => csDFVector(dt)
    case dt: DFOpaque    => csDFOpaque(dt)
    case dt: DFStruct    => csDFStruct(dt)
    case NoType          => NoType.noTypeErr
end DFTypePrinter
