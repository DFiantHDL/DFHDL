package DFiant.compiler
package printing
import ir.*
import DFiant.core.Context

trait Printer:
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
    s"${csDFType(cellType)}.X${cellDims.mkString("(", ", ", ")")}"
  def csDFOpaque(dfType: DFOpaque): String = dfType.name
  def csDFUnion(dfType: DFUnion): String =
    dfType.fieldSet.map(csDFType).mkString(" | ")
  def csDFStruct(dfType: DFStruct): String = dfType.name
  def csDFTuple(dfType: DFTuple): String =
    dfType.fieldList.view.map(csDFType).mkString("(", ", ", ")")

  def csDFType(dfType: DFType): String = dfType match
    case dt: DFBoolOrBit => csDFBoolOrBit(dt)
    case dt: DFBits      => csDFBits(dt)
    case dt: DFDecimal   => csDFDecimal(dt)
    case dt: DFEnum      => csDFEnum(dt)
    case dt: DFVector    => csDFVector(dt)
    case dt: DFOpaque    => csDFOpaque(dt)
    case dt: DFUnion     => csDFUnion(dt)
    case dt: DFStruct    => csDFStruct(dt)
    case dt: DFTuple     => csDFTuple(dt)

object DefaultPrinter extends Printer
trait CPrinter extends Printer:
  val ctx: Context
