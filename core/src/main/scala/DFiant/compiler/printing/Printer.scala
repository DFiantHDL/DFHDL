package DFiant.compiler
package printing
import ir.*
import DFiant.core.Context

trait Printer:
  def csDFBoolOrBit(dfType: DFBoolOrBit): String = ???
  def csDFBits(dfType: DFBits): String = ???
  def csDFDecimal(dfType: DFDecimal): String = ???
  def csDFEnum(dfType: DFEnum): String = ???
  def csDFVector(dfType: DFVector): String = ???
  def csDFOpaque(dfType: DFOpaque): String = ???
  def csDFUnion(dfType: DFUnion): String = ???
  def csDFStruct(dfType: DFStruct): String = ???
  def csDFTuple(dfType: DFTuple): String = ???

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
