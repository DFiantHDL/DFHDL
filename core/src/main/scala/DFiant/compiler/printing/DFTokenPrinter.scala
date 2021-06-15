package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*

protected trait DFTokenPrinter:
  def csDFBitsData(dfType: DFBits, data: (BitVector, BitVector)): String = ???
  def csDFBoolOrBitData(dfType: DFBoolOrBit, data: Option[Boolean]): String =
    data match
      case Some(value) =>
        dfType match
          case DFBool => data.toString
          case DFBit  => if (value) "1" else "0"
      case None => "?"
  def csDFDecimalData(dfType: DFDecimal, data: Option[BigInt]): String = ???
  def csDFEnumData(dfType: DFEnum, data: Option[BigInt]): String = ???
  def csDFVectorData(dfType: DFVector, data: Vector[DFToken]): String = ???
  def csDFOpaqueData(dfType: DFOpaque, data: DFToken): String =
    s"${dfType.name}(${csDFToken(data)})"
  def csDFUnionData(dfType: DFUnion, data: DFToken): String = csDFToken(data)
  def csDFStructData(dfType: DFStruct, data: List[DFToken]): String = ???
  def csDFTupleData(dfType: DFTuple, data: List[DFToken]): String =
    data.map(csDFToken).mkStringBrackets
  def csDFToken(token: DFToken): String = (token.dfType, token.data) match
    case (dt: DFBits, data: (BitVector, BitVector) @unchecked) =>
      csDFBitsData(dt, data)
    case (dt: DFBoolOrBit, data: Option[Boolean] @unchecked) =>
      csDFBoolOrBitData(dt, data)
    case (dt: DFDecimal, data: Option[BigInt] @unchecked) =>
      csDFDecimalData(dt, data)
    case (dt: DFEnum, data: Option[BigInt] @unchecked) =>
      csDFEnumData(dt, data)
    case (dt: DFVector, data: Vector[DFToken] @unchecked) =>
      csDFVectorData(dt, data)
    case (dt: DFOpaque, data: DFToken) =>
      csDFOpaqueData(dt, data)
    case (dt: DFUnion, data: DFToken) =>
      csDFUnionData(dt, data)
    case (dt: DFStruct, data: List[DFToken] @unchecked) =>
      csDFStructData(dt, data)
    case (dt: DFTuple, data: List[DFToken] @unchecked) =>
      csDFTupleData(dt, data)
    case x =>
      throw new IllegalArgumentException(
        s"Unexpected token type and data found: $x"
      )
