package DFiant.compiler
package ir
import printing.{Printer, NCCode}
import DFiant.internals.*
import scala.collection.immutable.{ListMap, ListSet}

sealed trait DFType extends NCCode, Product, Serializable:
  val __width: Int

//sealed trait DFToken extends NCCode, Product, Serializable:
//  val __dfType: DFType
//  val data: Any
//  protected[DFiant] lazy val valueBits: BitVector
//  protected[DFiant] lazy val bubbleBits: BitVector
////  def ==[R <: DFType](rhs: DFToken): DFBool.Token =
////    dfType.tokenEquals(this, rhs)
////  def codeString(using Printer): String = dfType.tokenCodeString(data)
//object DFToken:
//  sealed trait DFOptional extends DFToken:
//    val data: Option[Any]
//    final lazy val (valueBits, bubbleBits): (BitVector, BitVector) =
//      value match {
//        case Some(t) => (valueToBitVector(t), false.toBitVector(width))
//        case None    => (0.toBitVector(width), true.toBitVector(width))
//      }
//    def valueToBitVector(value: Value): BitVector
//    def valueCodeString(value: Value)(implicit printer: CSPrinter): String

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  final val __width = 1
object DFBoolOrBit
//  final case class Token(__dfType: DFBoolOrBit, data: Option[Boolean])
//      extends DFToken:
//    protected[DFiant] lazy val valueBits: BitVector = data._1
//    protected[DFiant] lazy val bubbleBits: BitVector = data._2
//    def codeString(using Printer): String = ???

case object DFBool extends DFBoolOrBit:
  def codeString(using Printer): String = "DFBool"
case object DFBit extends DFBoolOrBit:
  def codeString(using Printer): String = "DFBit"
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(val __width: Int) extends DFType:
  def codeString(using Printer): String = s"DFBits(${__width})"
object DFBits
//  final case class Token(__dfType: DFBits, data: (BitVector, BitVector))
//      extends DFToken:
//    protected[DFiant] lazy val valueBits: BitVector = data._1
//    protected[DFiant] lazy val bubbleBits: BitVector = data._2
//    def codeString(using Printer): String = ???
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    val name: String,
    val __width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType:
  def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDims: List[Int]
) extends DFType:
  val __width: Int = cellType.__width * cellDims.reduce(_ * _)
  def codeString(using Printer): String =
    s"${cellType.codeString}.X${cellDims.mkString("(", ", ", ")")}"
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(name: String, actualType: DFType) extends DFType:
  final val __width: Int = actualType.__width
  final def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion(fieldSet: ListSet[DFType]) extends DFType:
  val __width: Int = fieldSet.head.__width
  def codeString(using Printer): String =
    fieldSet.map(_.codeString).mkString(" | ")
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct(
    name: String,
    fieldMap: ListMap[String, DFType]
) extends DFType:
  val __width: Int = fieldMap.values.map(_.__width).sum
  def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple(fieldList: List[DFType]) extends DFType:
  val __width: Int = fieldList.view.map(_.__width).sum
  def codeString(using Printer): String =
    fieldList.view.map(_.codeString).mkString("(", ", ", ")")
/////////////////////////////////////////////////////////////////////////////
