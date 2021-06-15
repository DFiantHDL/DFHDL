package DFiant.compiler
package ir
import printing.{Printer, NCCode}
import DFiant.internals.*
import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag
sealed trait DFType extends NCCode, Product, Serializable:
  val width: Int
object DFType:
  type Token = DFToken[DFType, Any]
  protected[ir] abstract class Companion[T <: DFType, D](using ClassTag[T]):
    type Token = DFToken[T, D]
    object Token:
      type Data = D
      def apply(dfType: T, data: D): DFToken[T, D] = DFToken(dfType, data)
      def unapply(token: DFType.Token): Option[(T, D)] =
        token.dfType match
          case dt: T =>
            Some((dt, token.data.asInstanceOf[D]))
          case _ => None

//object DFToken:
//  sealed trait Optional extends DFToken:
//    type Data
//    val data: Option[Data]
//    final lazy val valueBits: BitVector =
//      data match
//        case Some(t) => dataToBitVector(t)
//        case None    => 0.toBitVector(width)
//    final lazy val bubbleBits: BitVector =
//      data match
//        case Some(t) => false.toBitVector(width)
//        case None    => true.toBitVector(width)
//    protected def dataToBitVector(data: Data): BitVector
//    protected def dataCodeString(data: Data)(using Printer): String
//    final def codeString(using Printer): String = data match {
//      case Some(t) => dataCodeString(t)
//      case None    => "?"
//    }

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  final val width = 1
object DFBoolOrBit extends DFType.Companion[DFBoolOrBit, Option[Boolean]]

case object DFBool extends DFBoolOrBit:
  def codeString(using Printer): String = "DFBool"
case object DFBit extends DFBoolOrBit:
  def codeString(using Printer): String = "DFBit"
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFBits
/////////////////////////////////////////////////////////////////////////////
final case class DFBits(val width: Int) extends DFType:
  def codeString(using Printer): String = s"DFBits(${width})"
object DFBits extends DFType.Companion[DFBits, (BitVector, BitVector)]
//  final case class Token(dfType: DFBits, data: (BitVector, BitVector))
//      extends DFToken:
//    def codeString(using Printer): String =

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    signed: Boolean,
    width: Int,
    fractionWidth: Int
) extends DFType:
  val magnitudeWidth: Int = width - fractionWidth
  def codeString(using Printer): String = ???

object DFDecimal extends DFType.Companion[DFDecimal, Option[BigInt]]
/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    val name: String,
    val width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType:
  def codeString(using Printer): String = name

object DFEnum extends DFType.Companion[DFEnum, Option[BigInt]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDims: List[Int]
) extends DFType:
  val width: Int = cellType.width * cellDims.reduce(_ * _)
  def codeString(using Printer): String =
    s"${cellType.codeString}.X${cellDims.mkStringBrackets}"

object DFVector extends DFType.Companion[DFVector, Vector[DFType.Token]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(name: String, actualType: DFType) extends DFType:
  final val width: Int = actualType.width
  final def codeString(using Printer): String = name

object DFOpaque extends DFType.Companion[DFOpaque, DFType.Token]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion(fieldSet: ListSet[DFType]) extends DFType:
  val width: Int = fieldSet.head.width
  def codeString(using Printer): String =
    fieldSet.map(_.codeString).mkString(" | ")

object DFUnion extends DFType.Companion[DFUnion, DFType.Token]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFStruct
/////////////////////////////////////////////////////////////////////////////
final case class DFStruct(
    name: String,
    fieldMap: ListMap[String, DFType]
) extends DFType:
  val width: Int = fieldMap.values.map(_.width).sum
  def codeString(using Printer): String = name

object DFStruct extends DFType.Companion[DFStruct, List[DFType.Token]]
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple(fieldList: List[DFType]) extends DFType:
  val width: Int = fieldList.view.map(_.width).sum
  def codeString(using Printer): String =
    fieldList.view.map(_.codeString).mkStringBrackets

object DFTuple extends DFType.Companion[DFTuple, List[DFType.Token]]
/////////////////////////////////////////////////////////////////////////////
