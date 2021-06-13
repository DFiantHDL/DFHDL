package DFiant.compiler
package ir
import printing.{Printer, NCCode}
import DFiant.internals.*
import scala.collection.immutable.{ListMap, ListSet}

sealed trait DFType extends NCCode, Product, Serializable:
  val __width: Int

sealed trait DFToken extends NCCode, Product, Serializable:
  val __dfType: DFType
  val __data: Any
  final lazy val __width: Int = __dfType.__width
  lazy val __valueBits: BitVector
  lazy val __bubbleBits: BitVector
//  def ==[R <: DFType](rhs: DFToken): DFBool.Token =
//    dfType.tokenEquals(this, rhs)
//  def codeString(using Printer): String = dfType.tokenCodeString(data)
object DFToken:
  sealed trait Optional extends DFToken:
    type Data
    val __data: Option[Data]
    final lazy val __valueBits: BitVector =
      __data match
        case Some(t) => dataToBitVector(t)
        case None    => 0.toBitVector(__width)
    final lazy val __bubbleBits: BitVector =
      __data match
        case Some(t) => false.toBitVector(__width)
        case None    => true.toBitVector(__width)
    protected def dataToBitVector(data: Data): BitVector
    protected def dataCodeString(data: Data)(using Printer): String
    final def codeString(using Printer): String = __data match {
      case Some(t) => dataCodeString(t)
      case None    => "?"
    }

/////////////////////////////////////////////////////////////////////////////
// DFBool or DFBit
/////////////////////////////////////////////////////////////////////////////
sealed trait DFBoolOrBit extends DFType:
  final val __width = 1
object DFBoolOrBit:
  final case class Token(__dfType: DFBoolOrBit, __data: Option[Boolean])
      extends DFToken.Optional:
    type Data = Boolean
    protected def dataToBitVector(data: Data): BitVector =
      data.toBitVector(__width)
    protected def dataCodeString(data: Data)(using Printer): String =
      __dfType match
        case DFBool => __data.toString
        case DFBit  => if (data) "1" else "0"

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
object DFBits:
  final case class Token(__dfType: DFBits, __data: (BitVector, BitVector))
      extends DFToken:
    lazy val __valueBits: BitVector = __data._1
    lazy val __bubbleBits: BitVector = __data._2
    private def binZip(v: BitVector, b: BitVector, bubbleChar: Char): String =
      v.toBin
        .zip(b.toBin)
        .map {
          case (_, '1')       => bubbleChar
          case (zeroOrOne, _) => zeroOrOne
        }
        .mkString
    private def hexZip(
        v: BitVector,
        b: BitVector,
        bubbleChar: Char,
        allowBinMode: Boolean
    ): Option[String] =
      Some(
        v.toHex
          .zip(b.toHex)
          .flatMap {
            case (_, 'F' | 'f') => s"$bubbleChar"
            case (h, '0')       => s"$h"
            case (h, b) if allowBinMode =>
              s"{${binZip(BitVector(h), BitVector(b), bubbleChar)}}"
            case _ => return None
          }
          .mkString
      )
    def toBinString(bubbleChar: Char): String =
      binZip(__valueBits, __bubbleBits, bubbleChar)
    def toHexString(bubbleChar: Char, allowBinMode: Boolean): Option[String] =
      if (__width % 4 == 0)
        hexZip(__valueBits, __bubbleBits, bubbleChar, allowBinMode)
      else
        val headWidth = __width % 4
        val (headValue, theRestValue) = __valueBits.splitAt(headWidth)
        val (headBubble, theRestBubble) = __bubbleBits.splitAt(headWidth)

        val headOption =
          if (headBubble == BitVector.high(headWidth)) Some(s"$bubbleChar")
          else
            hexZip(
              headValue.resize(4),
              headBubble.resize(4),
              bubbleChar,
              allowBinMode
            )
        val theRestOption =
          hexZip(theRestValue, theRestBubble, bubbleChar, allowBinMode)
        for (h <- headOption; tr <- theRestOption) yield h + tr
    def codeString(using Printer): String =
      val binRep = toBinString('?')
      val hexRep = s"${__width}'${toHexString('?', allowBinMode = true).get}"
      //choosing the shorter representation for readability
      if (binRep.length <= hexRep.length) s"""b"$binRep""""
      else s"""h"$hexRep""""

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    __signed: Boolean,
    __width: Int,
    __fractionWidth: Int
) extends DFType:
  def codeString(using Printer): String = ???

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
