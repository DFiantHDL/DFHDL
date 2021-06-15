package DFiant.compiler
package ir
import printing.{Printer, NCCode}
import DFiant.internals.*
import scala.collection.immutable.{ListMap, ListSet}

sealed trait DFType extends NCCode, Product, Serializable:
  type TokenData
  val width: Int
object DFType:
  type Token = DFToken
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
object DFBoolOrBit:
  type TokenData = Option[Boolean]
  object Token:
    def unapply(token: DFToken): Option[(DFBoolOrBit, TokenData)] =
      token.dfType match
        case dt: DFBoolOrBit =>
          Some((dt, token.data.asInstanceOf[TokenData]))
        case _ => None

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
object DFBits:
  type TokenData = (BitVector, BitVector)
  object Token:
    def unapply(token: DFToken): Option[(DFBits, TokenData)] =
      token.dfType match
        case dt: DFBits =>
          Some((dt, token.data.asInstanceOf[TokenData]))
        case _ => None
//  final case class Token(dfType: DFBits, data: (BitVector, BitVector))
//      extends DFToken:
//    lazy val valueBits: BitVector = data._1
//    lazy val bubbleBits: BitVector = data._2
//    private def binZip(v: BitVector, b: BitVector, bubbleChar: Char): String =
//      v.toBin
//        .zip(b.toBin)
//        .map {
//          case (_, '1')       => bubbleChar
//          case (zeroOrOne, _) => zeroOrOne
//        }
//        .mkString
//    private def hexZip(
//        v: BitVector,
//        b: BitVector,
//        bubbleChar: Char,
//        allowBinMode: Boolean
//    ): Option[String] =
//      Some(
//        v.toHex
//          .zip(b.toHex)
//          .flatMap {
//            case (_, 'F' | 'f') => s"$bubbleChar"
//            case (h, '0')       => s"$h"
//            case (h, b) if allowBinMode =>
//              s"{${binZip(BitVector(h), BitVector(b), bubbleChar)}}"
//            case _ => return None
//          }
//          .mkString
//      )
//    def toBinString(bubbleChar: Char): String =
//      binZip(valueBits, bubbleBits, bubbleChar)
//    def toHexString(bubbleChar: Char, allowBinMode: Boolean): Option[String] =
//      if (width % 4 == 0)
//        hexZip(valueBits, bubbleBits, bubbleChar, allowBinMode)
//      else
//        val headWidth = width % 4
//        val (headValue, theRestValue) = valueBits.splitAt(headWidth)
//        val (headBubble, theRestBubble) = bubbleBits.splitAt(headWidth)
//
//        val headOption =
//          if (headBubble == BitVector.high(headWidth)) Some(s"$bubbleChar")
//          else
//            hexZip(
//              headValue.resize(4),
//              headBubble.resize(4),
//              bubbleChar,
//              allowBinMode
//            )
//        val theRestOption =
//          hexZip(theRestValue, theRestBubble, bubbleChar, allowBinMode)
//        for (h <- headOption; tr <- theRestOption) yield h + tr
//    def codeString(using Printer): String =
//      val binRep = toBinString('?')
//      val hexRep = s"${width}'${toHexString('?', allowBinMode = true).get}"
//      //choosing the shorter representation for readability
//      if (binRep.length <= hexRep.length) s"""b"$binRep""""
//      else s"""h"$hexRep""""

/////////////////////////////////////////////////////////////////////////////
// DFDecimal
/////////////////////////////////////////////////////////////////////////////
final case class DFDecimal(
    signed: Boolean,
    width: Int,
    fractionWidth: Int
) extends DFType:
  type TokenData = Option[BigInt]
  val magnitudeWidth: Int = width - fractionWidth
  def codeString(using Printer): String = ???

/////////////////////////////////////////////////////////////////////////////
// DFEnum
/////////////////////////////////////////////////////////////////////////////
final case class DFEnum(
    val name: String,
    val width: Int,
    val entries: ListMap[String, BigInt]
) extends DFType:
  type TokenData = Option[BigInt]
  def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFVector
/////////////////////////////////////////////////////////////////////////////
final case class DFVector(
    cellType: DFType,
    cellDims: List[Int]
) extends DFType:
  type TokenData = Vector[DFToken]
  val width: Int = cellType.width * cellDims.reduce(_ * _)
  def codeString(using Printer): String =
    s"${cellType.codeString}.X${cellDims.mkStringBrackets}"
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFOpaque
/////////////////////////////////////////////////////////////////////////////
final case class DFOpaque(name: String, actualType: DFType) extends DFType:
  type TokenData = DFToken
  final val width: Int = actualType.width
  final def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFUnion
/////////////////////////////////////////////////////////////////////////////
final case class DFUnion(fieldSet: ListSet[DFType]) extends DFType:
  type TokenData = DFToken
  val width: Int = fieldSet.head.width
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
  type TokenData = ListMap[String, DFToken]
  val width: Int = fieldMap.values.map(_.width).sum
  def codeString(using Printer): String = name
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// DFTuple
/////////////////////////////////////////////////////////////////////////////
final case class DFTuple(fieldList: List[DFType]) extends DFType:
  type TokenData = List[DFToken]
  val width: Int = fieldList.view.map(_.width).sum
  def codeString(using Printer): String =
    fieldList.view.map(_.codeString).mkStringBrackets
/////////////////////////////////////////////////////////////////////////////
