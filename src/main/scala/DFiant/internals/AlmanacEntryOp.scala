package DFiant.internals

import DFiant.tokens._

///////////////////////////////////////////////////////////////////////////////////////
//(:=) Identity assignment
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryAssign private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntry {
  val id : AlmanacID = arg0.id
  val address : AlmanacAddress = Almanac.getCurrentAddress
  val bitsRange : BitsRange = arg0.bitsRange
  val timeRef : AlmanacTimeRef = arg0.timeRef
  val init : Seq[Token] = arg0.init

  override def toString: String = s"$arg0 := $arg1"
  if (Almanac.printEntrees) {
    println(this)
  }

  def codeString: String = "BADCODE_AlmanacEntryAssign"
}
object AlmanacEntryAssign {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}

//Set Operation Entry. Used for an
abstract class AlmanacEntryOp(outWidth : Int, val init : Seq[Token]) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(outWidth)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current

  if (Almanac.printEntrees) {
    println(this)
  }
}

class AlmanacEntryOp1 private (arg0 : AlmanacEntry, opString : String, outWidth : Int, init : Seq[Token]) extends
  AlmanacEntryOp(outWidth, init) {
  override def toString: String = s"${super.toString} := $opString$arg0"
  def codeString: String = "BADCODE_AlmanacEntryOp1"
}
object AlmanacEntryOp1 {
  def apply(arg0 : AlmanacEntry, opString : String, outWidth : Int, init : Seq[Token]) : AlmanacEntryOp =
    Almanac.fetchEntry(new AlmanacEntryOp1(arg0, opString, outWidth, init))
}

/////////////////////////////////////////////////////////////////////////////////////////
////(!, ~) Invert (Not)
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpInv private (arg0 : AlmanacEntry) extends AlmanacEntryOp1(arg0) {
//  def opString : String = "~"
//}
//object AlmanacEntryOpInv {
//  def apply(arg0 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpInv(arg0))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(-) Negate (as a prefix operator)
/////////////////////////////////////////////////////////////////////////////////////////
////TBD. May be changed to (0 - arg)
//class AlmanacEntryOpNeg private (arg0 : AlmanacEntry) extends AlmanacEntryOp1(arg0) {
//  def opString : String = "-"
//}
//object AlmanacEntryOpNeg {
//  def apply(arg0 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpNeg(arg0))
//}


class AlmanacEntryOp2 private (arg0 : AlmanacEntry, arg1 : AlmanacEntry, opString : String, outWidth : Int, init : Seq[Token]) extends
  AlmanacEntryOp(outWidth, init) {
  override def toString: String = s"${super.toString} := $arg0 $opString $arg1"
  def codeString: String = "BADCODE_AlmanacEntryOp2"
}
object AlmanacEntryOp2 {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry, opString : String, outWidth : Int, init : Seq[Token]) : AlmanacEntryOp =
    Almanac.fetchEntry(new AlmanacEntryOp2(arg0, arg1, opString, outWidth, init))
}

//
/////////////////////////////////////////////////////////////////////////////////////////
////(^) XOR
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpXor private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = "^"
//}
//object AlmanacEntryOpXor {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpXor(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(|, ||) OR
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpOr private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = "|"
//}
//object AlmanacEntryOpOr {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpOr(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(&, &&) And
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpAnd private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = "&"
//}
//object AlmanacEntryOpAnd {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpAnd(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(##) Concatenation
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpCat private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  override val bitsRange : BitsRange = arg0.bitsRange + arg1.bitsRange
//  def opString : String = "##"
//}
//object AlmanacEntryOpCat {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpCat(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(<<) Left Shift
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpLsh private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = "<<"
//}
//object AlmanacEntryOpLsh {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpLsh(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(>>) Right Shift
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpRsh private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = ">>"
//}
//object AlmanacEntryOpRsh {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpRsh(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(+) Add
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpAdd private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  override val bitsRange : BitsRange = arg0.bitsRange.max(arg1.bitsRange).incBy(1)
//  def opString : String = "+"
//}
//object AlmanacEntryOpAdd {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpAdd(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(-) Subtract
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpSub private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  def opString : String = "-"
//}
//object AlmanacEntryOpSub {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpSub(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(==) Equals
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpEq private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  override val bitsRange : BitsRange = BitsRange(0,0)
//  def opString : String = "=="
//}
//object AlmanacEntryOpEq {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpEq(arg0, arg1))
//}
//
/////////////////////////////////////////////////////////////////////////////////////////
////(<) Less Than
/////////////////////////////////////////////////////////////////////////////////////////
//class AlmanacEntryOpLsTn private (arg0 : AlmanacEntry, arg1 : AlmanacEntry) extends AlmanacEntryOp2(arg0, arg1) {
//  override val bitsRange : BitsRange = BitsRange(0,0)
//  def opString : String = "<"
//}
//object AlmanacEntryOpLsTn {
//  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry) = Almanac.fetchEntry(new AlmanacEntryOpLsTn(arg0, arg1))
//}



//TBD
trait AlmanacTypeIO extends AlmanacEntry {

}

