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

  def codeString: String = s"$arg0 := $arg1"
}
object AlmanacEntryAssign {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry)(implicit almanac : Almanac) = almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}

class AlmanacEntryOp private (outWidth : Int, opString : String, val init : Seq[Token], val args : Seq[AlmanacEntry], codeStringBld : String => String, refCodeStringBld : String => String) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(outWidth)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  override def refCodeString: String = refCodeStringBld(s"$id")
  def codeString: String = codeStringBld(s"$id")
}
object AlmanacEntryOp {
  def apply(outWidth : Int, opString : String, init : Seq[Token], args : Seq[AlmanacEntry], codeStringBld : String => String, refCodeStringBld : String => String)(implicit almanac : Almanac) : AlmanacEntryOp =
    almanac.fetchEntry(new AlmanacEntryOp(outWidth, opString, init, args, codeStringBld, refCodeStringBld))
}




//TBD
trait AlmanacTypeIO extends AlmanacEntry {

}

