package DFiant.internals

import DFiant.DFAny.Token

///////////////////////////////////////////////////////////////////////////////////////
//(:=) Identity assignment
///////////////////////////////////////////////////////////////////////////////////////
class AlmanacEntryAssign private (arg0 : AlmanacEntry, arg1 : AlmanacEntry)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = arg0.id
  val address : AlmanacAddress = almanac.getCurrentAddress
  val bitsRange : BitsRange = arg0.bitsRange
  val timeRef : AlmanacTimeRef = arg0.timeRef
  val init : Seq[Token] = arg0.init

  def codeString: String = s"$arg0 := $arg1"
}
object AlmanacEntryAssign {
  def apply(arg0 : AlmanacEntry, arg1 : AlmanacEntry)(implicit almanac : Almanac) = almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}

//TBD
trait AlmanacTypeIO extends AlmanacEntry {

}

