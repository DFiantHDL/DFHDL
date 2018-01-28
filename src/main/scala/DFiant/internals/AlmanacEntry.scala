package DFiant.internals

import DFiant.tokens._

trait AlmanacGuard {

}



trait AlmanacEntry {
  val id : AlmanacID
  val address : AlmanacAddress
  val bitsRange : BitsRange
  val timeRef : AlmanacTimeRef
  val init : Seq[Token]
  //`signed` indicates whether or not entry is signed, meaning the MSbit indicates the sign
  val signed : Boolean = false
  val reversed : Boolean = false

  def refCodeString: String = s"$id"
  def codeString : String
  def simInject(that : BigInt) : Boolean = ???
  final override def toString: String = codeString
  Almanac.addEntry(this)
}



//Get Entry. Used when reading a DF variable.
trait AlmanacEntryGet extends AlmanacEntry

//Set Empty Entry. Used when writing an empty value to a DF variable.
//Typically done when a new DF variable is created.
//class AlmanacEntrySetEmpty(id : AlmanacID, address : AlmanacAddress, bitsRange : BitsRange) extends AlmanacEntry(id, address, bitsRange) {
//  override def toString: String = "<EMPTY>"
//}

//Set Constant Entry. Used for constant value assignment or operation.
class AlmanacEntryConst private (token : Token) extends AlmanacEntry {
  val id : AlmanacID = AlmanacIDConst(token)
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(token.width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = Seq(token)
  def codeString : String = token.codeString
}

object AlmanacEntryConst {
  def apply(token : Token) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryConst(token))
}


class AlmanacEntryNewDFVar private (width : Int, val init : Seq[Token], codeStringBld : String => String) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  def codeString : String = codeStringBld(refCodeString)
}

object AlmanacEntryNewDFVar {
  def apply(width : Int, init : Seq[Token], codeStringBld : String => String) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryNewDFVar(width, init, codeStringBld))
}


class AlmanacEntryAliasDFVar private (aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, val timeRef: AlmanacTimeRef, val init : Seq[Token], codeStringBld : String => String) extends AlmanacEntry {
  val id : AlmanacID = aliasedEntry.id
  val address : AlmanacAddress = aliasedEntry.address
  val bitsRange : BitsRange = aliasedEntry.bitsRange.subRangeRel(relBitsRange)
  override def refCodeString : String = codeStringBld(s"$id")
  def codeString : String = refCodeString
}

object AlmanacEntryAliasDFVar {
  def apply(aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, timeRef: AlmanacTimeRef, init : Seq[Token], codeStringBld : String => String) : AlmanacEntry =
    Almanac.fetchEntry(new AlmanacEntryAliasDFVar(aliasedEntry, relBitsRange, timeRef, init, codeStringBld))
}


class AlmanacEntryGetDFVar private (varEntry : AlmanacEntry) extends AlmanacEntry {
  val id : AlmanacID = varEntry.id
  val address : AlmanacAddress = Almanac.getCurrentAddress
  val bitsRange : BitsRange = varEntry.bitsRange
  val timeRef : AlmanacTimeRef = varEntry.timeRef
  val init : Seq[Token] = varEntry.init //TODO: consider changing
  def codeString : String = "BADCODE_AlmanacEntryGetDFVar"
}

object AlmanacEntryGetDFVar {
  def apply(varEntry : AlmanacEntry) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryGetDFVar(varEntry))
}


import scala.collection.mutable.MutableList
class AlmanacEntryStruct private (width : Int, val structEntryList : MutableList[AlmanacEntry]) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = ??? //Should be a concatenation of the inits
  def codeString : String = "BADCODE_AlmanacEntryStruct"
}

object AlmanacEntryStruct {
  def apply(width : Int) : AlmanacEntryStruct = Almanac.fetchEntry(new AlmanacEntryStruct(width, MutableList()))
}
