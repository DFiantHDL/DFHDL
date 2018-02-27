package DFiant.internals

import DFiant.DFAny.Token

trait AlmanacGuard {

}



abstract class AlmanacEntry(implicit almanac : Almanac) {
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
  almanac.addEntry(this)
}



//Get Entry. Used when reading a DF variable.
//trait AlmanacEntryGet extends AlmanacEntry

//Set Empty Entry. Used when writing an empty value to a DF variable.
//Typically done when a new DF variable is created.
//class AlmanacEntrySetEmpty(id : AlmanacID, address : AlmanacAddress, bitsRange : BitsRange) extends AlmanacEntry(id, address, bitsRange) {
//  override def toString: String = "<EMPTY>"
//}

//Set Constant Entry. Used for constant value assignment or operation.
class AlmanacEntryConst private (token : Token)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = AlmanacIDConst(token)
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(token.width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = Seq(token)
  def codeString : String = token.codeString
}

object AlmanacEntryConst {
  def apply(token : Token)(implicit almanac : Almanac) : AlmanacEntry = almanac.fetchEntry(new AlmanacEntryConst(token))
}


class AlmanacEntryNewDFVar private (width : Int, val init : Seq[Token], codeStringBld : String => String)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  def codeString : String = codeStringBld(refCodeString)
}

object AlmanacEntryNewDFVar {
  def apply(width : Int, init : Seq[Token], codeStringBld : String => String)(implicit almanac : Almanac) : AlmanacEntry =
    almanac.fetchEntry(new AlmanacEntryNewDFVar(width, init, codeStringBld))
}


class AlmanacEntryAliasDFVar private (aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, val timeRef: AlmanacTimeRef, val init : Seq[Token], codeStringBld : String => String)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = aliasedEntry.id
  val address : AlmanacAddress = aliasedEntry.address
  val bitsRange : BitsRange = aliasedEntry.bitsRange.subRangeRel(relBitsRange)
  override def refCodeString : String = codeStringBld(s"$id")
  def codeString : String = refCodeString
}

object AlmanacEntryAliasDFVar {
  def apply(aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, timeRef: AlmanacTimeRef, init : Seq[Token], codeStringBld : String => String)(implicit almanac : Almanac) : AlmanacEntry =
    almanac.fetchEntry(new AlmanacEntryAliasDFVar(aliasedEntry, relBitsRange, timeRef, init, codeStringBld))
}


class AlmanacEntryGetDFVar private (varEntry : AlmanacEntry)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = varEntry.id
  val address : AlmanacAddress = almanac.getCurrentAddress
  val bitsRange : BitsRange = varEntry.bitsRange
  val timeRef : AlmanacTimeRef = varEntry.timeRef
  val init : Seq[Token] = varEntry.init //TODO: consider changing
  def codeString : String = s"$id.consume" //TODO: consider changing
}

object AlmanacEntryGetDFVar {
  def apply(varEntry : AlmanacEntry)(implicit almanac : Almanac) : AlmanacEntry =
    almanac.fetchEntry(new AlmanacEntryGetDFVar(varEntry))
}


import scala.collection.mutable.MutableList
class AlmanacEntryStruct private (width : Int, val structEntryList : MutableList[AlmanacEntry])(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = ??? //Should be a concatenation of the inits
  def codeString : String = "BADCODE_AlmanacEntryStruct"
}

object AlmanacEntryStruct {
  def apply(width : Int)(implicit almanac : Almanac) : AlmanacEntryStruct = almanac.fetchEntry(new AlmanacEntryStruct(width, MutableList()))
}
