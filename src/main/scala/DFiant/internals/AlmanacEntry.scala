package DFiant.internals

import DFiant.DFAny.Token
import DFiant.DFDir

trait AlmanacGuard {

}



sealed abstract class AlmanacEntry(implicit almanac : Almanac) {
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


class AlmanacEntryNewDFVar private (width : Int, val init : Seq[Token], val name : String, codeStringBld : String => String)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val fullName : String = s"${almanac.fullName}.$name"
  override def refCodeString: String = name
  def codeString : String = codeStringBld(refCodeString)
}

object AlmanacEntryNewDFVar {
  def apply(width : Int, init : Seq[Token], name : String, codeStringBld : String => String)(implicit almanac : Almanac) : AlmanacEntry =
    almanac.fetchEntry(new AlmanacEntryNewDFVar(width, init, name, codeStringBld))
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

class AlmanacEntryPort private (width : Int, val _init : Seq[Token], val sourceEntry : Option[AlmanacEntry], val dir : DFDir, val name : String)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = if (sourceEntry.isDefined) sourceEntry.get.id else AlmanacID()
  val address : AlmanacAddress = if (sourceEntry.isDefined) sourceEntry.get.address else AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val init : Seq[Token] = if (sourceEntry.isDefined) sourceEntry.get.init else _init
  val timeRef : AlmanacTimeRef = if (sourceEntry.isDefined) sourceEntry.get.timeRef else AlmanacTimeRef.Current
  val fullName : String = s"${almanac.fullName}.$name"
  override def refCodeString: String = name
  def codeString : String = s"$fullName"
}

object AlmanacEntryPort {
  def apply(width : Int, init : Seq[Token], sourceEntry : Option[AlmanacEntry], dir : DFDir, name : String)(implicit almanac : Almanac) : AlmanacEntryPort =
    almanac.fetchEntry(new AlmanacEntryPort(width, init, sourceEntry, dir, name))
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
  def apply(arg0 : => AlmanacEntry, arg1 : => AlmanacEntry)(implicit almanac : Almanac) = almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}
