package DFiant.internals

import DFiant.DFAny.Token
import DFiant.DFDir

trait AlmanacGuard {

}



sealed abstract class AlmanacEntry(implicit val almanac : Almanac) {
  val id : AlmanacID
  val address : AlmanacAddress
  val bitsRange : BitsRange
  val timeRef : AlmanacTimeRef
  val init : Seq[Token]
  //`signed` indicates whether or not entry is signed, meaning the MSbit indicates the sign
  val signed : Boolean = false
  val reversed : Boolean = false

  def codeString : String
  def simInject(that : BigInt) : Boolean = ???
  final override def toString: String = codeString
  almanac.addEntry(this)
}

trait AlmanacEntryNamed extends AlmanacEntry {
  val name : String
}

//Get Entry. Used when reading a DF variable.
//trait AlmanacEntryGet extends AlmanacEntry

//Set Empty Entry. Used when writing an empty value to a DF variable.
//Typically done when a new DF variable is created.
//class AlmanacEntrySetEmpty(id : AlmanacID, address : AlmanacAddress, bitsRange : BitsRange) extends AlmanacEntry(id, address, bitsRange) {
//  override def toString: String = "<EMPTY>"
//}

//Set Constant Entry. Used for constant value assignment or operation.
final class AlmanacEntryConst private (token : Token, val name : String, _codeString : => String)(implicit almanac : Almanac) extends AlmanacEntryNamed {
  val id : AlmanacID = AlmanacIDConst(token)
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(token.width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = Seq(token)
  def codeString : String = _codeString
}

object AlmanacEntryConst {
  def apply(token : Token, name : String, codeString : => String)(implicit almanac : Almanac) : AlmanacEntryConst =
    almanac.fetchEntry(new AlmanacEntryConst(token, name, codeString))
}


final class AlmanacEntryNewDFVar private (width : Int, val init : Seq[Token], val name : String, _codeString : => String)(implicit almanac : Almanac) extends AlmanacEntryNamed {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  lazy val fullName : String = s"${almanac.fullName}.$name"
  def codeString : String = _codeString
}

object AlmanacEntryNewDFVar {
  def apply(width : Int, init : Seq[Token], name : String, codeString : String)(implicit almanac : Almanac) : AlmanacEntryNewDFVar =
    almanac.fetchEntry(new AlmanacEntryNewDFVar(width, init, name, codeString))
}

sealed trait AliasReference {
  val aliasCodeString : String
}
object AliasReference {
  case class AsIs(aliasCodeString : String) extends AliasReference
  case class BitsWL(relWidth : Int, relBitLow : Int, aliasCodeString : String) extends AliasReference
  case class Prev(step : Int) extends AliasReference {
    val aliasCodeString : String = if (step == 1) ".prev" else s".prev($step)"
  }
  case class BitReverse(aliasCodeString : String) extends AliasReference
  case class Invert(aliasCodeString : String) extends AliasReference
}


final class AlmanacEntryAliasDFVar private (val aliasedEntry : AlmanacEntryNamed, aliasReference : AliasReference, val init : Seq[Token], val name : String, _codeString : => String)(implicit almanac : Almanac) extends AlmanacEntryNamed {
  val id : AlmanacID = aliasedEntry.id
  val address : AlmanacAddress = aliasedEntry.address
  val bitsRange : BitsRange = aliasedEntry.bitsRange.subRangeRel(aliasReference match {
    case AliasReference.BitsWL(relWidth, relBitLow, _) => BitsRange(relBitLow + relWidth - 1, relBitLow)
    case _ => aliasedEntry.bitsRange
  })
  val timeRef : AlmanacTimeRef = aliasReference match {
    case AliasReference.Prev(step) => aliasedEntry.timeRef.stepBy(-step)
    case _ => aliasedEntry.timeRef
  }
  def codeString : String = _codeString
}

object AlmanacEntryAliasDFVar {
  def apply(aliasedEntry : AlmanacEntryNamed, aliasReference : AliasReference, init : Seq[Token], name : String, codeString : => String)(implicit almanac : Almanac) : AlmanacEntryAliasDFVar =
    almanac.fetchEntry(new AlmanacEntryAliasDFVar(aliasedEntry, aliasReference, init, name, codeString))
}


final class AlmanacEntryGetDFVar private (varEntry : AlmanacEntryNamed)(implicit almanac : Almanac) extends AlmanacEntryNamed {
  val id : AlmanacID = varEntry.id
  val address : AlmanacAddress = almanac.getCurrentAddress
  val bitsRange : BitsRange = varEntry.bitsRange
  val timeRef : AlmanacTimeRef = varEntry.timeRef
  val init : Seq[Token] = varEntry.init //TODO: consider changing
  val name : String = varEntry.name
  def codeString : String = s"$id.consume" //TODO: consider changing
}

object AlmanacEntryGetDFVar {
  def apply(varEntry : AlmanacEntryNamed)(implicit almanac : Almanac) : AlmanacEntryGetDFVar =
    almanac.fetchEntry(new AlmanacEntryGetDFVar(varEntry))
}

final class AlmanacEntryPort private (width : Int, val _init : Seq[Token], val sourceEntry : Option[AlmanacEntryNamed], val dir : DFDir, val name : String, _codeString : => String)(implicit almanac : Almanac) extends AlmanacEntryNamed {
  val id : AlmanacID = if (sourceEntry.isDefined) sourceEntry.get.id else AlmanacID()
  val address : AlmanacAddress = if (sourceEntry.isDefined) sourceEntry.get.address else AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val init : Seq[Token] = if (sourceEntry.isDefined) sourceEntry.get.init else _init
  val timeRef : AlmanacTimeRef = if (sourceEntry.isDefined) sourceEntry.get.timeRef else AlmanacTimeRef.Current
  lazy val fullName : String = s"${almanac.fullName}.$name"
  def codeString : String = _codeString
}

object AlmanacEntryPort {
  def apply(width : Int, init : Seq[Token], sourceEntry : Option[AlmanacEntryNamed], dir : DFDir, name : String, codeString : => String)(implicit almanac : Almanac) : AlmanacEntryPort =
    almanac.fetchEntry(new AlmanacEntryPort(width, init, sourceEntry, dir, name, codeString))
}



import scala.collection.mutable.MutableList
final class AlmanacEntryStruct private (width : Int, val structEntryList : MutableList[AlmanacEntry])(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : Seq[Token] = ??? //Should be a concatenation of the inits
  def codeString : String = "BADCODE_AlmanacEntryStruct"
}

object AlmanacEntryStruct {
  def apply(width : Int)(implicit almanac : Almanac) : AlmanacEntryStruct =
    almanac.fetchEntry(new AlmanacEntryStruct(width, MutableList()))
}


///////////////////////////////////////////////////////////////////////////////////////
//(:=) Identity assignment
///////////////////////////////////////////////////////////////////////////////////////
final class AlmanacEntryAssign private (arg0 : AlmanacEntryNamed, arg1 : AlmanacEntryNamed)(implicit almanac : Almanac) extends AlmanacEntry {
  val id : AlmanacID = arg0.id
  val address : AlmanacAddress = almanac.getCurrentAddress
  val bitsRange : BitsRange = arg0.bitsRange
  val timeRef : AlmanacTimeRef = arg0.timeRef
  val init : Seq[Token] = arg0.init

  def codeString: String = s"${arg0.name} := ${arg1.name}"
}
object AlmanacEntryAssign {
  def apply(arg0 : => AlmanacEntryNamed, arg1 : => AlmanacEntryNamed)(implicit almanac : Almanac) : AlmanacEntryAssign =
    almanac.fetchEntry(new AlmanacEntryAssign(arg0, arg1))
}
