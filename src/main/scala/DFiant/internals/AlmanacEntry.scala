package DFiant.internals

import DFiant.tokens._

trait AlmanacGuard {

}



trait AlmanacEntry {
  val id : AlmanacID
  val address : AlmanacAddress
  val bitsRange : BitsRange
  val timeRef : AlmanacTimeRef
  val init : AlmanacInit
  //`signed` indicates whether or not entry is signed, meaning the MSbit indicates the sign
  val signed : Boolean = false

  def simInject(that : BigInt) : Boolean = ???
  override def toString: String = s"$id[$bitsRange]"
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
class AlmanacEntryConst private (constVal : BigInt) extends AlmanacEntry {
  val id : AlmanacID = AlmanacIDConst(constVal)
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(bigIntRepWidth(constVal)-1,0)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : AlmanacInit = AlmanacInit(Token(constVal))
}

object AlmanacEntryConst {
  def apply(constVal : BigInt) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryConst(constVal))
}


class AlmanacEntryCreateDFVar private (width : Int) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width-1, 0)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : AlmanacInit = ??? //AlmanacInit(ZeroToken)
}

object AlmanacEntryCreateDFVar {
  def apply(width : Int) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryCreateDFVar(width))
}


class AlmanacEntryAliasDFVar private (aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, val timeRef: AlmanacTimeRef, val init : AlmanacInit) extends AlmanacEntry {
  val id : AlmanacID = aliasedEntry.id
  val address : AlmanacAddress = aliasedEntry.address
  val bitsRange : BitsRange = aliasedEntry.bitsRange.subRangeRel(relBitsRange)
}

object AlmanacEntryAliasDFVar {
  def apply(aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, timeRef: AlmanacTimeRef, init : AlmanacInit) : AlmanacEntry =
    Almanac.fetchEntry(new AlmanacEntryAliasDFVar(aliasedEntry, relBitsRange, timeRef, init))
}


class AlmanacEntryGetDFVar private (varEntry : AlmanacEntry) extends AlmanacEntry {
  val id : AlmanacID = varEntry.id
  val address : AlmanacAddress = Almanac.getCurrentAddress
  val bitsRange : BitsRange = varEntry.bitsRange
  val timeRef : AlmanacTimeRef = varEntry.timeRef
  val init : AlmanacInit = AlmanacInit.Bubble //TODO: consider changing to varEntry.init
}

object AlmanacEntryGetDFVar {
  def apply(varEntry : AlmanacEntry) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryGetDFVar(varEntry))
}


import scala.collection.mutable.MutableList
class AlmanacEntryStruct private (width : Int, val structEntryList : MutableList[AlmanacEntry]) extends AlmanacEntry {
  val id : AlmanacID = AlmanacID()
  val address : AlmanacAddress = AlmanacAddressLatest
  val bitsRange : BitsRange = BitsRange(width-1, 0)
  val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current
  val init : AlmanacInit = ??? //Should be a concatination of the inits
}

object AlmanacEntryStruct {
  def apply(width : Int) : AlmanacEntryStruct = Almanac.fetchEntry(new AlmanacEntryStruct(width, MutableList()))
}
