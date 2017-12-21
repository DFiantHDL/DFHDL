package DFiant.internals


trait AlmanacGuard {

}



abstract class AlmanacEntry
(implicit
 val id : AlmanacID,
 val address : AlmanacAddress,
 val bitsRange : BitsRange,
 val timeRef : AlmanacTimeRef = AlmanacTimeRef.Current,
 val init : AlmanacInit = AlmanacInit.Bubble
) {
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
class AlmanacEntryConst private (val constVal : BigInt)
                       (implicit id : AlmanacID = AlmanacIDConst(constVal),
                        address : AlmanacAddress = AlmanacAddressLatest,
                        bitsRange : BitsRange = BitsRange(bigIntRepWidth(constVal)-1,0))
  extends AlmanacEntry() {
}

object AlmanacEntryConst {
  def apply(constVal : BigInt) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryConst(constVal))
}

class AlmanacEntryCreateDFVar private (val width : Int)
                             (implicit id : AlmanacID = AlmanacID(),
                              address : AlmanacAddress = AlmanacAddressLatest,
                              bitsRange : BitsRange = BitsRange(width-1, 0))
  extends AlmanacEntry() {

}
object AlmanacEntryCreateDFVar {
  def apply(width : Int) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryCreateDFVar(width))
}



class AlmanacEntryAliasDFVar private (val aliasedEntry : AlmanacEntry, val relBitsRange: BitsRange)
                            (implicit id : AlmanacID = aliasedEntry.id,
                             address : AlmanacAddress = aliasedEntry.address,
                             bitsRange : BitsRange = aliasedEntry.bitsRange.subRangeRel(relBitsRange))
  extends AlmanacEntry() {

}
object AlmanacEntryAliasDFVar {
  def apply(aliasedEntry : AlmanacEntry, relBitsRange: BitsRange, deltaStep : Int, init : AlmanacInit) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryAliasDFVar(aliasedEntry, relBitsRange))
}



class AlmanacEntryGetDFVar private (val varEntry : AlmanacEntry)
                            (implicit id : AlmanacID = varEntry.id,
                             address : AlmanacAddress = Almanac.getCurrentAddress,
                             bitsRange : BitsRange = varEntry.bitsRange)
  extends AlmanacEntry() {

}

object AlmanacEntryGetDFVar {
  def apply(varEntry : AlmanacEntry) : AlmanacEntry = Almanac.fetchEntry(new AlmanacEntryGetDFVar(varEntry))
}



import scala.collection.mutable.MutableList
class AlmanacEntryStruct private (val width : Int, val structEntryList : MutableList[AlmanacEntry])
                                  (implicit id : AlmanacID = AlmanacID(),
                                   address : AlmanacAddress = AlmanacAddressLatest,
                                   bitsRange : BitsRange = BitsRange(width-1, 0))
  extends AlmanacEntry() {

}

object AlmanacEntryStruct {
  def apply(width : Int) : AlmanacEntryStruct = Almanac.fetchEntry(new AlmanacEntryStruct(width, MutableList()))
}
