//I have a code structure similar to the following. Is this legal?
//```scala
//class Enum {
//  type Entry <: EnumEntry
//}
//object GoodEnum extends Enum {
//  sealed trait Entry
//  case object Nice extends Entry
//  case object Tasty extends Entry
//  case object Fun extends Entry
//}
//object BadEnum extends Enum {
//  sealed trait Entry
//  case object Rude extends Entry
//  case object Yucky extends Entry
//  case object Boring extends Entry
//}
//
//trait AnyBox {
//  type T <: AnyBox
//  def ===(that : T) : Boolean
//}
//trait EnumBox[E <: Enum] extends AnyToken {
//}
//object EnumBox {
//  trait Unbounded {
//    type T =  EnumToken[E]
//    def ===(that : T) : Boolean = ???
//    def == (that : E#Entry) : Boolean = ???
//
//  }
//}
//```