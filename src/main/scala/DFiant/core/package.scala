package DFiant

//import singleton.ops._
//import singleton.twoface._

package object core {
  //////////////////////////////////////////////////////////////////////////////////////////
  // Ports
  //////////////////////////////////////////////////////////////////////////////////////////
  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //////////////////////////////////////////////////////////////////////////////////////////

  type BitsRange = internals.BitsRange
  val BitsRange = internals.BitsRange

  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
}
