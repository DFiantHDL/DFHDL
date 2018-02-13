package DFiant

//import singleton.ops._
//import singleton.twoface._

package object core {
  //////////////////////////////////////////////////////////////////////////////////////////
  // Ports
  //////////////////////////////////////////////////////////////////////////////////////////
//  type OUT = DFDir.OUT
//  type IN = DFDir.IN

  //to indicate a port is open
  trait OPEN
  object OPEN extends OPEN

  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //////////////////////////////////////////////////////////////////////////////////////////

  type BitsRange = internals.BitsRange
  val BitsRange = internals.BitsRange

  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
}
