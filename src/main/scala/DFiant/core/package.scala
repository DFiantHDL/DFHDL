package DFiant

//import singleton.ops._
//import singleton.twoface._

package object core {
  //////////////////////////////////////////////////////////////////////////////////////////
  // Ports
  //////////////////////////////////////////////////////////////////////////////////////////
  type OUT = DFDir.OUT.type
  type IN = DFDir.IN.type

//  type <>[DF <: DFAny, DIR <: DFDir] = DFPort[DF, DIR]
//  type <>[DF <: DFAny, DIR <: DFDir] = DF with DIR

  implicit class OutPortExtender[DF <: DFAny](port : DFPort[DF, OUT]) {
    def := [DF2 <: DF](that : DF2) : Unit = {}
  }
  //////////////////////////////////////////////////////////////////////////////////////////

  type BitsRange = internals.BitsRange
  val BitsRange = internals.BitsRange

  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
}
