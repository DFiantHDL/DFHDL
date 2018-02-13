package DFiant

//import singleton.ops._
//import singleton.twoface._

package object core {
  type BitsRange = internals.BitsRange
  val BitsRange = internals.BitsRange

  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
}
