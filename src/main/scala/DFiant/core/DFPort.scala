package DFiant.core

//Direction of a Port
sealed trait DFDir
object DFDir {

  implicit object OUT extends DFDir
  implicit object IN extends DFDir
}

trait DFPort[DF <: DFAny, DIR <: DFDir]

//trait DFPortIn[DF <: DF]


//object dflkflk {
//  class Maybe(in : DFBits.Unsafe <> IN, out : DFBits.Unsafe <> OUT)
//  new DFPort[DFBits.Unsafe, OUT]{}
//}
