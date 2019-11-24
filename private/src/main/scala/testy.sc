trait DFAny {
  val width : Int
}


case class DFUInt(width : Int) extends DFAny

sealed trait DFDir
case object IN extends DFDir
case object OUT extends DFDir

case class DFPort[DF <: DFAny, DIR <: DFDir](dir : DF)



