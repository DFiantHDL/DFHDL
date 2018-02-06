package DFiant.core

//Direction of a Port
sealed trait DFDir
object DFDir {

  implicit object OUT extends DFDir
  implicit object IN extends DFDir
}

//to indicate a port is open
object OPEN

trait DFPort[DF <: DFAny, DIR <: DFDir] {
  val read : DF
  val isOpen : Boolean
}
trait DFPortOut[DF <: DFAny] extends DFPort[DF, OUT] {
  def := (that : DF) = ???
}
object DFPortOut {
  implicit def fromDF[DF <: DFAny.Var[_,_,_,_]](dfVar : DF) : DFPortOut[DF] = new DFPortOut[DF] {
    lazy val read: DF = dfVar
    val isOpen : Boolean = false
  }
  implicit def fromOPEN[DF <: DFAny.Var[_,_,_,_]](dfVar : OPEN.type) : DFPortOut[DF] = new DFPortOut[DF] {
    lazy val read: DF = throw new IllegalAccessException("Cannot read from output port")
    val isOpen : Boolean = true
  }
}
