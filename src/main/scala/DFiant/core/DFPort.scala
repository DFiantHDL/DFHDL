package DFiant.core

//Direction of a Port
sealed trait DFDir
object DFDir {

  implicit object OUT extends DFDir
  implicit object IN extends DFDir
}

//to indicate a port is open
object OPEN

trait DFPort[+DF <: DFAny, DIR <: DFDir] {
  val read : DF
  val isOpen : Boolean
}
trait DFPortOut[+DF <: DFAny] extends DFPort[DF, OUT] {
//  def := (that : DF) = ???
}
object DFPortOut {
  implicit def fromDF[DF <: DFAny.Var](dfVar : DF) : DFPortOut[DF] = new DFPortOut[DF] {
    lazy val read: DF = dfVar
    val isOpen : Boolean = false
  }
  implicit def fromOPEN[DF <: DFAny.Var](dfVar : OPEN.type) : DFPortOut[DF] = new DFPortOut[DF] {
    lazy val read: DF = throw new IllegalAccessException("Cannot read from an OPEN port")
    val isOpen : Boolean = true
  }
}

trait DFPortIn[+DF <: DFAny] extends DFPort[DF, IN]
object DFPortIn {
  implicit def toDF[DF <: DFAny](port : DFPortIn[DF]) : DF = port.read
  implicit def fromDF[DF <: DFAny](dfVar : DF) : DFPortIn[DF] = new DFPortIn[DF] {
    lazy val read: DF = dfVar
    val isOpen : Boolean = false
  }
  implicit def fromOPEN[DF <: DFAny](dfVar : OPEN.type) : DFPortIn[DF] = new DFPortIn[DF] {
    lazy val read: DF = throw new IllegalAccessException("Cannot read from an OPEN port")
    val isOpen : Boolean = true
  }
}
