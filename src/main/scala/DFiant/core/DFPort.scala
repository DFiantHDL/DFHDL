package DFiant.core

//Direction of a Port
sealed trait DFDir
object DFDir {
  implicit object OUT extends DFDir
  implicit object IN extends DFDir
  type OUT = OUT.type
  type IN = IN.type
}

trait DFPort[+DF <: DFAny, DIR <: DFDir] {
  val read : DF
  val isOpen : Boolean
}
object DFPort {
//  implicit def toDF[DF <: DFAny](port : DFPort[DF, IN]) : DF = port.read
//  implicit def fromOPEN[DF <: DFAny, DIR <: DFDir](dfVar : OPEN) : DFPort[DF, DIR] = new DFPort[DF, DIR] {
//    lazy val read: DF = throw new IllegalAccessException("Cannot read from an OPEN port")
//    val isOpen : Boolean = true
//  }
//  implicit def fromDFIn[DF <: DFAny](dfVar : DF) : DFPort[DF, IN] = new DFPort[DF, IN] {
//    lazy val read: DF = dfVar
//    val isOpen : Boolean = false
//  }
//  implicit def fromDFOut[DF <: DFAny.Var](dfVar : DF) : DFPort[DF, OUT] = new DFPort[DF, OUT] {
//    lazy val read: DF = dfVar
//    val isOpen : Boolean = false
//  }
}
