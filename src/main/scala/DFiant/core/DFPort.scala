package DFiant.core

//Direction of a Port
sealed trait DFDir
object DFDir {
  implicit object OUT extends DFDir
  implicit object IN extends DFDir
  type OUT = OUT.type
  type IN = IN.type
}

