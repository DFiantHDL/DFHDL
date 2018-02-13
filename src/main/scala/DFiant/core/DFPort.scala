package DFiant.core

//Direction of a Port
sealed trait DFDir
sealed trait IN extends DFDir
object IN extends IN
sealed trait OUT extends DFDir
object OUT extends OUT
