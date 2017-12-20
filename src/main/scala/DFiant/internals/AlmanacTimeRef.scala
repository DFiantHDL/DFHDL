package DFiant.internals

sealed trait AlmanacTimeRef
case class AlmanacTimeRefCurrent() extends AlmanacTimeRef {
  override def toString: String = "C"
}
case class AlmanacTimeRefPast(idx : Int) extends AlmanacTimeRef {
  override def toString: String = "P" + idx
}
case class AlmanacTimeRefFuture(idx : Int) extends AlmanacTimeRef {
  override def toString: String = "F" + idx
}


//Time reference of the address
//Negative value - Past
//Zero (0) value - Current/Present time
//Positive value - Future
//val timeRef : AlmanacTimeRef
