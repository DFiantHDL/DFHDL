//package DFiant.internals
//
////Time reference of the address
////Negative step - Prev(Past time)
////Zero (0) step - Current(Present time)
////Positive step - Next(Future time)
//class AlmanacTimeRef private (val step : Int) {
//  override def toString: String = if(step < 0) s"P$step" else if (step == 0) "C" else s"N$step"
//  def stepBy(deltaStep : Int) = if (deltaStep == 0) this else AlmanacTimeRef(step + deltaStep)
//}
//
//object AlmanacTimeRef {
//  case object Current extends AlmanacTimeRef(0)
//
//  def apply(step : Int) = new AlmanacTimeRef(step)
//}
//
//
