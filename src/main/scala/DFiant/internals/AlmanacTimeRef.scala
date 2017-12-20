package DFiant.internals

//Time reference of the address
//Negative step - Prev(Past time)
//Zero (0) step - Current(Present time)
//Positive step - Next(Future time)
class AlmanacTimeRef(val step : Int) {
  override def toString: String = if(step < 0) s"P$step" else if (step == 0) "C" else s"N$step"
  def stepBy(deltaStep : Int) = new AlmanacTimeRef(step + deltaStep)
}
case object AlmanacTimeRefCurrent extends AlmanacTimeRef(0)

