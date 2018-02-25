package DFiant.core

trait DFInterface {
  protected type <>[DF <: DFAny, DIR <: DFDir] = DFPort.<>[DF, DIR]
  protected type DFDir = DFPort.DFDir
  protected type IN = DFPort.IN
  protected type OUT = DFPort.OUT
  protected type OPEN = DFPort.OPEN
  protected final val OPEN = DFPort.OPEN
}

protected [DFiant] object DFPort {
  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //Direction of a Port
  sealed trait DFDir
  sealed trait IN extends DFDir
  object IN extends IN
  sealed trait OUT extends DFDir
  object OUT extends OUT

  //to indicate a port is open
  trait OPEN
  object OPEN extends OPEN
}
