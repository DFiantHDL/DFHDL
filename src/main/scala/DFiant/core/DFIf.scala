package DFiant.core

object ifdf {
  protected[DFiant] def execIf(cond: DFBool)(block: => Unit) {
    block
  }
  def apply(cond: DFBool)(block: => Unit): IfBool = {
    new IfBool(cond)
  }
}

class IfBool (prevCond: DFBool) {
  def elseifdf (cond: DFBool)(block: => Unit): IfBool = {
    ifdf.execIf(!prevCond && cond){ block }
    new IfBool(prevCond || cond)
  }
  def elsedf (block: => Unit) {
    val cond = !prevCond
    ifdf.execIf(cond){ block }
  }
}
