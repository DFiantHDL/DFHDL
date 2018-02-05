package DFiant.core

object ifdf {
  protected[DFiant] def execIf(cond: DFBool)(block: => Unit)(implicit dsn : DFDesign) {
    block
  }
  def apply(cond: DFBool)(block: => Unit)(implicit dsn : DFDesign): IfBool = {
    new IfBool(cond)
  }

}

class ElseIfClause(val cond : DFBool, _block: => Unit)(implicit dsn : DFDesign){
  def unary_! : ElseIfClause = new ElseIfClause(!cond, _block)
  def block = _block
}

class IfBool (prevCond: DFBool)(implicit dsn : DFDesign) {
  def elseifdf (clause : ElseIfClause) : IfBool = privElseifdf(clause.cond)(clause.block)
  private def privElseifdf (cond : DFBool)(block: => Unit) : IfBool = {
    ifdf.execIf(!prevCond && cond){ block }
    new IfBool(prevCond || cond)
  }
  def elsedf (block: => Unit) {
    val cond = !prevCond
    ifdf.execIf(cond){ block }
  }
}




