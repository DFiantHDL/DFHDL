package DFiant.core

object ifdf {
  protected[DFiant] def execIf(cond: DFBool)(block: => Unit)(implicit dsn : DFDesign) {
    block
  }
  def apply(cond: DFBool)(block: => Unit)(implicit dsn : DFDesign): IfBool = {
    new IfBool(cond)
  }

}

class ElseIfClause(val cond : DFBool, _block: => Unit){
  def unary_!(implicit dsn : DFDesign) : ElseIfClause = new ElseIfClause(!cond, _block)
  def block(implicit dsn : DFDesign) = _block
}

class IfBool (prevCond: DFBool) {
  def elseifdf (clause : ElseIfClause)(implicit dsn : DFDesign) : IfBool = privElseifdf(clause.cond)(clause.block)
  private def privElseifdf (cond : DFBool)(block: => Unit)(implicit dsn : DFDesign) : IfBool = {
    ifdf.execIf(!prevCond && cond){ block }
    new IfBool(prevCond || cond)
  }
  def elsedf (block: => Unit)(implicit dsn : DFDesign) {
    val cond = !prevCond
    ifdf.execIf(cond){ block }
  }
}



