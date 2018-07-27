package DFiant

import DFiant.internals._
import DFiant.basiclib.DFBasicLib

object ifdf {
  protected[DFiant] def execIf(cond: DFBool)(block: => Unit)(implicit ctx : DFAny.Op.Context) {
    block
  }
  def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFAny.Op.Context): IfBool = {
    new IfBool(cond)
  }

}

class ElseIfClause(val cond : DFBool, _block: => Unit)(implicit ctx : DFAny.Op.Context){
  def unary_! : ElseIfClause = new ElseIfClause(!cond, _block)
  def block = _block
}

class IfBool (prevCond: DFBool)(implicit ctx : DFAny.Op.Context) {
  def elseifdf (clause : DFBool)(block : => Unit) : IfBool = ??? //privElseifdf(clause.cond)(clause.block)
//  def elseifdf (clause : ElseIfClause) : IfBool = privElseifdf(clause.cond)(clause.block)
  private def privElseifdf (cond : DFBool)(block: => Unit) : IfBool = {
    ifdf.execIf(!prevCond && cond){ block }
    new IfBool(prevCond || cond)
  }
  def elsedf (block: => Unit) {
    val cond = !prevCond
    ifdf.execIf(cond){ block }
  }
}


//protected abstract class DFIfBlock(cond : DFBool)(implicit blk : DFBlock, basicLib: DFBasicLib, n : NameIt
//) extends DFBlock()(Some(blk), basicLib, n) {
//}

