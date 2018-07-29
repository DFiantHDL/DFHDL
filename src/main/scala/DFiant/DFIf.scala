package DFiant

import DFiant.internals._
import DFiant.basiclib.DFBasicLib

object ifdf {
  def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFIfBlock.Context): DFIfBlock = {
    new DFIfBlock(cond, block)
  }
}

protected class DFIfBlock(cond : DFBool, block: => Unit)(implicit ctx : DFIfBlock.Context)
  extends DFBlock {
  def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : DFIfBlock.Context) 
  : DFIfBlock = new DFElseIfBlock(elseCond, elseBlock)
  def elsedf(block: => Unit)(implicit ctx : DFIfBlock.Context)
  : Unit = new DFElseBlock(block)

  override protected def newAlmanac : AlmanacIf = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)

  block
}

protected class DFElseIfBlock(cond : DFBool, block: => Unit)(implicit ctx : DFIfBlock.Context)
  extends DFIfBlock(cond, block) {
}

protected class DFElseBlock(block: => Unit)(implicit ctx : DFIfBlock.Context) extends
  DFIfBlock(DFBool.const(DFBool.Token(true)), block) {
}

object DFIfBlock {
  type Context = DFAnyOwner.ContextWithLib
}
