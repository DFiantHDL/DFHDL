package DFiant

import DFiant.internals._
import DFiant.basiclib.DFBasicLib

//object ifdf {
//  def apply(cond: DFBool)(block: => Unit)(implicit ctx : DFIfBlock.Context): DFIfBlock = {
//    new DFIfBlock(cond, block)
//  }
//}

protected class DFIfBlock(cond : DFBool, block: => Unit)(implicit ctx : DFIfBlock.Context)
  extends DFBlock {
  def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : DFIfBlock.Context)
  : DFIfBlock = new DFElseIfBlock(this, elseCond, elseBlock)
  def elsedf(block: => Unit)(implicit ctx : DFIfBlock.Context)
  : Unit = new DFElseBlock(this, block)

  override protected def createAlmanac : AlmanacIf = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ cond
  override def codeString: String =
    s"val $name = ifdf(${cond.name}) {\n$bodyCodeString\n}"
}

protected class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block: => Unit)(implicit ctx : DFIfBlock.Context)
  extends DFIfBlock(cond, block) {
  override protected def createAlmanac : AlmanacElseIf =
    new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
  override def codeString: String =
    s".elseifdf(${cond.name}) {\n$bodyCodeString\n}"
}

protected class DFElseBlock(prevIfBlock : DFIfBlock, block: => Unit)(implicit ctx : DFIfBlock.Context)
  extends DFBlock {
  override protected def createAlmanac : AlmanacElse =
    new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
  override protected def discoveryDepenencies = super.discoveryDepenencies :+ prevIfBlock
  override def codeString: String =
    s".elsedf() {\n$bodyCodeString\n}"

  block
}

object DFIfBlock {
  type Context = DFBlock.Context
}
