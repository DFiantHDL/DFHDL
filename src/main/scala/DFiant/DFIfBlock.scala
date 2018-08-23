package DFiant
import internals._

protected[DFiant] trait ConditionalBlock

protected case class Select[RV](cond : Boolean)(thenSel : RV, elseSel : RV)(implicit ctx : DFIfBlock.Context) {
  def getValue : RV = ???
}

protected class DFIfBlock[RV](cond : DFBool, block : => RV, returnVar : Option[RV])(implicit ctx : DFIfBlock.Context, mutableOwner: MutableOwner)
  extends DFDesign with ConditionalBlock {
  def elseifdf(elseCond : DFBool)(elseBlock : => RV)(implicit ctx : DFIfBlock.Context)
  : DFIfBlock[RV] = new DFElseIfBlock[RV](this, elseCond, elseBlock, returnVar)
  def elsedf(elseBlock: => RV)(implicit ctx : DFIfBlock.Context)
  : RV = {
    new DFElseBlock[RV](this, elseBlock, returnVar)
    returnVar match {
      case Some(v) => v
      case _ => {}.asInstanceOf[RV]
    }
  }

  override private[DFiant] def createAlmanac : Almanac = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
  private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
  final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
  override private[DFiant] def nameDefault: String = if (returnVar.isDefined) ctx.getName + "$if" else ctx.getName
  override def codeString: String = s"\nval $name = ifdf(${cond.refCodeString}) {$bodyCodeString\n}"

  private val originalOwner = mutableOwner.value
  mutableOwner.value = this
  final val returnValue : RV = block
  returnVar match {
    case Some(v : DFAny.Var) => v.assign(returnValue.asInstanceOf[DFAny])(ctx.updateOwner(mutableOwner.value))
    case _ =>
  }
  mutableOwner.value = originalOwner
}

protected class DFElseIfBlock[RV](prevIfBlock : DFIfBlock[RV], cond : DFBool, block : => RV, returnVar : Option[RV])(implicit ctx : DFIfBlock.Context, mutableOwner : MutableOwner)
  extends DFIfBlock[RV](cond, block, returnVar) {
  override private[DFiant] def nameDefault: String = ctx.getName + "$elseif"
  override private[DFiant] def createAlmanac : Almanac =
    new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
  final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
  override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
}

protected class DFElseBlock[RV](prevIfBlock : DFIfBlock[RV], block : => RV, returnVar : Option[RV])(implicit ctx : DFIfBlock.Context, mutableOwner : MutableOwner)
  extends DFIfBlock[RV](null, block, returnVar) {
  override private[DFiant] def nameDefault: String = ctx.getName + "$else"
  override private[DFiant] def createAlmanac : AlmanacElse =
    new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
  final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
  override def codeString: String = s".elsedf {$bodyCodeString\n}"
}

object DFIfBlock {
  type Context = DFDesign.Context
}



