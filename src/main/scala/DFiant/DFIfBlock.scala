package DFiant
import internals._

protected[DFiant] trait ConditionalBlock

//protected case class Select[RV](cond : Boolean)(thenSel : RV, elseSel : RV)(implicit ctx : Context) {
//  def getValue : RV = ???
//}

object ConditionalBlock {
  type Context = DFDesign.Context
  class WithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[L, R] <: DFAny.Op.Builder[L, R]](returnVar : DFAny.NewVar) {
    protected[DFiant] class DFIfBlock(val cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends DFDesign with ConditionalBlock {
      def elseifdf[R](elseCond : DFBool)(elseBlock : => Able[R])(implicit ctx : Context, op : Builder[RV, R])
      : DFIfBlock = new DFElseIfBlock(this, elseCond, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
      def elsedf[R](elseBlock: => Able[R])(implicit ctx : Context, op : Builder[RV, R])
      : RV = {
        val dfIfElseBlock = new DFElseBlock(this, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
        returnVar.initialize(dfIfElseBlock.initFunc.asInstanceOf[Seq[returnVar.TToken]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }
      def initFunc : Seq[RV#TToken] = returnValue.getInit

      override private[DFiant] def createAlmanac : Almanac = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = ctx.getName + "$if"
      override def codeString: String = s"\nval $name = ifdf(${cond.refCodeString}) {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      final val returnValue : RV = block
      returnVar.assign(returnValue)(ctx.updateOwner(mutableOwner.value))
      mutableOwner.value = originalOwner
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(cond, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "$elseif"
      override private[DFiant] def createAlmanac : Almanac =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
      override def initFunc : Seq[RV#TToken] = DFBool.Token.select(prevIfBlock.cond.getInit, prevIfBlock.initFunc, returnValue.getInit)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "$else"
      override private[DFiant] def createAlmanac : AlmanacElse =
        new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
      override def initFunc : Seq[RV#TToken] = DFBool.Token.select(prevIfBlock.cond.getInit, prevIfBlock.initFunc, returnValue.getInit)
    }

    def apply[R](cond: DFBool)(block: => Able[R])(
      implicit ctx : Context, op : Builder[RV, R]
    ) : DFIfBlock = new DFIfBlock(cond, op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
  }
  object NoRetVal {
    protected[DFiant] class DFIfBlock(val cond : DFBool, block : => Unit)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends DFDesign with ConditionalBlock {
      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : Context)
      : DFIfBlock = new DFElseIfBlock(this, elseCond, elseBlock)
      def elsedf(elseBlock: => Unit)(implicit ctx : Context)
      : Unit = new DFElseBlock(this, elseBlock)

      override private[DFiant] def createAlmanac : Almanac = new AlmanacIf(name, owner.protAlmanac, cond.almanacEntry)
      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = ctx.getName
      override def codeString: String = s"\nval $name = ifdf(${cond.refCodeString}) {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      block
      mutableOwner.value = originalOwner
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => Unit)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(cond, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "$elseif"
      override private[DFiant] def createAlmanac : Almanac =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => Unit)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "$else"
      override private[DFiant] def createAlmanac : AlmanacElse =
        new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
    }
  }

}



