package DFiant
import internals._

protected[DFiant] trait ConditionalBlock

//protected case class Select[RV](cond : Boolean)(thenSel : RV, elseSel : RV)(implicit ctx : Context) {
//  def getValue : RV = ???
//}

object ConditionalBlock {
  type Context = DFDesign.Context
  class IfWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[L, R] <: DFAny.Op.Builder[L, R]](returnVar : DFAny.NewVar) {
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
      override private[DFiant] def nameDefault: String = ctx.getName + "ǂif"
      override def codeString: String = s"\nval $name = ifdf(${cond.refCodeString}) {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      final val returnValue : RV = block
      returnVar.assign(returnValue)(ctx.updateOwner(mutableOwner.value))
      mutableOwner.value = originalOwner
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(cond, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "ǂelseif"
      override private[DFiant] def createAlmanac : Almanac =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
      override def initFunc : Seq[RV#TToken] = DFBool.Token.select(prevIfBlock.cond.getInit, prevIfBlock.initFunc, returnValue.getInit)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "ǂelse"
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
  class IfNoRetVal(mutableOwner: MutableOwner) {
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
      override private[DFiant] def nameDefault: String = ctx.getName + "ǂelseif"
      override private[DFiant] def createAlmanac : Almanac =
        new AlmanacElseIf(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf], cond.almanacEntry)
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf(${cond.refCodeString}) {$bodyCodeString\n}"
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => Unit)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = ctx.getName + "ǂelse"
      override private[DFiant] def createAlmanac : AlmanacElse =
        new AlmanacElse(name, owner.protAlmanac, prevIfBlock.protAlmanac.asInstanceOf[AlmanacIf])
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
    }

    def apply(cond: DFBool)(block: => Unit)(implicit ctx : Context): DFIfBlock =
      new DFIfBlock(cond, block)(ctx, mutableOwner)
  }

//[PatternAble[R] <: DFAny.Pattern.Able[R], PatternBuilder[L <: DFAny] <: DFAny.Pattern.Builder[L, PatternAble]]
  class MatchNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](matchVal : MV)(implicit ctx : Context, mutableOwner: MutableOwner) extends DSLMemberConstruct {
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](None, matchVal, patternBld(matchVal, pattern), block)
      override private[DFiant] def nameDefault: String = ctx.getName
      implicit val owner = ctx.owner
      override def codeString: String = s"\nval $name = matchdf(${matchVal.refCodeString})\n"
      private[DFiant] lazy val nameIt = ctx.n
      val id : Int = getID
      keep
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](prevCase : Option[DFCasePatternBlock[MV]], matchVal : MV, pattern : MV#TPattern, block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](Some(this), matchVal, patternBld(matchVal, pattern), block)
      def casedf_(block : => Unit)(implicit ctx : Context)
      : Unit = new DFCase_Block[MV](Some(this), matchVal, block)

      final lazy val prevAlamanc = if (prevCase.isDefined) Some(prevCase.get.protAlmanac.asInstanceOf[AlmanacCasePattern]) else None
      override private[DFiant] def createAlmanac : Almanac =
        new AlmanacCasePattern(name, owner.protAlmanac, prevAlamanc, matchVal.almanacEntry, pattern)
      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = if (prevCase.isDefined) List(matchVal, prevCase.get) else List(matchVal)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = ctx.getName
      override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      block
      mutableOwner.value = originalOwner
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](prevCase : Option[DFCasePatternBlock[MV]], matchVal : MV, block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](prevCase, matchVal, null.asInstanceOf[MV#TPattern], block) {
      override private[DFiant] def createAlmanac : Almanac = new AlmanacCase_(name, owner.protAlmanac, prevAlamanc, matchVal.almanacEntry)
      override def codeString: String = s".casedf_ {$bodyCodeString\n}"
    }

    def apply[MV <: DFAny](matchValue : MV)(implicit ctx : Context): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.asInstanceOf[MV#TVal])(ctx, mutableOwner)
  }


}



