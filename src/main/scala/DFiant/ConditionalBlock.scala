package DFiant
import internals._

import scala.collection.mutable.ListBuffer

protected[DFiant] trait ConditionalBlock extends DSLTransparentOwnerConstruct {
  type ThisOwner <: DFBlock
}

//protected case class Select[RV](cond : Boolean)(thenSel : RV, elseSel : RV)(implicit ctx : Context) {
//  def getValue : RV = ???
//}

sealed trait MatchConfig
object MatchConfig {
  object NoOverlappingCases extends MatchConfig
  object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  type Context = DFAny.Op.Context
  class IfWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]) {
    protected[DFiant] class DFIfBlock(val cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends DFDesign with ConditionalBlock {
      def elseifdf[R](elseCond : DFBool)(elseBlock : => Able[R])(implicit ctx : Context, op : Builder[R])
      : DFIfBlock = new DFElseIfBlock(this, elseCond, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
      def elsedf[R](elseBlock: => Able[R])(implicit ctx : Context, op : Builder[R])
      : RV = {
        val dfIfElseBlock = new DFElseBlock(this, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
        returnVar.initialize(firstIf.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }
      private[DFiant] val firstIf : DFIfBlock = this
      private[DFiant] var nextIf : Option[DFIfBlock] = None

      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}if"
      override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      final val returnValue : RV = block
      returnVar.assign(returnValue)(ctx.updateOwner(mutableOwner.value))
      mutableOwner.value = originalOwner
      if (cond != null) cond.consume()

      protected lazy val initLB : LazyBox[Seq[RV#TToken]] = LazyBox.Args3[Seq[RV#TToken], Seq[DFBool.Token], Seq[RV#TToken], Seq[RV#TToken]](this)(DFBool.Token.select, cond.initLB, returnValue.initLB, nextIf.get.initLB)
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(cond, block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}elseif"
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"
      override private[DFiant] val firstIf : DFIfBlock = prevIfBlock.firstIf
      prevIfBlock.nextIf = Some(this)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => RV)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}else"
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
      override lazy val initLB : LazyBox[Seq[RV#TToken]] = returnValue.initLB
      override private[DFiant] val firstIf : DFIfBlock = prevIfBlock.firstIf
      prevIfBlock.nextIf = Some(this)
    }

    def apply[R](cond: DFBool)(block: => Able[R])(
      implicit ctx : Context, op : Builder[R]
    ) : DFIfBlock = new DFIfBlock(cond, op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
  }

  class SelectWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]) {
    def apply[T, E](cond : DFBool)(thenSel : Able[T], elseSel : Able[E])(
      implicit ctx : Context, opT : Builder[T], opE : Builder[E]
    ) : RV = {
      object ifdf extends ConditionalBlock.IfWithRetVal[RV, Able, Builder](returnVar)
      ifdf(cond){thenSel}.elsedf(elseSel)
    }
      //new DFIfBlock(cond, op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
  }

  class IfNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] class DFIfBlock(val cond : DFBool, block : => Unit)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends DFDesign with ConditionalBlock {
      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : Context)
      : DFIfBlock = new DFElseIfBlock(this, elseCond, elseBlock)
      def elsedf(elseBlock: => Unit)(implicit ctx : Context)
      : Unit = new DFElseBlock(this, elseBlock)

      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = ctx.getName
      override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"
      private[DFiant] var _nextIfBlockOption : Option[DFIfBlock] = None
      final lazy val nextIfBlockOption = _nextIfBlockOption
      final def isFinalBlock : Boolean = nextIfBlockOption.isEmpty

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      block
      mutableOwner.value = originalOwner
      if (cond != null) cond.consume()
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => Unit)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(cond, block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}elseif"
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"
      prevIfBlock._nextIfBlockOption = Some(this)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => Unit)(implicit ctx : Context, mutableOwner : MutableOwner)
      extends DFIfBlock(null, block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}else"
      final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      override def codeString: String = s".elsedf {$bodyCodeString\n}"
      prevIfBlock._nextIfBlockOption = Some(this)
    }

    def apply(cond: DFBool)(block: => Unit)(implicit ctx : Context): DFIfBlock =
      new DFIfBlock(cond, block)(ctx, mutableOwner)
  }

  class MatchNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](val matchVal : MV, matchConfig : MatchConfig)(implicit ctx : Context, mutableOwner: MutableOwner) extends DSLMemberConstruct {
      type TPattern = matchVal.TPattern
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](this)(None, patternBld(matchVal, pattern).asInstanceOf[TPattern], block)
      override private[DFiant] def nameDefault: String = ctx.getName
      private[DFiant] val patternList : ListBuffer[TPattern] = ListBuffer.empty[TPattern]
      private[DFiant] def addCasePattern(pattern : TPattern) : Unit = {
        privHasOverlappingCases =
          if (privHasOverlappingCases) true
          else patternList.foldLeft(false)((ol, p) => ol || p.overlapsWith(pattern))
        if (privHasOverlappingCases && matchConfig == MatchConfig.NoOverlappingCases)
          throw new IllegalArgumentException(s"\ncase pattern $pattern overlaps with previous case patterns.\nEither change the patterns or apply MatchConfig.AllowOverlappingCases to the matchdf's second argument")
        patternList += pattern
      }
      private var privHasOverlappingCases : Boolean = false
      def hasOverlappingCases : Boolean = privHasOverlappingCases
      private def matchConfigCodeString : String =
        if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ matchVal
      lazy val ownerOption : Option[DSLOwnerConstruct] = ctx.ownerOption
      override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"
      private[DFiant] lazy val nameIt = ctx.n
      val id : Int = getID
      matchVal.consume()
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : DFAny.Pattern[_], block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      final val matchVal = matchHeader.matchVal
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](matchHeader)(Some(this), patternBld(matchVal, pattern), block)
      def casedf_(block : => Unit)(implicit ctx : Context)
      : Unit = new DFCase_Block[MV](matchHeader)(Some(this), block)

      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] =
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
        if (prevCase.isDefined && matchHeader.hasOverlappingCases) List(matchHeader, prevCase.get) else List(matchHeader)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}case"
      override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"
      private var nextCase : Option[DFCasePatternBlock[MV]] = None
      final def isLastCase : Boolean = nextCase.isEmpty || nextCase.get.isNotDiscovered
      prevCase.foreach(pc => pc.nextCase = Some(this))

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      block
      mutableOwner.value = originalOwner
      protected val addPatternToHeader : Unit = if (pattern != null) matchHeader.addCasePattern(pattern.asInstanceOf[matchHeader.matchVal.TPattern])
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[DFAny.Pattern[_]], block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}case_"
      override def codeString: String = s".casedf_ {$bodyCodeString\n}"
    }

    def apply[MV <: DFAny](matchValue : MV, matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(implicit ctx : Context): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.asInstanceOf[MV#TVal], matchConfig)(ctx, mutableOwner)
  }

  class MatchWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]){
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](val matchVal : MV, matchConfig : MatchConfig)(implicit ctx : Context, mutableOwner: MutableOwner) extends DSLMemberConstruct {
      type TPattern = matchVal.TPattern
      type TToken = matchVal.TToken
      def casedf[MC, R](pattern : matchVal.TPatternAble[MC]*)(block : => Able[R])(
        implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV], retVld : Builder[R]
      ) : DFCasePatternBlock[MV] =
        new DFCasePatternBlock[MV](this)(None, patternBld(matchVal, pattern), retVld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])

      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}match"
      private[DFiant] val patternList : ListBuffer[TPattern] = ListBuffer.empty[TPattern]
      private[DFiant] def addCasePattern(pattern : TPattern) : Unit = {
        privHasOverlappingCases =
          if (privHasOverlappingCases) true
          else patternList.foldLeft(false)((ol, p) => ol || p.overlapsWith(pattern))
        if (privHasOverlappingCases && matchConfig == MatchConfig.NoOverlappingCases)
          throw new IllegalArgumentException(s"\ncase pattern $pattern overlaps with previous case patterns.\nEither change the patterns or apply MatchConfig.AllowOverlappingCases to the matchdf's second argument")
        patternList += pattern
      }
      private var privHasOverlappingCases : Boolean = false
      def hasOverlappingCases : Boolean = privHasOverlappingCases
      private def matchConfigCodeString : String =
        if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
      override protected def discoveryDepenencies = super.discoveryDepenencies :+ matchVal
      lazy val ownerOption : Option[DSLOwnerConstruct] = ctx.ownerOption
      override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"
      private[DFiant] lazy val nameIt = ctx.n
      val id : Int = getID
      matchVal.consume()
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : MV#TPattern, block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      final val matchVal = matchHeader.matchVal
      def casedf[MC, R](pattern : matchVal.TPatternAble[MC]*)(block : => Able[R])(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV], retBld : Builder[R])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](matchHeader)(Some(this), patternBld(matchVal, pattern), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
      def casedf_[R](block : => Able[R])(implicit ctx : Context, retBld : Builder[R])
      : RV = {
        val dfCase_Block = new DFCase_Block[MV](matchHeader)(Some(this), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
        returnVar.initialize(firstCase.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }
      private var nextCase : Option[DFCasePatternBlock[MV]] = None
      private val firstCase : DFCasePatternBlock[MV] = if (prevCase.isDefined) prevCase.get.firstCase else this
      if (prevCase.isDefined) prevCase.get.nextCase = Some(this)

      private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] =
      //each case is independent unless there are overlapping cases (which must be enabled by the designer)
        if (prevCase.isDefined && matchHeader.hasOverlappingCases) List(matchHeader, prevCase.get) else List(matchHeader)
      final override protected def discoveryDepenencies = super.discoveryDepenencies ++ ifDiscoveryDepenencies
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}case"
      override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      final val returnValue : RV = block
      returnVar.assign(returnValue)(ctx.updateOwner(mutableOwner.value))
      mutableOwner.value = originalOwner
      protected val addPatternToHeader : Unit = if (pattern != null) matchHeader.addCasePattern(pattern.asInstanceOf[matchHeader.matchVal.TPattern])
      private lazy val patternLB : LazyBox[Seq[DFBool.Token]] = LazyBox.Args1C(this)(DFAny.Token.patternMatch[matchVal.TToken, matchVal.TToken#TPattern], matchVal.initLB, pattern.asInstanceOf[matchVal.TToken#TPattern])
      protected lazy val initLB : LazyBox[Seq[RV#TToken]] = LazyBox.Args3[Seq[RV#TToken],Seq[DFBool.Token],Seq[RV#TToken],Seq[RV#TToken]](this)(DFBool.Token.select, patternLB, returnValue.initLB, nextCase.get.initLB)
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[MV#TPattern], block) {
      override private[DFiant] def nameDefault: String = s"$ctx${Name.Separator}case_"
      override def codeString: String = s".casedf_ {$bodyCodeString\n}"
      override lazy val initLB : LazyBox[Seq[RV#TToken]] = returnValue.initLB
    }

    def apply[MV <: DFAny](matchValue : MV, matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(implicit ctx : Context): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.asInstanceOf[MV#TVal], matchConfig)(ctx, ctx.owner.mutableOwner)
  }

}



