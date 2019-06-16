/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
import internals._

import scala.collection.mutable.ListBuffer

protected[DFiant] trait ConditionalBlock extends DSLTransparentOwnerConstruct {
  protected[DFiant] trait __DevConditionalBlock extends __DevDSLTransparentOwnerConstruct {

  }
  override private[DFiant] lazy val __dev : __DevConditionalBlock = ???
  import __dev._
  protected[DFiant] type ThisOwner <: DFBlock
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
      protected[DFiant] trait __DevDFIfBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}if"
        override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def preDiscoveryRun() : Unit = {
          returnVar.name //return value should get the name first then internals of the conditional block
          super.preDiscoveryRun()
        }
        private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
        final override protected def discoveryDependencies : List[Discoverable] =
          super.discoveryDependencies ++ ifDiscoveryDepenencies
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      override private[DFiant] lazy val __dev : __DevDFIfBlock = new __DevDFIfBlock {}
      import __dev._

      def elseifdf[R](elseCond : DFBool)(elseBlock : => Able[R])(implicit ctx : Context, op : Builder[R])
      : DFIfBlock = new DFElseIfBlock(this, elseCond.replacement(), op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])

      def elsedf[R](elseBlock: => Able[R])(implicit ctx : Context, op : Builder[R])
      : RV = {
        val dfIfElseBlock = new DFElseBlock(this, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
        returnVar.initialize(firstIf.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }
      private[DFiant] val firstIf : DFIfBlock = this
      private[DFiant] var nextIf : Option[DFIfBlock] = None

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      final val returnValue : RV = block
      returnVar.assign(returnValue)(ctx.updateOwner(mutableOwner.value))
      mutableOwner.value = originalOwner
      if (cond != null) cond.consume()

      protected lazy val initLB : LazyBox[Seq[RV#TToken]] =
        LazyBox.Args3[Seq[RV#TToken], Seq[DFBool.Token], Seq[RV#TToken], Seq[RV#TToken]](this)(
          DFBool.Token.select, cond.initLB, returnValue.initLB, nextIf.get.initLB
        )
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(cond, block) {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}elseif"
        override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      }
      override private[DFiant] lazy val __dev : __DevDFElseIfBlock = new __DevDFElseIfBlock {}
      import __dev._

      override private[DFiant] val firstIf : DFIfBlock = prevIfBlock.firstIf
      prevIfBlock.nextIf = Some(this)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => RV)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(null, block) {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}else"
        override def codeString: String = s".elsedf {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      }
      override private[DFiant] lazy val __dev : __DevDFElseBlock = new __DevDFElseBlock {}
      import __dev._

      override lazy val initLB : LazyBox[Seq[RV#TToken]] = returnValue.initLB
      override private[DFiant] val firstIf : DFIfBlock = prevIfBlock.firstIf
      prevIfBlock.nextIf = Some(this)
    }

    def apply[R](cond: DFBool)(block: => Able[R])(
      implicit ctx : Context, op : Builder[R]
    ) : DFIfBlock = new DFIfBlock(cond.replacement(), op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
  }

  class SelectWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]) {
    def apply[T, E](cond : DFBool)(thenSel : Able[T], elseSel : Able[E])(
      implicit ctx : Context, opT : Builder[T], opE : Builder[E]
    ) : RV = {
      object ifdf extends ConditionalBlock.IfWithRetVal[RV, Able, Builder](returnVar)
      ifdf(cond.replacement()){thenSel}.elsedf(elseSel)
    }
      //new DFIfBlock(cond, op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
  }

  class IfNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] class DFIfBlock(val cond : DFBool, block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      protected[DFiant] trait __DevDFIfBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = ctx.getName
        override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond)
        final override protected def discoveryDependencies : List[Discoverable] =
          super.discoveryDependencies ++ ifDiscoveryDepenencies
      }
      override private[DFiant] lazy val __dev : __DevDFIfBlock = new __DevDFIfBlock {}
      import __dev._

      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : Context)
      : DFIfBlock = new DFElseIfBlock(this, elseCond.replacement(), elseBlock)

      def elsedf(elseBlock: => Unit)(implicit ctx : Context)
      : Unit = new DFElseBlock(this, elseBlock)

      private[DFiant] var _nextIfBlockOption : Option[DFIfBlock] = None
      final lazy val nextIfBlockOption = _nextIfBlockOption
      final def isFinalBlock : Boolean = nextIfBlockOption.isEmpty

      private val originalOwner = mutableOwner.value
      mutableOwner.value = this
      block
      mutableOwner.value = originalOwner
      if (cond != null) cond.consume()
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => Unit)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(cond, block) {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}elseif"
        override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(cond, prevIfBlock)
      }
      override private[DFiant] lazy val __dev : __DevDFElseIfBlock = new __DevDFElseIfBlock {}
      import __dev._

      prevIfBlock._nextIfBlockOption = Some(this)
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => Unit)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(null, block) {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}else"
        override def codeString: String = s".elsedf {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        final override private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] = List(prevIfBlock)
      }
      override private[DFiant] lazy val __dev : __DevDFElseBlock = new __DevDFElseBlock {}
      import __dev._

      prevIfBlock._nextIfBlockOption = Some(this)
    }

    def apply(cond: DFBool)(block: => Unit)(implicit ctx : Context): DFIfBlock =
      new DFIfBlock(cond.replacement(), block)(ctx, mutableOwner)
  }

  class MatchNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](val matchVal : MV, matchConfig : MatchConfig)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends DSLMemberConstruct {
      final private[DFiant] lazy val ctx = ctx0
      protected[DFiant] trait __DevDFMatchHeader extends __DevDSLMemberConstruct {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = ctx.getName
        private def matchConfigCodeString : String =
          if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
        override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def discoveryDependencies : List[Discoverable] =super.discoveryDependencies :+ matchVal

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Ownership
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      override private[DFiant] lazy val __dev : __DevDFMatchHeader = new __DevDFMatchHeader {}
      import __dev._

      protected[DFiant] type TPattern = matchVal.TPattern
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx0 : Context, patternBld : matchVal.TPatternBuilder[MV]
      ) : DFCasePatternBlock[MV] =
        new DFCasePatternBlock[MV](this)(None, patternBld(matchVal, pattern).asInstanceOf[TPattern], block)

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
      private[DFiant] lazy val nameIt = ctx.n
      matchVal.consume()
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : DFAny.Pattern[_], block : => Unit)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}case"
        override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] =
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
          if (prevCase.isDefined && matchHeader.hasOverlappingCases) List(matchHeader, prevCase.get) else List(matchHeader)
        final override protected def discoveryDependencies : List[Discoverable] =super.discoveryDependencies ++ ifDiscoveryDepenencies
      }
      override private[DFiant] lazy val __dev : __DevDFCasePatternBlock = new __DevDFCasePatternBlock {}
      import __dev._

      final val matchVal = matchHeader.matchVal
      def casedf[MC](pattern : matchVal.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx0 : Context, patternBld : matchVal.TPatternBuilder[MV]
      ) : DFCasePatternBlock[MV] =
        new DFCasePatternBlock[MV](matchHeader)(Some(this), patternBld(matchVal, pattern), block)

      def casedf_(block : => Unit)(implicit ctx0 : Context)
      : Unit = new DFCase_Block[MV](matchHeader)(Some(this), block)

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
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}case_"
        override def codeString: String = s".casedf_ {$bodyCodeString\n}"
      }
      override private[DFiant] lazy val __dev : __DevDFCase_Block = new __DevDFCase_Block {}
      import __dev._
    }

    def apply[MV <: DFAny](matchValue : MV, matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
      implicit ctx : Context
    ): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.replacement().asInstanceOf[MV#TVal], matchConfig)(ctx, mutableOwner)
  }

  class MatchWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]){
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](val matchVal : MV, matchConfig : MatchConfig)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends DSLMemberConstruct {
      final private[DFiant] lazy val ctx = ctx0
      protected[DFiant] trait __DevMatchWithRetVal extends __DevDSLMemberConstruct {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}match"
        private def matchConfigCodeString : String =
          if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
        override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def preDiscoveryRun() : Unit = {
          returnVar.name //return value should get the name first then internals of the conditional block
          super.preDiscoveryRun()
        }
        override protected def discoveryDependencies : List[Discoverable] =super.discoveryDependencies :+ matchVal

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Ownership
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      override private[DFiant] lazy val __dev : __DevMatchWithRetVal = new __DevMatchWithRetVal {}
      import __dev._

      protected[DFiant] type TPattern = matchVal.TPattern
      protected[DFiant] type TToken = matchVal.TToken

      def casedf[MC, R](pattern : matchVal.TPatternAble[MC]*)(block : => Able[R])(
        implicit ctx0 : Context, patternBld : matchVal.TPatternBuilder[MV], retVld : Builder[R]
      ) : DFCasePatternBlock[MV] =
        new DFCasePatternBlock[MV](this)(None, patternBld(matchVal, pattern), retVld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])

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
      private[DFiant] lazy val nameIt = ctx.n
      matchVal.consume()
    }

    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : MV#TPattern, block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFDesign with ConditionalBlock {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}case"
        override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        private[DFiant] def ifDiscoveryDepenencies : List[Discoverable] =
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
          if (prevCase.isDefined && matchHeader.hasOverlappingCases) List(matchHeader, prevCase.get) else List(matchHeader)
        final override protected def discoveryDependencies : List[Discoverable] =super.discoveryDependencies ++ ifDiscoveryDepenencies
      }
      override private[DFiant] lazy val __dev : __DevDFCasePatternBlock = new __DevDFCasePatternBlock {}
      import __dev._

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
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override protected def nameDefault: String = s"$ctx${Name.Separator}case_"
        override def codeString: String = s".casedf_ {$bodyCodeString\n}"
      }
      override private[DFiant] lazy val __dev : __DevDFCase_Block = new __DevDFCase_Block {}
      import __dev._

      override lazy val initLB : LazyBox[Seq[RV#TToken]] = returnValue.initLB
    }

    def apply[MV <: DFAny](matchValue : MV, matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
      implicit ctx : Context
    ): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.replacement().asInstanceOf[MV#TVal], matchConfig)(ctx, ctx.owner.mutableOwner)
  }

}



