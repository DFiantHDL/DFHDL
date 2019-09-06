/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
import internals._

import scala.collection.mutable.ListBuffer

protected[DFiant] abstract class ConditionalBlock[CB <: ConditionalBlock[CB, RV], RV <: Any](returnVar : Option[DFAny.Var])(prevBlock : Option[CB], block : => RV)(
  implicit ctx : ConditionalBlock.Context, mutableOwner: MutableOwner
) extends DFDesign with DSLTransparentOwnerConstruct {self : CB =>
  protected[DFiant] trait __DevConditionalBlock extends __DevDFDesign with __DevDSLTransparentOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Conditional Blocks
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected[ConditionalBlock] var _nextBlock : Option[CB] = None
    final lazy val nextBlock : Option[CB] = _nextBlock
    final lazy val firstBlock : CB = prevBlock match {
      case Some(b) => b.firstBlock
      case None => self
    }
    final lazy val lastBlock : CB = nextBlock match {
      case Some(b) => b.lastBlock
      case None => self
    }
    final lazy val isFirstCondBlock : Boolean = firstBlock == self
    final lazy val isLastCondBlock : Boolean = lastBlock == self
    val isExhaustive : Boolean
  }
  override private[DFiant] lazy val __dev : __DevConditionalBlock = ???
  import __dev._
  protected[DFiant] type ThisOwner <: DFBlock

  prevBlock.foreach{pb =>pb.__dev._nextBlock = Some(self)}

  private val originalOwner = mutableOwner.value
  mutableOwner.value = this
  protected final val returnValue : RV = block
  returnVar.foreach(rv => {
    rv.nameFirst = true
    rv.assign(returnValue.asInstanceOf[DFAny])(ctx.updateOwner(mutableOwner.value))
  })
  mutableOwner.value = originalOwner
  id
}

sealed trait MatchConfig
object MatchConfig {
  object NoOverlappingCases extends MatchConfig
  object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  type Context = DFAny.Op.Context
  implicit def fetchDev(from : ConditionalBlock[_,_])(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
  class IfWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]) {
    protected[DFiant] class DFIfBlock(prevBlock : Option[DFIfBlock], val cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends ConditionalBlock[DFIfBlock, RV](Some(returnVar))(prevBlock, block) {self =>
      protected[DFiant] trait __DevDFIfBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        lazy val isExhaustive : Boolean = nextBlock match {
          case Some(ni) => ni.__dev.isExhaustive
          case None => false //a single if statement cannot be exhaustive unless condition is always true
        }
        final val condVersionedSource = if (cond == null) None else Some(cond.source.versioned)

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}if"
        override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (cond == null) super.discoveryDependenciesStatic
          else super.discoveryDependenciesStatic + cond
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      override private[DFiant] lazy val __dev : __DevDFIfBlock = new __DevDFIfBlock {}
      import __dev._

      def elseifdf[R](elseCond : DFBool)(elseBlock : => Able[R])(implicit ctx : Context, op : Builder[R])
      : DFIfBlock = new DFElseIfBlock(this, elseCond.replacement(), op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])

      def elsedf[R](elseBlock: => Able[R])(implicit ctx : Context, op : Builder[R])
      : RV = {
        val dfIfElseBlock = new DFElseBlock(this, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])

        returnVar.initialize(firstBlock.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }

      protected lazy val initLB : LazyBox[Seq[RV#TToken]] =
        LazyBox.Args3[Seq[RV#TToken], Seq[DFBool.Token], Seq[RV#TToken], Seq[RV#TToken]](this)(
          DFBool.Token.select, cond.initLB, returnValue.initLB, nextBlock.get.initLB
        )
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), cond, block) {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}elseif"
        override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          super.discoveryDependenciesStatic + prevIfBlock
      }
      override private[DFiant] lazy val __dev : __DevDFElseIfBlock = new __DevDFElseIfBlock {}
      import __dev._
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => RV)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), null, block) {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val isExhaustive : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}else"
        override def codeString: String = s".elsedf {$bodyCodeString\n}"
      }
      override private[DFiant] lazy val __dev : __DevDFElseBlock = new __DevDFElseBlock {}
      import __dev._

      override lazy val initLB : LazyBox[Seq[RV#TToken]] = returnValue.initLB
    }

    def apply[R](cond: DFBool)(block: => Able[R])(
      implicit ctx : Context, op : Builder[R]
    ) : DFIfBlock = new DFIfBlock(None, cond.replacement(), op(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])(ctx, ctx.owner.mutableOwner)
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
    protected[DFiant] class DFIfBlock(prevBlock : Option[DFIfBlock], val cond : DFBool, block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends ConditionalBlock[DFIfBlock, Unit](None)(prevBlock, block) {
      protected[DFiant] trait __DevDFIfBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        lazy val isExhaustive : Boolean = nextBlock match {
          case Some(ni) => ni.__dev.isExhaustive
          case None => false //a single if statement cannot be exhaustive unless condition is always true
        }
        final val condVersionedSource = if (cond == null) None else Some(cond.source.versioned)
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (cond == null) super.discoveryDependenciesStatic
          else super.discoveryDependenciesStatic + cond
      }
      override private[DFiant] lazy val __dev : __DevDFIfBlock = new __DevDFIfBlock {}
      import __dev._

      def elseifdf(elseCond : DFBool)(elseBlock : => Unit)(implicit ctx : Context)
      : DFIfBlock = new DFElseIfBlock(this, elseCond.replacement(), elseBlock)

      def elsedf(elseBlock: => Unit)(implicit ctx : Context)
      : Unit = new DFElseBlock(this, elseBlock)

      if (cond != null) cond.consume()
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => Unit)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), cond, block) {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}elseif"
        override def codeString: String = s".elseifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline final override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          super.discoveryDependenciesStatic + prevIfBlock
      }
      override private[DFiant] lazy val __dev : __DevDFElseIfBlock = new __DevDFElseIfBlock {}
      import __dev._
    }

    protected[DFiant] class DFElseBlock(prevIfBlock : DFIfBlock, block : => Unit)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), null, block) {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val isExhaustive : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}else"
        override def codeString: String = s".elsedf {$bodyCodeString\n}"
      }
      override private[DFiant] lazy val __dev : __DevDFElseBlock = new __DevDFElseBlock {}
      import __dev._
    }

    def apply(cond: DFBool)(block: => Unit)(implicit ctx : Context): DFIfBlock =
      new DFIfBlock(None, cond.replacement(), block)(ctx, mutableOwner)
  }

  class MatchNoRetVal(mutableOwner: MutableOwner) {
    protected[DFiant] final class DFMatchHeader[MV <: DFAny](val matchVal : MV, matchConfig : MatchConfig)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends DFAnyMember {
      final private[DFiant] override lazy val ctx = ctx0
      protected[DFiant] trait __DevDFMatchHeader extends __DevDFAnyMember {
        final val matchValVersionedSource = matchVal.source.versioned
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        private def matchConfigCodeString : String =
          if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
        override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          super.discoveryDependenciesStatic + matchVal

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
      private[DFiant] lazy val nameIt = ctx.meta
      matchVal.consume()
      id
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : DFAny.Pattern[_], block : => Unit)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends ConditionalBlock[DFCasePatternBlock[MV], Unit](None)(prevCase, block) {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        lazy val hasCase_ : Boolean = nextBlock match {
          case Some(nc) => nc.__dev.hasCase_
          case None => false
        }
        lazy val isExhaustive: Boolean = hasCase_ // || TODO: check that the pattern check is exhaustive
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}case"
        override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (prevCase.isDefined && matchHeader.hasOverlappingCases)
            super.discoveryDependenciesStatic +  matchHeader + prevCase.get
          else super.discoveryDependenciesStatic + matchHeader
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

      def enddf : Unit = {}

      protected val addPatternToHeader : Unit = if (pattern != null) matchHeader.addCasePattern(pattern.asInstanceOf[matchHeader.matchVal.TPattern])
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], block : => Unit)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[DFAny.Pattern[_]], block) {
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val hasCase_ : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}case_"
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
    ) extends DFAnyMember {
      final private[DFiant] override lazy val ctx = ctx0
      protected[DFiant] trait __DevMatchWithRetVal extends __DevDFAnyMember {
        final val matchValVersionedSource = matchVal.source.versioned
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}match"
        private def matchConfigCodeString : String =
          if (hasOverlappingCases) ", MatchConfig.AllowOverlappingCases" else ""
        override def codeString: String = s"\nmatchdf(${matchVal.refCodeString(owner)}$matchConfigCodeString)\n"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          super.discoveryDependenciesStatic + matchVal

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
      private[DFiant] lazy val nameIt = ctx.meta
      matchVal.consume()
      returnVar.nameFirst = true
      id
    }

    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : MV#TPattern, block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends ConditionalBlock[DFCasePatternBlock[MV], RV](Some(returnVar))(prevCase, block) {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevDFDesign with __DevConditionalBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        lazy val hasCase_ : Boolean = nextBlock match {
          case Some(nc) => nc.__dev.hasCase_
          case None => false
        }
        lazy val isExhaustive: Boolean = hasCase_ // || TODO: check that the pattern check is exhaustive

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}case"
        override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (prevCase.isDefined && matchHeader.hasOverlappingCases)
            super.discoveryDependenciesStatic +  matchHeader + prevCase.get
          else super.discoveryDependenciesStatic + matchHeader
      }
      override private[DFiant] lazy val __dev : __DevDFCasePatternBlock = new __DevDFCasePatternBlock {}
      import __dev._

      final val matchVal = matchHeader.matchVal
      def casedf[MC, R](pattern : matchVal.TPatternAble[MC]*)(block : => Able[R])(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV], retBld : Builder[R])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](matchHeader)(Some(this), patternBld(matchVal, pattern), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
      def casedf_[R](block : => Able[R])(implicit ctx : Context, retBld : Builder[R])
      : RV = {
        val dfCase_Block = new DFCase_Block[MV](matchHeader)(Some(this), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
        returnVar.initialize(firstBlock.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }
      def enddf(implicit ctx : Context) : RV = {
        returnVar.initialize(firstBlock.initLB.asInstanceOf[LazyBox[Seq[returnVar.TToken]]], ctx.owner)
        returnVar.asInstanceOf[RV]
      }

      protected val addPatternToHeader : Unit = if (pattern != null) matchHeader.addCasePattern(pattern.asInstanceOf[matchHeader.matchVal.TPattern])
      private lazy val patternLB : LazyBox[Seq[DFBool.Token]] = LazyBox.Args1C(this)(DFAny.Token.patternMatch[matchVal.TToken, matchVal.TToken#TPattern], matchVal.initLB, pattern.asInstanceOf[matchVal.TToken#TPattern])
      protected lazy val initLB : LazyBox[Seq[RV#TToken]] = LazyBox.Args3[Seq[RV#TToken],Seq[DFBool.Token],Seq[RV#TToken],Seq[RV#TToken]](this)(DFBool.Token.select, patternLB, returnValue.initLB, nextBlock.get.initLB)
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[MV#TPattern], block) {
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val hasCase_ : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override val nameScala: String = s"$ctx${Name.Separator}case_"
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



