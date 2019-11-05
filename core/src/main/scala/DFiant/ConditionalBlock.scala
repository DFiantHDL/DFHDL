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
) extends DFBlock with DSLTransparentOwnerConstruct {self : CB =>
  protected[DFiant] trait __DevConditionalBlock extends __DevDFBlock with __DevDSLTransparentOwnerConstruct {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Conditional Blocks
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected[ConditionalBlock] var _nextBlock : Option[CB] = None
    final lazy val nextBlock : Option[CB] = _nextBlock
    final val firstBlock : CB = prevBlock match {
      case Some(b) => b.firstBlock
      case None => self
    }
    final lazy val lastBlock : CB = nextBlock match {
      case Some(b) => b.lastBlock
      case None => self
    }
    final protected val prevBlocks : List[CB] = prevBlock match {
      case Some(b) => b.prevBlocks :+ self
      case None => List(self)
    }
    final lazy val allBlocks : List[CB] = lastBlock.prevBlocks
    final val isFirstCondBlock : Boolean = firstBlock == self
    final lazy val isLastCondBlock : Boolean = lastBlock == self
    val isExhaustive : Boolean

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    lazy val discoveredSet : CacheBoxRO[Set[DFAnyMember]] = owner.discoveredSet
  }
  override private[DFiant] lazy val __dev : __DevConditionalBlock = ???
  import __dev._

  prevBlock.foreach{pb =>pb.__dev._nextBlock = Some(self)}

  private val originalOwner = mutableOwner.value
  mutableOwner.value = this
  protected final val returnValue : RV = block
  returnVar.foreach(rv => {
    rv.nameFirst = true
    rv.assign(returnValue.asInstanceOf[DFAny])(ctx.updateOwner(this))
  })
  mutableOwner.value = originalOwner
  id
}

protected[DFiant] abstract class ConditionalRetBlock[CB <: ConditionalRetBlock[CB, RV], RV <: DFAny](returnVar : DFAny.NewVar[RV])(prevBlock : Option[CB], block : => RV)(
  implicit ctx : ConditionalBlock.Context, mutableOwner: MutableOwner
) extends ConditionalBlock[CB, RV](Some(returnVar))(prevBlock, block) {self : CB =>
  protected[DFiant] trait __DevConditionalRetBlock extends __DevConditionalBlock {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Initialization
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    val initCB : CacheBoxRO[Seq[RV#TToken]]
  }
  override private[DFiant] lazy val __dev : __DevConditionalRetBlock = ???
  import __dev._
  if (isFirstCondBlock) returnVar.conditionalBlockDriver.set(Some(self))
}


sealed trait MatchConfig
object MatchConfig {
  object NoOverlappingCases extends MatchConfig
  object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  type Context = DFAny.Op.Context
  trait IfBlock {
    val cond : DFBool
  }
  trait ElseIfBlock extends IfBlock
  trait ElseBlock extends IfBlock
  trait MatchHeader[MV <: DFAny] {
    val matchVal : MV
  }
  trait CasePatternBlock {
    val pattern : DFAny.Pattern[_]
  }
  trait Case_Block extends CasePatternBlock
  trait WithRetVal
  trait NoRetVal
  implicit def fetchDev(from : ConditionalBlock[_,_])(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  class IfWithRetVal[RV <: DFAny, Able[R] <: DFAny.Op.Able[R], Builder[R] <: DFAny.Op.Builder[RV, R]](returnVar : DFAny.NewVar[RV]) {
    protected[DFiant] class DFIfBlock(prevBlock : Option[DFIfBlock], val cond : DFBool, block : => RV)(implicit ctx : Context, mutableOwner: MutableOwner)
      extends ConditionalRetBlock[DFIfBlock, RV](returnVar)(prevBlock, block) with IfBlock with WithRetVal {self =>
      protected[DFiant] trait __DevDFIfBlock extends __DevConditionalRetBlock {
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
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}if"
        override def codeString: String = s"\nifdf${cond.refCodeString.applyBrackets(false)} {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (cond == null) super.discoveryDependenciesStatic
          else super.discoveryDependenciesStatic + cond

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Initialization
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        lazy val initCB : CacheBoxRO[Seq[RV#TToken]] = CacheDerivedRO(cond.initCB, returnValue.initCB, nextBlock.get.__dev.initCB) {
          DFBool.Token.select(cond.initCB, returnValue.initCB, nextBlock.get.__dev.initCB)
        }
      }
      override private[DFiant] lazy val __dev : __DevDFIfBlock = new __DevDFIfBlock {}
      import __dev._

      def elseifdf[R](elseCond : DFBool)(elseBlock : => Able[R])(implicit ctx : Context, op : Builder[R])
      : DFIfBlock = new DFElseIfBlock(this, elseCond.replacement(), op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])

      def elsedf[R](elseBlock: => Able[R])(implicit ctx : Context, op : Builder[R])
      : RV = {
        val dfIfElseBlock = new DFElseBlock(this, op(returnVar.asInstanceOf[RV], elseBlock).asInstanceOf[RV])
        returnVar.asInstanceOf[RV]
      }

    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => RV)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), cond, block) with ElseIfBlock {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}elseif"
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
    ) extends DFIfBlock(Some(prevIfBlock), null, block) with ElseBlock {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val isExhaustive : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}else"
        override def codeString: String = s".elsedf {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Initialization
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val initCB : CacheBoxRO[Seq[RV#TToken]] = returnValue.initCB
      }
      override private[DFiant] lazy val __dev : __DevDFElseBlock = new __DevDFElseBlock {}
      import __dev._
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
    ) extends ConditionalBlock[DFIfBlock, Unit](None)(prevBlock, block) with IfBlock with NoRetVal {
      protected[DFiant] trait __DevDFIfBlock extends __DevConditionalBlock {
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
    }

    protected[DFiant] class DFElseIfBlock(prevIfBlock : DFIfBlock, cond : DFBool, block : => Unit)(
      implicit ctx : Context, mutableOwner : MutableOwner
    ) extends DFIfBlock(Some(prevIfBlock), cond, block) with ElseIfBlock {
      protected[DFiant] trait __DevDFElseIfBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}elseif"
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
    ) extends DFIfBlock(Some(prevIfBlock), null, block) with ElseBlock {
      protected[DFiant] trait __DevDFElseBlock extends __DevDFIfBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val isExhaustive : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}else"
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
    ) extends DFAnyMember with MatchHeader[MV] with NoRetVal {
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
      id
    }
    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : DFAny.Pattern[_], block : => Unit)(
      implicit ctx0 : Context, mutableOwner: MutableOwner
    ) extends ConditionalBlock[DFCasePatternBlock[MV], Unit](None)(prevCase, block) with CasePatternBlock with NoRetVal {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevConditionalBlock {
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
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}case"
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
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[DFAny.Pattern[_]], block) with Case_Block {
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val hasCase_ : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}case_"
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
    ) extends DFAnyMember with MatchHeader[MV] with WithRetVal {
      final private[DFiant] override lazy val ctx = ctx0
      protected[DFiant] trait __DevMatchWithRetVal extends __DevDFAnyMember {
        final val matchValVersionedSource = matchVal.source.versioned
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}match"
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
      id
    }

    protected[DFiant] class DFCasePatternBlock[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], val pattern : MV#TPattern, block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends ConditionalRetBlock[DFCasePatternBlock[MV], RV](returnVar)(prevCase, block) with CasePatternBlock with WithRetVal {
      protected[DFiant] trait __DevDFCasePatternBlock extends __DevConditionalRetBlock {
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
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}case"
        override def codeString: String = s".casedf(${pattern.codeString}) {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Member discovery
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        //each case is independent unless there are overlapping cases (which must be enabled by the designer)
        @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
          if (prevCase.isDefined && matchHeader.hasOverlappingCases)
            super.discoveryDependenciesStatic +  matchHeader + prevCase.get
          else super.discoveryDependenciesStatic + matchHeader

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Initialization
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        private lazy val patternLB = CacheDerivedRO(matchVal.initCB)(DFAny.Token.patternMatch(matchVal.initCB, pattern))
        lazy val initCB : CacheBoxRO[Seq[RV#TToken]] = CacheDerivedRO(patternLB, returnValue.initCB, nextBlock.get.__dev.initCB){
          DFBool.Token.select(patternLB, returnValue.initCB, nextBlock.get.__dev.initCB)
        }
      }
      override private[DFiant] lazy val __dev : __DevDFCasePatternBlock = new __DevDFCasePatternBlock {}
      import __dev._

      final val matchVal = matchHeader.matchVal
      def casedf[MC, R](pattern : matchVal.TPatternAble[MC]*)(block : => Able[R])(implicit ctx : Context, patternBld : matchVal.TPatternBuilder[MV], retBld : Builder[R])
      : DFCasePatternBlock[MV] = new DFCasePatternBlock[MV](matchHeader)(Some(this), patternBld(matchVal, pattern), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
      def casedf_[R](block : => Able[R])(implicit ctx : Context, retBld : Builder[R])
      : RV = {
        val dfCase_Block = new DFCase_Block[MV](matchHeader)(Some(this), retBld(returnVar.asInstanceOf[RV], block).asInstanceOf[RV])
        returnVar.asInstanceOf[RV]
      }
      def enddf(implicit ctx : Context) : RV = {
        returnVar.asInstanceOf[RV]
      }

      protected val addPatternToHeader : Unit = if (pattern != null) matchHeader.addCasePattern(pattern.asInstanceOf[matchHeader.matchVal.TPattern])
    }

    protected[DFiant] class DFCase_Block[MV <: DFAny](matchHeader : DFMatchHeader[MV])(prevCase : Option[DFCasePatternBlock[MV]], block : => RV)(
      implicit ctx : Context, mutableOwner: MutableOwner
    ) extends DFCasePatternBlock[MV](matchHeader)(prevCase, null.asInstanceOf[MV#TPattern], block) with Case_Block {
      protected[DFiant] trait __DevDFCase_Block extends __DevDFCasePatternBlock {
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Conditional Blocks
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val hasCase_ : Boolean = true

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Naming
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val nameScala: String = s"$ctx${Meta.Name.Separator}case_"
        override def codeString: String = s".casedf_ {$bodyCodeString\n}"

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Initialization
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        override lazy val initCB : CacheBoxRO[Seq[RV#TToken]] = returnValue.initCB
      }
      override private[DFiant] lazy val __dev : __DevDFCase_Block = new __DevDFCase_Block {}
      import __dev._
    }

    def apply[MV <: DFAny](matchValue : MV, matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
      implicit ctx : Context
    ): DFMatchHeader[MV#TVal] =
      new DFMatchHeader[MV#TVal](matchValue.replacement().asInstanceOf[MV#TVal], matchConfig)(ctx, ctx.owner.mutableOwner)
  }

}



