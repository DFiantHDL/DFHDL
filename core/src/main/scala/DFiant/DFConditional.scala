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
import compiler.csprinter.CSPrinter
import DFMember.OwnedRefOption
import DFiant.internals.Exact
import OwnedRefOption._

sealed trait DFConditional[Owner <: DFConditional.Block] extends FSM.Capable {
  type TRet
  val owner : Owner
  private[DFiant] def applyBlock(block : => TRet)(implicit ctx : DFBlock.Context) : Unit
}

sealed trait MatchConfig extends Product with Serializable
object MatchConfig {
  case object NoOverlappingCases extends MatchConfig
  case object AllowOverlappingCases extends MatchConfig
}

object DFConditional {
  sealed trait Of[Owner <: DFConditional.Block, Ret] extends DFConditional[Owner]{type TRet = Ret}
  sealed trait Block extends DFBlock with CanBeGuarded with DFAny.CanBeAnonymous {
    val prevBlockRefOption : PrevBlockRefOption[Block]
  }
  type PrevBlockRefOption[+O <: Block] = DFMember.OwnedRefOption[PrevBlockRefOption.Type, O]
  object PrevBlockRefOption {
    trait Type extends DFMember.OwnedRef.Type
    implicit val ev : Type = new Type {}
    def unapply(ref : DFMember.OwnedRef): Boolean = ref.refType match {
      case _ : PrevBlockRefOption.Type => true
      case _ => false
    }
  }

  type CondRefOption = DFMember.OwnedRefOption[CondRefOption.Type, DFAny.Member]
  object CondRefOption {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  final case class IfElseBlock(
    condRefOption : CondRefOption, prevBlockRefOption : PrevBlockRefOption[IfElseBlock],
    ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends Block {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case IfElseBlock(condRefOption, prevBlockRefOption, _, tags) =>
        this.condRefOption =~ condRefOption && this.prevBlockRefOption =~ prevBlockRefOption && this.tags =~ tags
      case _ => false
    }
    def headerCodeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      prevBlockRefOption match {
        case None => s"$DF ifdf(${condRefOption.get.refCodeString})"
        case Some(_) => condRefOption match {
          case Some(condRef) => s".$DF elseifdf(${condRef.refCodeString})"
          case None => s".$DF elsedf"
        }
      }
    }
    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object IfElseBlock {
    def apply(
      cond: Option[DFAny.Member], prevBlock: Option[IfElseBlock]
    )(implicit ctx: DFBlock.Context) : IfElseBlock = {

      implicit lazy val ret : IfElseBlock with DFMember.RefOwner =
        ctx.db.addMemberOf[IfElseBlock](
          IfElseBlock(OwnedRefOption(cond), OwnedRefOption(prevBlock), ctx.owner, ctx.meta)
        )
      ret
    }
  }

  type MatchValRef = DFMember.OwnedRef.Of[MatchValRef.Type, DFAny.Member]
  object MatchValRef {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  final case class MatchHeader(
    matchValRef : MatchValRef, matchConfig: MatchConfig, ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends CanBeGuarded with DFAny.CanBeAnonymous {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case MatchHeader(matchValRef, matchConfig, _, tags) =>
        this.matchValRef =~ matchValRef && this.matchConfig == matchConfig && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      matchConfig match  {
        case MatchConfig.NoOverlappingCases => s"$DF matchdf(${matchValRef.refCodeString})"
        case MatchConfig.AllowOverlappingCases => s"$DF matchdf(${matchValRef.refCodeString}, MatchConfig.AllowOverlappingCases)"
      }
    }
    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember =
      getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }

  object MatchHeader {
    type Ref = DFMember.Ref.Of[Ref.Type, MatchHeader]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    def apply(matchVal: DFAny.Member, matchConfig: MatchConfig)(
      implicit ctx: DFMember.Context
    ): MatchHeader = {
      implicit lazy val ret : MatchHeader with DFMember.RefOwner =
        ctx.db.addMemberOf[MatchHeader](
          MatchHeader(matchVal, matchConfig, ctx.owner, ctx.meta)
        )
      ret
    }
  }

  final case class CaseBlock(
    matchHeaderRef : DFConditional.MatchHeader.Ref, prevBlockRefOption : PrevBlockRefOption[CaseBlock],
    patternOption : Option[DFAny.Pattern], ownerRef : DFOwner.Ref, tags : DFMember.Tags
  ) extends Block {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case CaseBlock(matchHeaderRef, prevBlockRefOption, patternOption, _, tags) =>
        this.prevBlockRefOption =~ prevBlockRefOption && this.matchHeaderRef =~ matchHeaderRef &&
          this.patternOption == patternOption && this.tags =~ tags
      case _ => false
    }
    def headerCodeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      patternOption match {
        case Some(pattern) => s".$DF casedf(${pattern.codeString})"
        case None => s".$DF casedf_"
      }
    }
    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object CaseBlock {
    def apply(
      matchHeader: MatchHeader, prevBlockOption: Option[CaseBlock], patternOption: Option[DFAny.Pattern]
    )(implicit ctx: DFBlock.Context): CaseBlock = {
      implicit lazy val ret : CaseBlock with DFMember.RefOwner =
        ctx.db.addMemberOf[CaseBlock](
          CaseBlock(matchHeader, OwnedRefOption(prevBlockOption), patternOption, ctx.owner, ctx.meta)
        )
      ret
    }
  }


  sealed abstract class WithRetVal[Owner <: DFConditional.Block, Type <: DFAny.Type](
    val retVar : DFAny.VarOf[Type]
  ) extends DFConditional.Of[Owner, DFAny.Of[Type]] with DFAny.Of[Type] {
    val member : DFAny.Member = retVar

    private[DFiant] def applyBlock(block : => DFAny.Of[Type])(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.db.OwnershipContext.injectOwnerAndRun(ctx.container, owner) {
      val returnValue = block
      retVar.assign(returnValue)
    }
  }
  object WithRetVal {
    final class IfElseBlock[Type <: DFAny.Type, HasElse](
      retVar : DFAny.VarOf[Type], condOption : Option[DFBool], prevBlockOption : Option[DFConditional.IfElseBlock]
    )(block: => DFAny.Of[Type])(implicit ctx : DFBlock.Context) extends WithRetVal[DFConditional.IfElseBlock, Type](retVar) {
      val owner : DFConditional.IfElseBlock = DFConditional.IfElseBlock(condOption.map(_.member), prevBlockOption)
      applyBlock(block)
    }
    object IfElseBlock {
      def newIf[Type <: DFAny.Type, B, C](retVar : DFAny.VarOf[Type], cond: DFBool)(block : => DFAny.Of[Type])(
        implicit ctx: DFBlock.Context
      ): IfElseBlock[Type, true] = new IfElseBlock[Type, true](retVar, Some(cond), None)(block)

      implicit class IfElseBlockOps[Type <: DFAny.Type](notElse : IfElseBlock[Type, true]) {
        import notElse.{retVar, dfType, owner}
        def elsedf[B](block : => Exact[B])(
          implicit ctx : DFBlock.Context, blockConv : retVar.Arg[B]
        ) : IfElseBlock[Type, false] =
          new IfElseBlock[Type, false](retVar, None, Some(owner))(blockConv(dfType, block))
        def elseifdf[C, B](cond : Exact[C])(block : => Exact[B])(
          implicit ctx : DFBlock.Context, condArg : DFBool.Arg[C], blockConv : retVar.Arg[B]
        ) : IfElseBlock[Type, true] =
          new IfElseBlock[Type, true](retVar, Some(condArg(cond)), Some(owner))(blockConv(dfType, block))
      }
    }

    final class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      val retVar : DFAny.VarOf[Type], matchVal : DFAny.Of[MVType], matchConfig: MatchConfig
    )(implicit ctx : DFMember.Context) {
      val dfType : Type = retVar.dfType
      val mvType : MVType = matchVal.dfType
      val member : DFConditional.MatchHeader = DFConditional.MatchHeader(matchVal, matchConfig)
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Exact[B])(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : retVar.Arg[B]
      ) : CaseBlock[Type, MVType, true] = new CaseBlock[Type, MVType, true](
        this, None, Some(patternBld(mvType, pattern))
      )(retBld(dfType, block))
    }

    final class CaseBlock[Type <: DFAny.Type, MVType <: DFAny.Type, HasMoreCases](
      val matchHeader : MatchHeader[Type, MVType],
      prevBlockOption : Option[DFConditional.CaseBlock], patternOption : Option[DFAny.Pattern]
    )(block: => DFAny.Of[Type])(implicit ctx : DFBlock.Context) extends WithRetVal[DFConditional.CaseBlock, Type](matchHeader.retVar) {
      val mvType : MVType = matchHeader.mvType
      val owner : DFConditional.CaseBlock =
        DFConditional.CaseBlock(matchHeader.member, prevBlockOption, patternOption)
      applyBlock(block)
    }
    object CaseBlock {
      implicit class CaseBlockOps[Type <: DFAny.Type, MVType <: DFAny.Type](notOthers : CaseBlock[Type, MVType, true]) {
        import notOthers.{dfType, mvType, matchHeader, owner}
        def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Exact[B])(
          implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : notOthers.retVar.Arg[B]
        ) : CaseBlock[Type, MVType, true] = new CaseBlock[Type, MVType, true](
          matchHeader, Some(owner), Some(patternBld(mvType, pattern))
        )(retBld(dfType, block))
        def casedf_[MC, B](block : => Exact[B])(
          implicit ctx : DFBlock.Context, retBld : DFAny.`Op:=,<>`.Builder[Type, B]
        ) : CaseBlock[Type, MVType, false] = new CaseBlock[Type, MVType, false](
          matchHeader, Some(owner), None
        )(retBld(dfType, block))
      }
    }
  }
  sealed trait NoRetVal[Owner <: DFConditional.Block] extends DFConditional.Of[Owner, Unit] {
    private[DFiant] def applyBlock(block : => Unit)(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.db.OwnershipContext.injectOwnerAndRun(ctx.container, owner)(block)
  }
  object NoRetVal {
    final class IfElseBlock[HasElse](
      condOption : Option[DFBool], prevBlockOption : Option[DFConditional.IfElseBlock]
    )(block: => Unit)(implicit ctx : DFBlock.Context) extends NoRetVal[DFConditional.IfElseBlock] {
      val owner : DFConditional.IfElseBlock = DFConditional.IfElseBlock(condOption.map(_.member), prevBlockOption)
      applyBlock(block)
    }
    object IfElseBlock {
      implicit class IfElseBlockOps(notElse : IfElseBlock[true]) {
        import notElse.owner
        def elsedf[B](block : => Unit)(
          implicit ctx : DFBlock.Context
        ) : IfElseBlock[false] = new IfElseBlock[false](None, Some(owner))(block)
        def elseifdf[C, B](cond : Exact[C])(block : => Unit)(
          implicit ctx : DFBlock.Context, condArg : DFBool.Arg[C]
        ) : IfElseBlock[true] = new IfElseBlock[true](Some(condArg(cond)), Some(owner))(block)
      }
    }

    sealed trait HasCaseDF[MVType <: DFAny.Type, HasMoreCases] {
      val mvType : MVType
      protected val matchHeader : MatchHeader[MVType]
      protected val thisBlockOption : Option[DFConditional.CaseBlock]
    }
    object HasCaseDF {
      implicit class CaseBlockOps[MVType <: DFAny.Type](notOthers : HasCaseDF[MVType, true]) {
        import notOthers.{mvType, matchHeader, thisBlockOption}
        def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Unit)(
          implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType]
        ) : CaseBlock[MVType, true] = new CaseBlock[MVType, true](
          matchHeader, thisBlockOption, Some(patternBld(mvType, pattern))
        )(block)
        def casedf_[MC, B](block : => Unit)(
          implicit ctx : DFBlock.Context
        ) : CaseBlock[MVType, false] = new CaseBlock[MVType, false](
          matchHeader, thisBlockOption, None
        )(block)
      }
    }

    final class MatchHeader[MVType <: DFAny.Type](
      matchVal : DFAny.Of[MVType], matchConfig: MatchConfig
    )(implicit ctx : DFMember.Context) extends HasCaseDF[MVType, true] {
      val mvType : MVType = matchVal.dfType
      val member : DFConditional.MatchHeader = DFConditional.MatchHeader(matchVal, matchConfig)
      protected val matchHeader : MatchHeader[MVType] = this
      protected val thisBlockOption : Option[DFConditional.CaseBlock] = None
    }

    final class CaseBlock[MVType <: DFAny.Type, HasMoreCases](
      val matchHeader : MatchHeader[MVType],
      prevBlockOption : Option[DFConditional.CaseBlock], patternOption : Option[DFAny.Pattern]
    )(block: => Unit)(
      implicit ctx : DFBlock.Context
    ) extends NoRetVal[DFConditional.CaseBlock] with HasCaseDF[MVType, HasMoreCases] {
      val mvType : MVType = matchHeader.mvType
      val owner : DFConditional.CaseBlock =
        DFConditional.CaseBlock(matchHeader.member, prevBlockOption, patternOption)
      protected val thisBlockOption : Option[DFConditional.CaseBlock] = Some(owner)
      applyBlock(block)
    }
  }
}

