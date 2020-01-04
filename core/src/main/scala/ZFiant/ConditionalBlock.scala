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

package ZFiant
import DFiant.internals._

sealed trait ConditionalBlock extends DFBlock with CanBeGuarded {
  type TRet
  private[ZFiant] def applyBlock(block : => TRet)(implicit ctx : DFBlock.Context) : Unit
}

sealed trait MatchConfig extends Product with Serializable
object MatchConfig {
  case object NoOverlappingCases extends MatchConfig
  case object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  trait Of[Ret] extends ConditionalBlock{type TRet = Ret}
  class PrevBlockRef[+CB <: ConditionalBlock] extends DFMember.Ref[CB]
  object PrevBlockRef {
    implicit def apply[CB <: ConditionalBlock](member : CB)(implicit ctx : DFMember.Context) : PrevBlockRef[CB] =
      DFMember.Ref.newRefFor(new PrevBlockRef[CB], member)
  }
  class CondRef extends DFAny.Ref.ConsumeFrom[DFBool]
  object CondRef extends DFAny.Ref.ConsumeFrom.CO[DFBool, CondRef](new CondRef)

  class MatchValRef[MVType <: DFAny.Type] extends DFAny.Ref[DFAny.Of[MVType]]
  object MatchValRef {
    implicit def refOf[MVType <: DFAny.Type](member : DFAny.Of[MVType])(implicit ctx : DFMember.Context) : MatchValRef[MVType] =
      DFMember.Ref.newRefFor(new MatchValRef[MVType], member)
  }

  sealed trait MatchHeader extends CanBeGuarded {
    type TMVType <: DFAny.Type
    val matchValRef : MatchValRef[TMVType]
    val matchConfig : MatchConfig
    def codeString(implicit getset : MemberGetSet) : String = matchConfig match  {
      case MatchConfig.NoOverlappingCases => s"matchdf(${matchValRef.refCodeString})"
      case MatchConfig.AllowOverlappingCases => s"matchdf(${matchValRef.refCodeString}, MatchConfig.AllowOverlappingCases)"
    }
  }
  object MatchHeader {
    trait Of[MVType <: DFAny.Type] extends MatchHeader{type TMVType = MVType}
    class Ref[+MH <: MatchHeader] extends DFMember.Ref[MH]
    object Ref {
      implicit def apply[MH <: MatchHeader](member : MH)(implicit ctx : DFMember.Context) : Ref[MH] =
        DFMember.Ref.newRefFor(new Ref[MH], member)
    }
  }
  sealed trait IfBlock extends DFBlock {
    val condRef : CondRef
    def headerCodeString(implicit getset : MemberGetSet) : String = s"ifdf(${condRef.refCodeString})"
  }
  sealed trait ElseIfBlock extends DFBlock {
    val condRef : CondRef
    val prevBlockRef : PrevBlockRef[ConditionalBlock]
    def headerCodeString(implicit getset : MemberGetSet) : String = s".elseifdf(${condRef.refCodeString})"
  }
  sealed trait ElseBlock extends DFBlock {
    val prevBlockRef : PrevBlockRef[ConditionalBlock]
    def headerCodeString(implicit getset : MemberGetSet) : String = s".elsedf"
  }

  sealed trait CasePatternBlock[MVType <: DFAny.Type] extends DFBlock {
    val pattern : MVType#TPattern
    val matchHeaderRef : MatchHeader.Ref[MatchHeader]
    def headerCodeString(implicit getset : MemberGetSet) : String = s".casedf(${pattern.codeString})"
  }
  sealed trait Case_Block extends DFBlock {
    val matchHeaderRef : MatchHeader.Ref[MatchHeader]
    def headerCodeString(implicit getset : MemberGetSet) : String = s".casedf_"
  }

  sealed trait WithRetVal[Type <: DFAny.Type] extends
    ConditionalBlock.Of[DFAny.Of[Type]] with DFAny.DefaultRet[Type] {
    val retVar : DFAny.VarOf[Type]
    final val thisVal : DFAny.Of[Type] = retVar
    final val dfType: Type = retVar.dfType

    private[ZFiant] def applyBlock(block : => DFAny.Of[Type])(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.ownerInjector.injectOwnerAndRun(this) {
      val returnValue = block
      retVar.assign(returnValue)(new DFAny.Context(ctx.meta, ctx.ownerInjector, ctx.db))
    }
  }
  object WithRetVal {
    final case class IfBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], condRef : CondRef, ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.IfBlock with WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVar, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVar, condConv(DFBool.Type(), cond), this)(blockConv(dfType, block))(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object IfBlock {
      def apply[Type <: DFAny.Type](retVar : DFAny.VarOf[Type], cond: DFBool)(block: => DFAny.Of[Type])(
        implicit ctx: DFBlock.Context
      ): IfBlock[Type] = ctx.db.addConditionalBlock(IfBlock[Type](retVar, cond, ctx.owner, ctx.meta), block)
    }
    final case class ElseIfBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], condRef : CondRef, prevBlockRef : PrevBlockRef[WithRetVal[Type]], ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.ElseIfBlock with WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVar, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVar, condConv(DFBool.Type(), cond), this)(blockConv(dfType, block))(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object ElseIfBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], cond: DFBool, prevBlock: WithRetVal[Type]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseIfBlock[Type] =
        ctx.db.addConditionalBlock(ElseIfBlock[Type](retVar, cond, prevBlock, ctx.owner, ctx.meta), block)
    }
    final case class ElseBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], prevBlockRef : PrevBlockRef[WithRetVal[Type]], ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.ElseBlock with WithRetVal[Type] {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object ElseBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], prevBlock: WithRetVal[Type]
      )(block : => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseBlock[Type] =
        ctx.db.addConditionalBlock(ElseBlock[Type](retVar, prevBlock, ctx.owner, ctx.meta), block)
    }

    final case class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], mvType : MVType,
      matchValRef : MatchValRef[MVType], matchConfig: MatchConfig, ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.MatchHeader.Of[MVType] {
      private val dfType = retVar.dfType
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVar, this, None, patternBld(mvType, pattern)
      )(retBld(dfType, block))(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object MatchHeader {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[Type, MVType] =
        ctx.db.addMember(MatchHeader(retVar, matchVal.dfType, matchVal, matchConfig, ctx.owner, ctx.meta))
    }
    final case class DFCasePatternBlock[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[Type, MVType]],
      prevCaseRef : Option[PrevBlockRef[DFCasePatternBlock[Type, MVType]]], pattern : MVType#TPattern,
      ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.CasePatternBlock[MVType] with WithRetVal[Type] {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVar, matchHeaderRef, Some(this), patternBld(mvType, pattern)
      )(retBld(dfType, block))(ctx)
      def casedf_[MC, B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCase_Block[Type, MVType] =
        DFCase_Block[Type, MVType](retVar, matchHeaderRef, this)(retBld(dfType, block))(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object DFCasePatternBlock {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: Option[DFCasePatternBlock[Type, MVType]], pattern: MVType#TPattern
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCasePatternBlock[Type, MVType] =
        ctx.db.addConditionalBlock(DFCasePatternBlock(retVar, matchHeader.mvType, matchHeader, prevCase.map(p => PrevBlockRef(p)), pattern, ctx.owner, ctx.meta), block)
    }
    final case class DFCase_Block[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[Type, MVType]],
      prevCase : PrevBlockRef[DFCasePatternBlock[Type, MVType]], ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.Case_Block with WithRetVal[Type] {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object DFCase_Block {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: DFCasePatternBlock[Type, MVType]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCase_Block[Type, MVType] =
        ctx.db.addConditionalBlock(DFCase_Block[Type, MVType](retVar, matchHeader.mvType, matchHeader, prevCase, ctx.owner, ctx.meta), block)
    }
  }
  sealed trait NoRetVal extends ConditionalBlock.Of[Unit] {
    private[ZFiant] def applyBlock(block : => Unit)(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.ownerInjector.injectOwnerAndRun(this)(block)
  }
  object NoRetVal {
    final case class IfBlock(condRef : CondRef, ownerRef : DFBlock.Ref, tags : DFMember.Tags) extends ConditionalBlock.IfBlock with NoRetVal {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), this)(block)(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object IfBlock {
      def apply(cond: DFBool)(block: => Unit)(implicit ctx: DFBlock.Context)
      : IfBlock = ctx.db.addConditionalBlock(IfBlock(cond, ctx.owner, ctx.meta), block)
    }
    final case class ElseIfBlock(condRef : CondRef, prevBlockRef : PrevBlockRef[NoRetVal], ownerRef : DFBlock.Ref, tags : DFMember.Tags) extends ConditionalBlock.ElseIfBlock with NoRetVal {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), this)(block)(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object ElseIfBlock {
      def apply(cond: DFBool, prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseIfBlock = ctx.db.addConditionalBlock(ElseIfBlock(cond, prevBlock, ctx.owner, ctx.meta), block)
    }
    final case class ElseBlock(prevBlockRef : PrevBlockRef[NoRetVal], ownerRef : DFBlock.Ref, tags : DFMember.Tags) extends ConditionalBlock.ElseBlock with NoRetVal {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object ElseBlock {
      def apply(prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseBlock = ctx.db.addConditionalBlock(ElseBlock(prevBlock, ctx.owner, ctx.meta), block)
    }

    final case class MatchHeader[MVType <: DFAny.Type](
      mvType : MVType,
      matchValRef : MatchValRef[MVType], matchConfig: MatchConfig, ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.MatchHeader.Of[MVType] {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType]
      ) : DFCasePatternBlock[MVType] = DFCasePatternBlock[MVType](
        this, None, patternBld(mvType, pattern)
      )(block)(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object MatchHeader {
      def apply[MVType <: DFAny.Type](
        matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[MVType] =
        ctx.db.addMember(MatchHeader(matchVal.dfType, matchVal, matchConfig, ctx.owner, ctx.meta))
    }
    final case class DFCasePatternBlock[MVType <: DFAny.Type](
      mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[MVType]],
      prevCaseRef : Option[PrevBlockRef[DFCasePatternBlock[MVType]]], pattern : MVType#TPattern, ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.CasePatternBlock[MVType] with NoRetVal {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType]
      ) : DFCasePatternBlock[MVType] = DFCasePatternBlock[MVType](
        matchHeaderRef, Some(this), patternBld(mvType, pattern)
      )(block)(ctx)
      def casedf_[MC, B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : DFCase_Block[MVType] = DFCase_Block[MVType](
        matchHeaderRef, this
      )(block)(ctx)
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object DFCasePatternBlock {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType],
        prevCase: Option[DFCasePatternBlock[MVType]], pattern: MVType#TPattern
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCasePatternBlock[MVType] =
        ctx.db.addConditionalBlock(DFCasePatternBlock(matchHeader.mvType, matchHeader, prevCase.map(p => PrevBlockRef(p)), pattern, ctx.owner, ctx.meta), block)
    }
    final case class DFCase_Block[MVType <: DFAny.Type](
      mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[MVType]],
      prevCase : PrevBlockRef[DFCasePatternBlock[MVType]], ownerRef : DFBlock.Ref, tags : DFMember.Tags
    ) extends ConditionalBlock.Case_Block with NoRetVal {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
    }
    object DFCase_Block {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType], prevCase: DFCasePatternBlock[MVType]
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCase_Block[MVType] =
        ctx.db.addConditionalBlock(DFCase_Block(matchHeader.mvType, matchHeader, prevCase, ctx.owner, ctx.meta), block)
    }
  }
}

