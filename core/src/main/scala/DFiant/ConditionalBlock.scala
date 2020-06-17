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
import DFiant.internals._
import DFiant.compiler.printer.Printer

sealed trait ConditionalBlock extends DFBlock with CanBeGuarded with DFAny.CanBeAnonymous {
  type TTags = DFMember.Tags.Basic
  type TCustomTag = DFMember.CustomTag
  type TRet
  private[DFiant] def applyBlock(block : => TRet)(implicit ctx : DFBlock.Context) : Unit
}

sealed trait MatchConfig extends Product with Serializable
object MatchConfig {
  case object NoOverlappingCases extends MatchConfig
  case object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  sealed trait Of[Ret] extends ConditionalBlock{type TRet = Ret}
  type PrevBlockRef[+CB <: ConditionalBlock] = DFMember.Ref.Of[PrevBlockRef.Type, CB]
  object PrevBlockRef {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
    def apply[CB <: ConditionalBlock](member : CB)(implicit ctx : DFMember.Context): PrevBlockRef[CB] = DFMember.Ref(member)
    def unapply(ref : DFMember.Ref): Boolean = ref.refType match {
      case _ : Type => true
      case _ => false
    }
  }

  type CondRef = DFMember.OwnedRef.Of[CondRef.Type, DFBool]
  object CondRef {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  type MatchValRef[MVType <: DFAny.Type] = DFMember.OwnedRef.Of[MatchValRef.Type, DFAny.Of[MVType]]
  object MatchValRef {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  sealed trait MatchHeader extends CanBeGuarded with DFAny.CanBeAnonymous {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    type TMVType <: DFAny.Type
    val matchValRef : MatchValRef[TMVType]
    val matchConfig : MatchConfig
    def codeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      matchConfig match  {
        case MatchConfig.NoOverlappingCases => s"$DF matchdf(${matchValRef.refCodeString})"
        case MatchConfig.AllowOverlappingCases => s"$DF matchdf(${matchValRef.refCodeString}, MatchConfig.AllowOverlappingCases)"
      }
    }
  }
  object MatchHeader {
    trait Of[MVType <: DFAny.Type] extends MatchHeader{type TMVType = MVType}
    type Ref[+MH <: MatchHeader] = DFMember.Ref.Of[Ref.Type, MH]
    object Ref {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
  }
  sealed trait IfBlock extends ConditionalBlock {
    val condRef : CondRef
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s"$DF ifdf(${condRef.refCodeString})"
    }
  }
  sealed trait ElseIfBlock extends ConditionalBlock {
    val condRef : CondRef
    val prevBlockRef : PrevBlockRef[ConditionalBlock]
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s".$DF elseifdf(${condRef.refCodeString})"
    }
  }
  sealed trait ElseBlock extends ConditionalBlock {
    val prevBlockRef : PrevBlockRef[ConditionalBlock]
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s".$DF elsedf"
    }
  }

  sealed trait CasePatternBlock[MVType <: DFAny.Type] extends ConditionalBlock {
    val pattern : MVType#TPattern
    val matchHeaderRef : MatchHeader.Ref[MatchHeader]
    val prevCaseRefOption : Option[PrevBlockRef[CasePatternBlock[MVType]]]
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s".$DF casedf(${pattern.codeString})"
    }
  }
  sealed trait Case_Block[MVType <: DFAny.Type] extends ConditionalBlock {
    val matchHeaderRef : MatchHeader.Ref[MatchHeader]
    val prevCaseRef : PrevBlockRef[CasePatternBlock[MVType]]
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s".$DF casedf_"
    }
  }

  sealed trait WithRetVal[Type <: DFAny.Type] extends
    ConditionalBlock.Of[DFAny.Of[Type]] with DFAny.DefaultRet[Type] {
    val retVarRef : WithRetVal.RetVarRef[Type]
    final def thisVal(implicit getSet: MemberGetSet) : DFAny.Of[Type] = retVarRef

    private[DFiant] def applyBlock(block : => DFAny.Of[Type])(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.ownerInjector.injectOwnerAndRun(this) {
      val returnValue = block
      retVarRef.get.assign(returnValue)
    }
  }
  object WithRetVal {
    type RetVarRef[Type <: DFAny.Type] = DFMember.Ref.Of[RetVarRef.Type, DFAny.VarOf[Type]]
    object RetVarRef {
      trait Type extends DFMember.Ref.Type
      implicit val ev : Type = new Type {}
    }
    final case class IfBlock[Type <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], condRef : CondRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.IfBlock with WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVarRef, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : C)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVarRef, condArg(), this)(blockConv(dfType, block))(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case IfBlock(dfType, retVarRef, condRef, _, tags) =>
          this.dfType == dfType && this.retVarRef =~ retVarRef && this.condRef =~ condRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object IfBlock {
      def apply[Type <: DFAny.Type](retVar : DFAny.VarOf[Type], cond: DFBool)(block: => DFAny.Of[Type])(
        implicit ctx: DFBlock.Context
      ): IfBlock[Type] = {
        implicit lazy val ret : IfBlock[Type] with DFMember.RefOwner =
          ctx.db.addConditionalBlock(IfBlock[Type](retVar.dfType, retVar, cond, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class ElseIfBlock[Type <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], condRef : CondRef, prevBlockRef : PrevBlockRef[WithRetVal[Type]], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.ElseIfBlock with WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVarRef, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : C)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVarRef, condArg(), this)(blockConv(dfType, block))(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case ElseIfBlock(dfType, retVarRef, condRef, prevBlockRef, _, tags) =>
          this.dfType == dfType && this.retVarRef =~ retVarRef && this.condRef =~ condRef && this.prevBlockRef =~ prevBlockRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object ElseIfBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], cond: DFBool, prevBlock: WithRetVal[Type]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseIfBlock[Type] = {
        implicit lazy val ret : ElseIfBlock[Type] with DFMember.RefOwner =
          ctx.db.addConditionalBlock(ElseIfBlock[Type](retVar.dfType, retVar, cond, prevBlock, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class ElseBlock[Type <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], prevBlockRef : PrevBlockRef[WithRetVal[Type]], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.ElseBlock with WithRetVal[Type] {
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case ElseBlock(dfType, retVarRef, prevBlockRef, _, tags) =>
          this.dfType == dfType && this.retVarRef =~ retVarRef && this.prevBlockRef =~ prevBlockRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object ElseBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], prevBlock: WithRetVal[Type]
      )(block : => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseBlock[Type] =
        ctx.db.addConditionalBlock(ElseBlock[Type](retVar.dfType, retVar, prevBlock, ctx.owner, ctx.meta), block)
    }

    final case class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], mvType : MVType,
      matchValRef : MatchValRef[MVType], matchConfig: MatchConfig, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.MatchHeader.Of[MVType] {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVarRef, this, None, patternBld(mvType, pattern)
      )(retBld(dfType, block))(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case MatchHeader(dfType, retVarRef, mvType, matchValRef, matchConfig, _, tags) =>
          this.dfType == dfType && this.retVarRef =~ retVarRef && this.mvType == mvType &&
          this.matchValRef =~ matchValRef && this.matchConfig == matchConfig && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object MatchHeader {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[Type, MVType] = {
        implicit lazy val ret : MatchHeader[Type, MVType] with DFMember.RefOwner =
          ctx.db.addMember(MatchHeader(retVar.dfType, retVar, matchVal.dfType, matchVal, matchConfig, ctx.owner, ctx.meta)).asRefOwner
        ret
      }
    }
    final case class DFCasePatternBlock[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[Type, MVType]],
      prevCaseRefOption : Option[PrevBlockRef[DFCasePatternBlock[Type, MVType]]], pattern : MVType#TPattern,
      ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.CasePatternBlock[MVType] with WithRetVal[Type] {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVarRef, matchHeaderRef, Some(this), patternBld(mvType, pattern)
      )(retBld(dfType, block))(ctx)
      def casedf_[MC, B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCase_Block[Type, MVType] =
        DFCase_Block[Type, MVType](retVarRef, matchHeaderRef, this)(retBld(dfType, block))(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case DFCasePatternBlock(dfType, retVarRef, mvType, matchHeaderRef, prevCaseRefOption, pattern, _, tags) =>
          val prevCaseEq = (this.prevCaseRefOption, prevCaseRefOption) match {
            case (Some(r1), Some(r2)) => r1 =~ r2
            case (None, None) => true
            case _ => false
          }
          prevCaseEq && this.dfType == dfType && this.retVarRef =~ retVarRef && this.mvType == mvType &&
          this.matchHeaderRef =~ matchHeaderRef && this.pattern == pattern && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object DFCasePatternBlock {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: Option[DFCasePatternBlock[Type, MVType]], pattern: MVType#TPattern
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCasePatternBlock[Type, MVType] = {
        implicit lazy val ret : DFCasePatternBlock[Type, MVType] with DFMember.RefOwner =
          ctx.db.addConditionalBlock(DFCasePatternBlock(retVar.dfType, retVar, matchHeader.mvType, matchHeader, prevCase.map(p => PrevBlockRef(p)), pattern, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class DFCase_Block[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, retVarRef : RetVarRef[Type], mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[Type, MVType]],
      prevCaseRef : PrevBlockRef[DFCasePatternBlock[Type, MVType]], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.Case_Block[MVType] with WithRetVal[Type] {
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case DFCase_Block(dfType, retVarRef, mvType, matchHeaderRef, prevCaseRef, _, tags) =>
          this.dfType == dfType && this.retVarRef =~ retVarRef && this.mvType == mvType &&
            this.matchHeaderRef =~ matchHeaderRef && this.prevCaseRef =~ prevCaseRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object DFCase_Block {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: DFCasePatternBlock[Type, MVType]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCase_Block[Type, MVType] =
        ctx.db.addConditionalBlock(DFCase_Block[Type, MVType](retVar.dfType, retVar, matchHeader.mvType, matchHeader, prevCase, ctx.owner, ctx.meta), block)
    }
  }
  sealed trait NoRetVal extends ConditionalBlock.Of[Unit] {
    private[DFiant] def applyBlock(block : => Unit)(
      implicit ctx : DFBlock.Context
    ) : Unit = ctx.ownerInjector.injectOwnerAndRun(this)(block)
  }
  object NoRetVal {
    sealed trait HasElseIfDF {
      def elseifdf[C, B](cond : C)(block : => Unit)(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0]
      ) : ElseIfBlock
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock
    }
    final case class IfBlock(
      condRef : CondRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.IfBlock with HasElseIfDF with NoRetVal {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : C)(block : => Unit)(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0]
      ) : ElseIfBlock = ElseIfBlock(condArg(), this)(block)(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case IfBlock(condRef, _, tags) =>
          this.condRef =~ condRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object IfBlock {
      def apply(cond: DFBool)(block: => Unit)(implicit ctx: DFBlock.Context)
      : IfBlock = {
        implicit lazy val ret : IfBlock with DFMember.RefOwner =
          ctx.db.addConditionalBlock(IfBlock(cond, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class ElseIfBlock(
      condRef : CondRef, prevBlockRef : PrevBlockRef[NoRetVal], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.ElseIfBlock with HasElseIfDF with NoRetVal {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : C)(block : => Unit)(
        implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0]
      ) : ElseIfBlock = ElseIfBlock(condArg(), this)(block)(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case ElseIfBlock(condRef, prevBlockRef, _, tags) =>
          this.condRef =~ condRef && this.prevBlockRef =~ prevBlockRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object ElseIfBlock {
      def apply(cond: DFBool, prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseIfBlock = {
        implicit lazy val ret : ElseIfBlock with DFMember.RefOwner =
          ctx.db.addConditionalBlock(ElseIfBlock(cond, prevBlock, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class ElseBlock(prevBlockRef : PrevBlockRef[NoRetVal], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends ConditionalBlock.ElseBlock with NoRetVal {
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case ElseBlock(prevBlockRef, _, tags) =>
          this.prevBlockRef =~ prevBlockRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object ElseBlock {
      def apply(prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseBlock = ctx.db.addConditionalBlock(ElseBlock(prevBlock, ctx.owner, ctx.meta), block)
    }

    sealed trait HasCaseDF[MVType <: DFAny.Type] {
      val mvType : MVType
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType]
      ) : DFCasePatternBlock[MVType]
    }

    final case class MatchHeader[MVType <: DFAny.Type](
      mvType : MVType,
      matchValRef : MatchValRef[MVType], matchConfig: MatchConfig, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.MatchHeader.Of[MVType] with HasCaseDF[MVType] {
      def casedf[MC, B](pattern : mvType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : mvType.TPatternBuilder[MVType]
      ) : DFCasePatternBlock[MVType] = DFCasePatternBlock[MVType](
        this, None, patternBld(mvType, pattern)
      )(block)(ctx)
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case MatchHeader(mvType, matchValRef, matchConfig, _, tags) =>
          this.mvType == mvType && this.matchValRef =~ matchValRef && this.matchConfig == matchConfig &&
            this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object MatchHeader {
      def apply[MVType <: DFAny.Type](
        matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[MVType] = {
        implicit lazy val ret : MatchHeader[MVType] with DFMember.RefOwner =
          ctx.db.addMember(MatchHeader(matchVal.dfType, matchVal, matchConfig, ctx.owner, ctx.meta)).asRefOwner
        ret
      }
    }
    final case class DFCasePatternBlock[MVType <: DFAny.Type](
      mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[MVType]],
      prevCaseRefOption : Option[PrevBlockRef[DFCasePatternBlock[MVType]]], pattern : MVType#TPattern, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.CasePatternBlock[MVType] with HasCaseDF[MVType] with NoRetVal {
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
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case DFCasePatternBlock(mvType, matchHeaderRef, prevCaseRefOption, pattern, _, tags) =>
          val prevCaseEq = (this.prevCaseRefOption, prevCaseRefOption) match {
            case (Some(r1), Some(r2)) => r1 =~ r2
            case (None, None) => true
            case _ => false
          }
          prevCaseEq && this.mvType == mvType && this.matchHeaderRef =~ matchHeaderRef && this.pattern == pattern && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object DFCasePatternBlock {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType],
        prevCase: Option[DFCasePatternBlock[MVType]], pattern: MVType#TPattern
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCasePatternBlock[MVType] = {
        implicit lazy val ret : DFCasePatternBlock[MVType] with DFMember.RefOwner =
          ctx.db.addConditionalBlock(DFCasePatternBlock(matchHeader.mvType, matchHeader, prevCase.map(p => PrevBlockRef(p)), pattern, ctx.owner, ctx.meta), block).asRefOwner
        ret
      }
    }
    final case class DFCase_Block[MVType <: DFAny.Type](
      mvType : MVType,
      matchHeaderRef : ConditionalBlock.MatchHeader.Ref[MatchHeader[MVType]],
      prevCaseRef : PrevBlockRef[DFCasePatternBlock[MVType]], ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
    ) extends ConditionalBlock.Case_Block[MVType] with NoRetVal {
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case DFCase_Block(mvType, matchHeaderRef, prevCaseRef, _, tags) =>
          this.mvType == mvType && this.matchHeaderRef =~ matchHeaderRef && this.prevCaseRef =~ prevCaseRef && this.tags =~ tags
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
    object DFCase_Block {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType], prevCase: DFCasePatternBlock[MVType]
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCase_Block[MVType] =
        ctx.db.addConditionalBlock(DFCase_Block(matchHeader.mvType, matchHeader, prevCase, ctx.owner, ctx.meta), block)
    }
  }
}

