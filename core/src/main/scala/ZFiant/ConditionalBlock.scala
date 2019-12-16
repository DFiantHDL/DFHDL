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
import DFiant.internals.Meta

sealed abstract class ConditionalBlock[Ret](block : => Ret) extends DFBlock {
  private[ZFiant] val originalOwner : DFBlock = owner.__injectedOwner
  private[ZFiant] lazy val applyBlock : Unit = ???
}

sealed trait MatchConfig extends Product with Serializable
object MatchConfig {
  case object NoOverlappingCases extends MatchConfig
  case object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  sealed abstract class WithRetVal[Type <: DFAny.Type](block : => DFAny.Of[Type]) extends
    ConditionalBlock[DFAny.Of[Type]](block) {
    val retVar : DFAny.VarOf[Type]
    lazy val dfType: Type = retVar.dfType

    override private[ZFiant] lazy val applyBlock : Unit = {
      owner.__injectedOwner = this
      val returnValue = block
      retVar.assign(returnValue)(DFAny.Context(meta.anonymize, this, topDesign.__db))
      owner.__injectedOwner = originalOwner
    }
  }
  object WithRetVal {
    final case class IfBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], condRef : DFRef[DFBool], ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => DFAny.Of[Type]) extends WithRetVal[Type](block) {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVar, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVar, condConv(DFBool.Type(), cond), this)(blockConv(dfType, block))(ctx)
      override lazy val typeName: String = "IfBlock"
    }
    object IfBlock {
      def apply[Type <: DFAny.Type](retVar : DFAny.VarOf[Type], cond: DFBool)(block: => DFAny.Of[Type])(
        implicit ctx: DFBlock.Context
      ): IfBlock[Type] = ctx.db.addConditionalBlock(IfBlock[Type](retVar, DFRef(cond), ctx.owner, ctx.meta)(block))
    }
    final case class ElseIfBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], condRef : DFRef[DFBool], prevBlockRef : DFRef[WithRetVal[Type]], ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => DFAny.Of[Type]) extends WithRetVal[Type](block) {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](retVar, this)(blockConv(dfType, block))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](retVar, condConv(DFBool.Type(), cond), this)(blockConv(dfType, block))(ctx)
      override lazy val typeName: String = "ElseIfBlock"
    }
    object ElseIfBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], cond: DFBool, prevBlock: WithRetVal[Type]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseIfBlock[Type] =
        ctx.db.addConditionalBlock(ElseIfBlock[Type](retVar, DFRef(cond), DFRef(prevBlock), ctx.owner, ctx.meta)(block))
    }
    final case class ElseBlock[Type <: DFAny.Type](
      retVar : DFAny.VarOf[Type], prevBlockRef : DFRef[WithRetVal[Type]], ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => DFAny.Of[Type]) extends WithRetVal[Type](block) {
      override lazy val typeName: String = "ElseBlock"
    }
    object ElseBlock {
      def apply[Type <: DFAny.Type](
        retVar : DFAny.VarOf[Type], prevBlock: WithRetVal[Type]
      )(block : => DFAny.Of[Type])(implicit ctx: DFBlock.Context) : ElseBlock[Type] =
        ctx.db.addConditionalBlock(ElseBlock[Type](retVar, DFRef(prevBlock), ctx.owner, ctx.meta)(block))
    }

    final case class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], matchValRef : DFRef[DFAny.Of[MVType]], matchConfig: MatchConfig, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends DFMember {
      private[WithRetVal] val matchVal = matchValRef.get
      private val dfType = retVar.dfType
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVar, this, None, patternBld(matchVal, pattern)
      )(retBld(dfType, block))(ctx)
    }
    object MatchHeader {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[Type, MVType] =
        ctx.db.addMember(MatchHeader(retVar, DFRef(matchVal), matchConfig, ctx.owner, ctx.meta))
    }
    final case class DFCasePatternBlock[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], matchHeaderRef : DFRef[MatchHeader[Type, MVType]],
      prevCaseRef : Option[DFRef[DFCasePatternBlock[Type, MVType]]], pattern : MVType#TPattern,
      ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => DFAny.Of[Type]) extends WithRetVal[Type](block) {
      private[WithRetVal] val matchVal = matchHeaderRef.matchVal
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        retVar, matchHeaderRef, Some(this), patternBld(matchVal, pattern)
      )(retBld(dfType, block))(ctx)
      def casedf_[MC, B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCase_Block[Type, MVType] =
        DFCase_Block[Type, MVType](retVar, matchHeaderRef, this)(retBld(dfType, block))(ctx)
    }
    object DFCasePatternBlock {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: Option[DFCasePatternBlock[Type, MVType]], pattern: MVType#TPattern
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCasePatternBlock[Type, MVType] =
        ctx.db.addConditionalBlock(DFCasePatternBlock(retVar, matchHeader, prevCase.map(p => DFRef(p)), pattern, ctx.owner, ctx.meta)(block))
    }
    final case class DFCase_Block[Type <: DFAny.Type, MVType <: DFAny.Type](
      retVar : DFAny.VarOf[Type], matchHeaderRef : DFRef[MatchHeader[Type, MVType]],
      prevCase : DFRef[DFCasePatternBlock[Type, MVType]], ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => DFAny.Of[Type]) extends WithRetVal[Type](block)
    object DFCase_Block {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        retVar : DFAny.VarOf[Type], matchHeader: MatchHeader[Type, MVType], prevCase: DFCasePatternBlock[Type, MVType]
      )(block: => DFAny.Of[Type])(implicit ctx: DFBlock.Context): DFCase_Block[Type, MVType] =
        ctx.db.addConditionalBlock(DFCase_Block[Type, MVType](retVar, matchHeader, DFRef(prevCase), ctx.owner, ctx.meta)(block))
    }
  }
  sealed abstract class NoRetVal(block : => Unit) extends ConditionalBlock[Unit](block) {
    override private[ZFiant] lazy val applyBlock : Unit = {
      owner.__injectedOwner = this
      block
      owner.__injectedOwner = originalOwner
    }
  }
  object NoRetVal {
    final case class IfBlock(condRef : DFRef[DFBool], ownerRef: DFRef[DFBlock], meta: Meta)(block : => Unit) extends NoRetVal(block) {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), this)(block)(ctx)
    }
    object IfBlock {
      def apply(cond: DFBool)(block: => Unit)(implicit ctx: DFBlock.Context)
      : IfBlock = ctx.db.addConditionalBlock(IfBlock(DFRef(cond), ctx.owner, ctx.meta)(block))
    }
    final case class ElseIfBlock(condRef : DFRef[DFBool], prevBlockRef : DFRef[NoRetVal], ownerRef: DFRef[DFBlock], meta: Meta)(block : => Unit) extends NoRetVal(block) {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(this)(block)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), this)(block)(ctx)
    }
    object ElseIfBlock {
      def apply(cond: DFBool, prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseIfBlock = ctx.db.addConditionalBlock(ElseIfBlock(DFRef(cond), prevBlock, ctx.owner, ctx.meta)(block))
    }
    final case class ElseBlock(prevBlockRef : DFRef[NoRetVal], ownerRef: DFRef[DFBlock], meta: Meta)(block : => Unit)(
      implicit val ctx : DFBlock.Context
    ) extends NoRetVal(block)
    object ElseBlock {
      def apply(prevBlock: NoRetVal)(block: => Unit)(
        implicit ctx: DFBlock.Context
      ): ElseBlock = ctx.db.addConditionalBlock(ElseBlock(prevBlock, ctx.owner, ctx.meta)(block)(ctx))
    }

    final case class MatchHeader[MVType <: DFAny.Type](
      matchValRef : DFRef[DFAny.Of[MVType]], matchConfig: MatchConfig, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends DFMember {
      private[NoRetVal] val matchVal = matchValRef.get
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
      ) : DFCasePatternBlock[MVType] = DFCasePatternBlock[MVType](
        this, None, patternBld(matchVal, pattern)
      )(block)(ctx)
    }
    object MatchHeader {
      def apply[MVType <: DFAny.Type](
        matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[MVType] =
        ctx.db.addMember(MatchHeader(DFRef(matchVal), matchConfig, ctx.owner, ctx.meta))
    }
    final case class DFCasePatternBlock[MVType <: DFAny.Type](
      matchHeaderRef : DFRef[MatchHeader[MVType]],
      prevCaseRef : Option[DFRef[DFCasePatternBlock[MVType]]], pattern : MVType#TPattern, ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => Unit) extends NoRetVal(block) {
      private[NoRetVal] val matchVal = matchHeaderRef.matchVal
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
      ) : DFCasePatternBlock[MVType] = DFCasePatternBlock[MVType](
        matchHeaderRef, Some(this), patternBld(matchVal, pattern)
      )(block)(ctx)
      def casedf_[MC, B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : DFCase_Block[MVType] = DFCase_Block[MVType](
        matchHeaderRef, this
      )(block)(ctx)
    }
    object DFCasePatternBlock {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType], prevCase: Option[DFCasePatternBlock[MVType]], pattern: MVType#TPattern
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCasePatternBlock[MVType] =
        ctx.db.addConditionalBlock(DFCasePatternBlock(matchHeader, prevCase.map(p => DFRef(p)), pattern, ctx.owner, ctx.meta)(block))
    }
    final case class DFCase_Block[MVType <: DFAny.Type](
      matchHeaderRef : DFRef[MatchHeader[MVType]],
      prevCase : DFRef[DFCasePatternBlock[MVType]], ownerRef: DFRef[DFBlock], meta: Meta
    )(block : => Unit) extends NoRetVal(block)
    object DFCase_Block {
      def apply[MVType <: DFAny.Type](
        matchHeader: MatchHeader[MVType], prevCase: DFCasePatternBlock[MVType]
      )(block: => Unit)(implicit ctx: DFBlock.Context): DFCase_Block[MVType] =
        ctx.db.addConditionalBlock(DFCase_Block(matchHeader, prevCase, ctx.owner, ctx.meta)(block))
    }
  }
}

