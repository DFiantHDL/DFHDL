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

sealed trait ConditionalBlock[Ret] extends DFBlock {
  val block : () => Ret
  private val originalOwner : DFBlock = owner.__injectedOwner
  owner.__injectedOwner = this
  final val returnValue : Ret = block()
  owner.__injectedOwner = originalOwner
}

sealed trait MatchConfig extends Product with Serializable
object MatchConfig {
  case object NoOverlappingCases extends MatchConfig
  case object AllowOverlappingCases extends MatchConfig
}

object ConditionalBlock {
  sealed trait WithRetVal[Type <: DFAny.Type] extends ConditionalBlock[DFAny.Of[Type]] with DFAny.ValOrVar[Type, false]
  object WithRetVal {
    final case class IfBlock[Type <: DFAny.Type](
      dfType : Type, cond : DFBool, block : () => DFAny.Of[Type], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](dfType, () => blockConv(dfType, block), this)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, condConv(DFBool.Type(), cond), () => blockConv(dfType, block), this)(ctx)
    }
    object IfBlock {
      def apply[Type <: DFAny.Type](dfType: Type, cond: DFBool, block: () => DFAny.Of[Type])(
        implicit ctx: DFBlock.Context
      ): IfBlock[Type] = ctx.compiler.addMember(IfBlock(dfType, cond, block, ctx.owner, ctx.meta))
    }
    final case class ElseIfBlock[Type <: DFAny.Type](
      dfType : Type, cond : DFBool, block : () => DFAny.Of[Type], prevBlock : WithRetVal[Type], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends WithRetVal[Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](dfType, () => blockConv(dfType, block), this)(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, condConv(DFBool.Type(), cond), () => blockConv(dfType, block), this)(ctx)
    }
    object ElseIfBlock {
      def apply[Type <: DFAny.Type](
        dfType: Type, cond: DFBool, block: () => DFAny.Of[Type], prevBlock: WithRetVal[Type]
      )(implicit ctx: DFBlock.Context) : ElseIfBlock[Type] =
        ctx.compiler.addMember(ElseIfBlock[Type](dfType, cond, block, prevBlock, ctx.owner, ctx.meta))
    }
    final case class ElseBlock[Type <: DFAny.Type](
      dfType : Type, block : () => DFAny.Of[Type], prevBlock : WithRetVal[Type], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends WithRetVal[Type]
    object ElseBlock {
      def apply[Type <: DFAny.Type](
        dfType: Type, block: () => DFAny.Of[Type], prevBlock: WithRetVal[Type]
      )(implicit ctx: DFBlock.Context) : ElseBlock[Type] =
        ctx.compiler.addMember(ElseBlock[Type](dfType, block, prevBlock, ctx.owner, ctx.meta))
    }

    final case class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchVal : DFAny.Of[MVType], matchConfig: MatchConfig, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends DFMember {
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        dfType, this, None, patternBld(matchVal, pattern), () => retBld(dfType, block)
      )(ctx)
    }
    object MatchHeader {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        dfType: Type, matchVal: DFAny.Of[MVType], matchConfig: MatchConfig
      )(implicit ctx: DFMember.Context): MatchHeader[Type, MVType] =
        ctx.compiler.addMember(MatchHeader(dfType, matchVal, matchConfig, ctx.owner, ctx.meta))
    }
    final case class DFCasePatternBlock[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchHeader : MatchHeader[Type, MVType],
      prevCase : Option[DFCasePatternBlock[Type, MVType]], pattern : MVType#TPattern,
      block : () => DFAny.Of[Type], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends WithRetVal[Type] {
      def casedf[MC, B](pattern : matchHeader.matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchHeader.matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = DFCasePatternBlock[Type, MVType](
        dfType, matchHeader, Some(this), patternBld(matchHeader.matchVal, pattern), () => retBld(dfType, block)
      )(ctx)
      def casedf_[MC, B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCase_Block[Type, MVType] = DFCase_Block[Type, MVType](
        dfType, matchHeader, this, () => retBld(dfType, block)
      )(ctx)
    }
    object DFCasePatternBlock {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        dfType: Type, matchHeader: MatchHeader[Type, MVType], prevCase: Option[DFCasePatternBlock[Type, MVType]], pattern: MVType#TPattern, block: () => DFAny.Of[Type]
      )(implicit ctx: DFBlock.Context): DFCasePatternBlock[Type, MVType] =
        ctx.compiler.addMember(DFCasePatternBlock(dfType, matchHeader, prevCase, pattern, block, ctx.owner, ctx.meta))
    }
    final case class DFCase_Block[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchHeader : MatchHeader[Type, MVType],
      prevCase : DFCasePatternBlock[Type, MVType], block : () => DFAny.Of[Type], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends WithRetVal[Type]
    object DFCase_Block {
      def apply[Type <: DFAny.Type, MVType <: DFAny.Type](
        dfType: Type, matchHeader: MatchHeader[Type, MVType], prevCase: DFCasePatternBlock[Type, MVType], block: () => DFAny.Of[Type]
      )(implicit ctx: DFBlock.Context): DFCase_Block[Type, MVType] =
        ctx.compiler.addMember(DFCase_Block[Type, MVType](dfType, matchHeader, prevCase, block, ctx.owner, ctx.meta))
    }
  }
  sealed trait NoRetVal[CB <: NoRetVal[CB]] extends ConditionalBlock[Unit]
//  object NoRetVal {
//    final case class IfBlock(cond : DFBool, block : () => Unit)(
//      implicit val ctx : DFBlock.Context
//    ) extends NoRetVal[IfBlock] {
//      def elsedf[B](block : => Unit)(
//        implicit ctx : DFBlock.Context
//      ) : ElseBlock = ElseBlock(() => block, Left(this))(ctx)
//      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
//        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
//      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), () => block, Left(this))(ctx)
//    }
//    final case class ElseIfBlock(cond : DFBool, block : () => Unit, prevBlock : Either[IfBlock, ElseIfBlock])(
//      implicit val ctx : DFBlock.Context
//    ) extends NoRetVal[IfBlock] {
//      def elsedf[B](block : => Unit)(
//        implicit ctx : DFBlock.Context
//      ) : ElseBlock = ElseBlock(() => block, Right(this))(ctx)
//      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
//        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
//      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), () => block, Right(this))(ctx)
//    }
//    final case class ElseBlock(block : () => Unit, prevBlock : Either[IfBlock, ElseIfBlock])(
//      implicit val ctx : DFBlock.Context
//    ) extends NoRetVal[IfBlock]
//
//    final case class MatchHeader[MVType <: DFAny.Type](
//      matchVal : DFAny.Of[MVType], matchConfig: MatchConfig
//    )(implicit val ctx : DFMember.Context) extends DFMember {
//      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
//        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
//      ) : DFCasePatternBlock[MVType] = new DFCasePatternBlock[MVType](
//        this, None, patternBld(matchVal, pattern), () => block
//      )(ctx)
//    }
//    final case class DFCasePatternBlock[MVType <: DFAny.Type](
//      matchHeader : MatchHeader[MVType],
//      prevCase : Option[DFCasePatternBlock[MVType]], pattern : MVType#TPattern,
//      block : () => Unit
//    )(implicit val ctx : DFBlock.Context) extends NoRetVal[DFCasePatternBlock[MVType]] {
//      def casedf[MC, B](pattern : matchHeader.matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
//        implicit ctx : DFBlock.Context, patternBld : matchHeader.matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
//      ) : DFCasePatternBlock[MVType] = new DFCasePatternBlock[MVType](
//        matchHeader, Some(this), patternBld(matchHeader.matchVal, pattern), () => block
//      )(ctx)
//      def casedf_[MC, B](block : => Unit)(
//        implicit ctx : DFBlock.Context
//      ) : DFCase_Block[MVType] = new DFCase_Block[MVType](
//        matchHeader, this, () => block
//      )(ctx)
//    }
//    final case class DFCase_Block[MVType <: DFAny.Type](
//      matchHeader : MatchHeader[MVType],
//      prevCase : DFCasePatternBlock[MVType], block : () => Unit
//    )(implicit val ctx : DFBlock.Context) extends NoRetVal[DFCase_Block[MVType]]
//  }
}

