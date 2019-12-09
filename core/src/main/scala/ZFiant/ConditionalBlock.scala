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

sealed trait ConditionalBlock[CB <: ConditionalBlock[CB, Ret], Ret] extends DFBlock {
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
  sealed trait WithRetVal[CB <: WithRetVal[CB, Type], Type <: DFAny.Type] extends ConditionalBlock[WithRetVal[CB, Type], DFAny.Of[Type]] with DFAny.ValOrVar[Type, false]
  object WithRetVal {
    final case class IfBlock[Type <: DFAny.Type](dfType : Type, cond : DFBool, block : () => DFAny.Of[Type])(
      implicit val ctx : DFBlock.Context
    ) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](dfType, () => blockConv(dfType, block), Left(this))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, condConv(DFBool.Type(), cond), () => blockConv(dfType, block), Left(this))(ctx)
    }
    final case class ElseIfBlock[Type <: DFAny.Type](
      dfType : Type, cond : DFBool, block : () => DFAny.Of[Type], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]]
    )(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf[B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseBlock[Type] = ElseBlock[Type](dfType, () => blockConv(dfType, block), Right(this))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
      ) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, condConv(DFBool.Type(), cond), () => blockConv(dfType, block), Right(this))(ctx)
    }
    final case class ElseBlock[Type <: DFAny.Type](
      dfType : Type, block : () => DFAny.Of[Type], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]]
    )(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type]

    final case class MatchHeader[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchVal : DFAny.Of[MVType], matchConfig: MatchConfig
    )(implicit val ctx : DFMember.Context) extends DFMemberNotAnOwner {
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = new DFCasePatternBlock[Type, MVType](
        dfType, this, None, patternBld(matchVal, pattern), () => retBld(dfType, block)
      )(ctx)
    }
    final case class DFCasePatternBlock[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchHeader : MatchHeader[Type, MVType],
      prevCase : Option[DFCasePatternBlock[Type, MVType]], pattern : MVType#TPattern,
      block : () => DFAny.Of[Type]
    )(implicit val ctx : DFBlock.Context) extends WithRetVal[DFCasePatternBlock[Type, MVType], Type] {
      def casedf[MC, B](pattern : matchHeader.matchVal.dfType.TPatternAble[MC]*)(block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, patternBld : matchHeader.matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]], retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCasePatternBlock[Type, MVType] = new DFCasePatternBlock[Type, MVType](
        dfType, matchHeader, Some(this), patternBld(matchHeader.matchVal, pattern), () => retBld(dfType, block)
      )(ctx)
      def casedf_[MC, B](block : => dfType.OpAble[B])(
        implicit ctx : DFBlock.Context, retBld : dfType.`Op:=Builder`[Type, B]
      ) : DFCase_Block[Type, MVType] = new DFCase_Block[Type, MVType](
        dfType, matchHeader, this, () => retBld(dfType, block)
      )(ctx)
    }
    final case class DFCase_Block[Type <: DFAny.Type, MVType <: DFAny.Type](
      dfType : Type, matchHeader : MatchHeader[Type, MVType],
      prevCase : DFCasePatternBlock[Type, MVType], block : () => DFAny.Of[Type]
    )(implicit val ctx : DFBlock.Context) extends WithRetVal[DFCase_Block[Type, MVType], Type]
  }
  sealed trait NoRetVal[CB <: NoRetVal[CB]] extends ConditionalBlock[NoRetVal[CB], Unit]
  object NoRetVal {
    final case class IfBlock(cond : DFBool, block : () => Unit)(
      implicit val ctx : DFBlock.Context
    ) extends NoRetVal[IfBlock] {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(() => block, Left(this))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), () => block, Left(this))(ctx)
    }
    final case class ElseIfBlock(cond : DFBool, block : () => Unit, prevBlock : Either[IfBlock, ElseIfBlock])(
      implicit val ctx : DFBlock.Context
    ) extends NoRetVal[IfBlock] {
      def elsedf[B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : ElseBlock = ElseBlock(() => block, Right(this))(ctx)
      def elseifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
        implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
      ) : ElseIfBlock = ElseIfBlock(condConv(DFBool.Type(), cond), () => block, Right(this))(ctx)
    }
    final case class ElseBlock(block : () => Unit, prevBlock : Either[IfBlock, ElseIfBlock])(
      implicit val ctx : DFBlock.Context
    ) extends NoRetVal[IfBlock]

    final case class MatchHeader[MVType <: DFAny.Type](
      matchVal : DFAny.Of[MVType], matchConfig: MatchConfig
    )(implicit val ctx : DFMember.Context) extends DFMemberNotAnOwner {
      def casedf[MC, B](pattern : matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
      ) : DFCasePatternBlock[MVType] = new DFCasePatternBlock[MVType](
        this, None, patternBld(matchVal, pattern), () => block
      )(ctx)
    }
    final case class DFCasePatternBlock[MVType <: DFAny.Type](
      matchHeader : MatchHeader[MVType],
      prevCase : Option[DFCasePatternBlock[MVType]], pattern : MVType#TPattern,
      block : () => Unit
    )(implicit val ctx : DFBlock.Context) extends NoRetVal[DFCasePatternBlock[MVType]] {
      def casedf[MC, B](pattern : matchHeader.matchVal.dfType.TPatternAble[MC]*)(block : => Unit)(
        implicit ctx : DFBlock.Context, patternBld : matchHeader.matchVal.dfType.TPatternBuilder[DFAny.Of[MVType]]
      ) : DFCasePatternBlock[MVType] = new DFCasePatternBlock[MVType](
        matchHeader, Some(this), patternBld(matchHeader.matchVal, pattern), () => block
      )(ctx)
      def casedf_[MC, B](block : => Unit)(
        implicit ctx : DFBlock.Context
      ) : DFCase_Block[MVType] = new DFCase_Block[MVType](
        matchHeader, this, () => block
      )(ctx)
    }
    final case class DFCase_Block[MVType <: DFAny.Type](
      matchHeader : MatchHeader[MVType],
      prevCase : DFCasePatternBlock[MVType], block : () => Unit
    )(implicit val ctx : DFBlock.Context) extends NoRetVal[DFCase_Block[MVType]]
  }
}

