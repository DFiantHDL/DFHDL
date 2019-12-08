package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

trait DFBlock extends DFMember with Implicits {self =>
  val ctx : DFBlock.Context
  protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    DFAny.Context(meta, self)
  private[ZFiant] var __injectedOwner : DFBlock = self
  protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    DFBlock.Context(meta, Some(__injectedOwner))
  protected implicit def __designContextOf[T <: DFDesign](implicit meta : Meta) : ContextOf[T] =
    ContextOf[T](meta, Some(__injectedOwner))

  def ifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : ConditionalBlock.NoRetVal.IfBlock = ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(),cond), () => block)(ctx)
  def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] = ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)(ctx)
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  final case class Context(meta : Meta, ownerOption : Option[DFBlock]) extends DFMember.Context {
    lazy val owner : DFBlock = ownerOption.get
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: DFDesign](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.ownerOption)
    implicit def evTop(implicit meta: Meta, topLevel : TopLevel, lp : shapeless.LowPriority) : Context =
      new Context(meta, None)
  }
}



