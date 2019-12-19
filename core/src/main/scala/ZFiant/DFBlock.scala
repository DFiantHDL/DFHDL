package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

trait DFBlock extends DFMember {
//  val ctx : DFBlock.Context
  private[ZFiant] var __injectedOwner : DFBlock = this

  ///////////////////////////////////////////////////////////////////
  // Ownership
  ///////////////////////////////////////////////////////////////////
  val isTop : Boolean = false
  ///////////////////////////////////////////////////////////////////
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  final case class Context(meta : Meta, ownerOption : Option[DFBlock], db : DFDesign.DB.Mutable) extends DFMember.Context {
    lazy val owner : DFBlock = ownerOption.get
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: DFDesign](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.ownerOption, ctx.db)
    implicit def evTop(implicit meta: Meta, topLevel : TopLevel, lp : shapeless.LowPriority) : Context =
      new Context(meta, None, new DFDesign.DB.Mutable)
  }
}



