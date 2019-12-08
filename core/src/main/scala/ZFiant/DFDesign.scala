package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

abstract class DFDesign(implicit val ctx : DFDesign.Context) extends DFBlock {
  final val ownerOption : Option[DFBlock] = ctx.ownerOption
  final override val topDesign : DFDesign = ownerOption match {
    case Some(o) => o.topDesign
    case None  => this
  }
  final override val isTop : Boolean = ownerOption.isEmpty
  override def toString: String = ctx.meta.name
}

@implicitNotFound(ContextOf.MissingError.msg)
final case class ContextOf[T <: DFDesign](meta : Meta, ownerOption : Option[DFBlock]) extends DFMember.Context {
  lazy val owner : DFBlock = ownerOption.get
}
object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtx[T1 <: DFDesign, T2 <: DFDesign](
    implicit ctx : ContextOf[T1], mustBeTheClassOf: MustBeTheClassOf[T1]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.ownerOption)
  implicit def evTop[T <: DFDesign](
    implicit meta: Meta, topLevel : TopLevel, mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority
  ) : ContextOf[T] = ContextOf[T](meta, None)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context
}