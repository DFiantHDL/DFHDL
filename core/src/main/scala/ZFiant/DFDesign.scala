package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

abstract class DFDesign(implicit val ctx : DFDesign.Context) extends DFBlock {
  override lazy val owner : DFBlock = ctx.ownerOption.getOrElse(this)
  final override lazy val topDesign : DFDesign = if (isTop) this else owner.topDesign
  final override lazy val id : Int = if (isTop) 0 else owner.addMember(this)
  final override val isTop : Boolean = ctx.ownerOption.isEmpty
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

//  final case class Shell[T <: DFDesign](dsn ) extends DFDesign
}