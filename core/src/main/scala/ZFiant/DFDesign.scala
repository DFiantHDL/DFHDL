package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock {
  final val meta : Meta = ctx.meta
  final val ownerRef: DFRef[DFBlock] = DFRef(ctx.ownerOption.getOrElse(this))
  final private[ZFiant] val __compiler : DFCompiler = ctx.compiler
  final override lazy val topDesign : DFDesign = if (isTop) this else owner.topDesign
  final override val isTop : Boolean = ctx.ownerOption.isEmpty
  final override lazy val fullName : String = if (isTop) name else s"${owner.fullName}.${name}"
  override def toString: String = ctx.meta.name
  __compiler.addMember(this)
}

@implicitNotFound(ContextOf.MissingError.msg)
final case class ContextOf[T <: DFDesign](meta : Meta, ownerOption : Option[DFBlock], compiler: DFCompiler) extends DFMember.Context {
  lazy val owner : DFBlock = ownerOption.get
}
object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtx[T1 <: DFDesign, T2 <: DFDesign](
    implicit ctx : ContextOf[T1], mustBeTheClassOf: MustBeTheClassOf[T1]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.ownerOption, ctx.compiler)
  implicit def evTop[T <: DFDesign](
    implicit meta: Meta, topLevel : TopLevel, mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority
  ) : ContextOf[T] = ContextOf[T](meta, None, new DFCompiler)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context

//  final case class Shell[T <: DFDesign](dsn ) extends DFDesign
}