package DFiant

import DFiant.DFDesign.DB
import DFiant.internals._

import scala.annotation.implicitNotFound

trait DFInterface extends HasTypeName with DFDesign.Implicits {
  type Owner <: DFOwner
  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context
  ///////////////////////////////////////////////////////////////////

}

object DFInterface {
  abstract class Pure()(implicit ctx : Context) extends DFInterface {self =>
    type Owner = DFOwner
    private[DFiant] final val owner : Owner = Owner(ctx)
    private[DFiant] final val __db: DFDesign.DB.Mutable = ctx.db
    final protected implicit val __getset : MemberGetSet = ctx.db.getSet
    final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
    final protected implicit def __anyContext(implicit meta : Meta) : Context =
      new Context(meta, owner, __db)
    final protected implicit def __contextOf[T <: Pure](implicit meta : Meta) : ContextOf[T] =
      new ContextOf[T](meta, owner, __db)
  }
  @implicitNotFound(Context.MissingError.msg)
  final class Context(val meta : Meta, ownerF : => DFOwner, val db : DFDesign.DB.Mutable)
    extends DFAny.Context {
    def owner : DFOwner = ownerF
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: Pure](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.owner, ctx.db)
    implicit def evBlockCtx(implicit ctx : DFBlock.Context) : Context =
      new Context(ctx.meta, ctx.owner, ctx.db)
  }

  final case class Owner(ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends DFOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, tags) => this.tags =~ tags
      case _ => false
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(
      implicit ctx : DFAny.Context
    ) : Owner = ctx.db.addMember(Owner(ctx.owner, ctx.meta))
  }
}

