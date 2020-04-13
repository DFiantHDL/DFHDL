package DFiant

import DFiant.DFDesign.DB
import DFiant.internals._
import singleton.ops._

import scala.annotation.implicitNotFound

abstract class DFInterface(namePrefix : String = "", nameSuffix : String = "_")(implicit ctx : DFInterface.Context)
  extends DFInterface.Abstract {
  type Owner = DFOwner
  private[DFiant] final val owner : Owner = DFInterface.Owner(namePrefix, nameSuffix)(ctx)
  private[DFiant] final val __db: DFDesign.DB.Mutable = ctx.db
  final protected implicit val __getset : MemberGetSet = ctx.db.getSet
  final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  final protected implicit def __anyContext(implicit meta : Meta) : DFInterface.Context =
    new DFInterface.Context(meta, owner, __db)
  final protected implicit def __contextOf[T <: DFInterface](implicit meta : Meta) : ContextOf[T] =
    new ContextOf[T](meta, owner, __db)
}

object DFInterface {
  trait Abstract extends HasTypeName with DFDesign.Implicits {
    type Owner <: DFOwner
    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context
    ///////////////////////////////////////////////////////////////////

  }
//  implicit class AbstractExt[T <: DFInterface](t : T) {
//    def <> (r : T)(implicit ctx : DFNet.Context) : Unit = t.owner.connectWith(r.owner)
//  }
  protected[DFiant] class Context(val meta : Meta, ownerF : => DFOwner, val db : DFDesign.DB.Mutable)
    extends DFAny.Context {
    def owner : DFOwner = ownerF
  }
  protected[DFiant] object Context {
    final object MissingError extends ErrorMsg (
      "The given context type `T` in `ContextOf[T]` is wrong",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: DFInterface](
      implicit
      ctx : ContextOf[T],
      mustBeTheClassOf: RequireMsg[ImplicitFound[MustBeTheClassOf[T]], MissingError.Msg]
    ) : Context = new Context(ctx.meta, ctx.owner, ctx.db)
  }

  final case class Owner(namePrefix : String, nameSuffix : String, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends DFOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(namePrefix, nameSuffix, _, tags) =>
        this.namePrefix == namePrefix && this.nameSuffix == nameSuffix && this.tags =~ tags
      case _ => false
    }
    def connectWith(that : Owner)(implicit ctx : DFNet.Context) : Unit = {}
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(namePrefix : String, nameSuffix : String)(
      implicit ctx : DFAny.Context
    ) : Owner = ctx.db.addMember(Owner(namePrefix, nameSuffix, ctx.owner, ctx.meta))
  }
}

