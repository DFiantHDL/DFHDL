package DFiant

import DFiant.DFDesign.DB
import DFiant.internals.{LateConstructionConfig, Meta}

trait DFInterface extends HasTypeName with DFDesign.Implicits {
  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context
  ///////////////////////////////////////////////////////////////////

}

object DFInterface {
  abstract class Pure()(implicit ctx : DFAny.Context) extends DFInterface {self =>
    private[DFiant] final val owner : Owner = Owner(ctx)
    private[DFiant] final val __db: DFDesign.DB.Mutable = ctx.db
    final protected implicit val __getset : MemberGetSet = ctx.db.getSet
    final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
    final protected implicit def __anyContext(implicit meta0 : Meta) : DFAny.Context =
      new DFAny.Context {
        val meta : Meta = meta0
        def owner : DFOwner = self.owner
        val db : DB.Mutable = self.__db
      }
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

