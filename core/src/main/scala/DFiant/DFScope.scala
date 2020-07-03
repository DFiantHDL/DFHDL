package DFiant

import internals._
class DFScope(val customName : Option[String] = None, val nameFlatten : DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix)(implicit ctx : DFBlock.Context) extends DFOwner.Container {
  type Owner = DFScope.Owner
  final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  private[DFiant] final val __ctx : DFMember.Context = ctx
  private[DFiant] final val owner : Owner = {
    val o = DFScope.Owner(this)(nameFlatten)(ctx)
    val namedOwner = customName match {
      case Some(name) => o.setName(name)
      case None => o
    }
    namedOwner
  }
  protected[DFiant] final implicit val __dir : DFDir = ctx.dir
  protected[DFiant] final implicit lazy val __db : DFDesign.DB.Mutable = ctx.db
}

object DFScope {
  final case class Owner(
    nameFlatten : DFOwner.NameFlatten, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, _, tags) => this.tags =~ tags //Deliberately ignoring nameFlatten. Only the final name (in tags) matters.
      case _ => false
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(container : DFOwner.Container)(nameFlatten: DFOwner.NameFlatten)(
      implicit ctx : DFMember.Context
    ) : Owner = ctx.db.addContainerOwner(container, Owner(nameFlatten, ctx.owner, ctx.meta))
  }
}

object funcdf {
  def apply[R](block : => R)(implicit meta : Meta, ctx : DFBlock.Context) : R = {
    val scope = new DFScope(Some(meta.name)) {
      ctx.db.OwnershipContext.injectContainer(this)
      val ret = block
      ctx.db.OwnershipContext.clearInjectedContainer()
    }
    scope.ret
  }
}