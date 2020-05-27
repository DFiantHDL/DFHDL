package DFiant
package dfsm

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Step
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[DFiant] sealed trait Step extends Product with Serializable
protected[DFiant] object Step {
  implicit val fsmFromStep : FSM.TC[Step] = s => FSM(Map(), s, s)
  final case class Basic(alwaysBlock : () => Unit)(implicit ctx : DFBlock.Context) extends Step
  final case class DoWhile(cond : () => DFBool, alwaysBlock : () => Unit)(implicit ctx : DFBlock.Context) extends Step

  final case class Owner(
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, tags) => this.tags =~ tags
      case _ => false
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(container : DFOwner.Container)(
      implicit ctx : DFBlock.Context
    ) : Owner = ctx.db.addOwner(container)(Owner(ctx.owner, ctx.meta))
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Edge
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[dfsm] final case class Edge(condOption : Option[() => DFBool], block : () => Unit, dest : Step)
/////////////////////////////////////////////////////////////////////////////////////////////////////////

