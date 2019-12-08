package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ctx : DFMember.Context
  lazy val owner : DFBlock = ctx.owner
  lazy val id = owner.addMember(this)
  override def toString: String = ctx.meta.name
}

trait DFMemberNotAnOwner extends DFMember {
  id //touch to trigger addition to owner
}

object DFMember {
  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
  }
}

