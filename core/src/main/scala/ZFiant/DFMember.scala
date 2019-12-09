package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ctx : DFMember.Context
  lazy val owner : DFBlock = ctx.owner
  lazy val id = owner.addMember(this)
  override def toString: String = ctx.meta.name
}

object DFMember {
  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
  }
}

