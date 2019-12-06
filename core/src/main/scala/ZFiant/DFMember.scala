package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ctx : DFMember.Context
  lazy val owner : DFBlock = ctx.owner
}
object DFMember {
  trait Context {
    val meta : Meta
    val owner : DFBlock
  }
}

