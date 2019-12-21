package ZFiant
import DFiant.internals._

sealed abstract class DFNet(op : String) extends DFMember {
  val toRef : DFRef[DFAny]
  val fromRef : DFRef[DFAny]
  def codeString(implicit getset : MemberGetSet) : String = s"${toRef.refCodeString} $op ${fromRef.refCodeString}"
  override def show(implicit getset : MemberGetSet) : String = codeString
}

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(toRef : DFRef[DFAny], fromRef : DFRef[DFAny], ownerRef: DFRef[DFBlock], meta: Meta) extends DFNet(":=") {
    def setMeta(meta : Meta)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(meta = meta))
  }
  object Assignment {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Assignment = ctx.db.addMember(Assignment(to, from, ctx.owner, ctx.meta))
  }

  final case class Connection(toRef : DFRef[DFAny], fromRef : DFRef[DFAny], ownerRef: DFRef[DFBlock], meta: Meta) extends DFNet("<>") {
    def setMeta(meta : Meta)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(meta = meta))
  }
  object Connection {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Connection = ctx.db.addMember(Connection(to, from, ctx.owner, ctx.meta))
  }
}
