package ZFiant
import DFiant.internals._

sealed abstract class DFNet(op : String) extends DFMember {
  val toRef : DFMember.Ref[DFAny]
  val fromRef : DFMember.Ref[DFAny]
  def codeString(implicit getset : MemberGetSet) : String = s"${toRef.refCodeString} $op ${fromRef.refCodeString}"
  override def show(implicit getset : MemberGetSet) : String = codeString
}

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(toRef : DFMember.Ref[DFAny], fromRef : DFMember.Ref[DFAny], ownerRef : DFMember.Ref[DFBlock], tags : DFMember.Tags) extends DFNet(":=") {
    def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
  }
  object Assignment {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Assignment = ctx.db.addMember(Assignment(to, from, ctx.owner, ctx.meta))
  }

  final case class Connection(toRef : DFMember.Ref[DFAny], fromRef : DFMember.Ref[DFAny], ownerRef : DFMember.Ref[DFBlock], tags : DFMember.Tags) extends DFNet("<>") {
    def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
  }
  object Connection {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Connection = ctx.db.addMember(Connection(to, from, ctx.owner, ctx.meta))
  }
}
