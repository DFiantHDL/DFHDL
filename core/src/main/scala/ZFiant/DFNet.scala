package ZFiant
import DFiant.internals._

sealed abstract class DFNet(op : String) extends DFMember {
  val toRef : DFNet.ToRef
  val fromRef : DFNet.FromRef
  def codeString(implicit getset : MemberGetSet) : String = s"${toRef.refCodeString} $op ${fromRef.refCodeString}"
  override def show(implicit getset : MemberGetSet) : String = codeString
}

object DFNet {
  type Context = DFAny.Context

  class ToRef extends DFAny.Ref[DFAny]
  object ToRef extends DFAny.Ref.CO[DFAny, ToRef](new ToRef)
  class FromRef extends DFAny.Ref[DFAny]
  object FromRef extends DFAny.Ref.CO[DFAny, FromRef](new FromRef)

  final case class Assignment(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFBlock.Ref, tags : DFMember.Tags) extends DFNet(":=") {
    def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
  }
  object Assignment {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Assignment = ctx.db.addMember(Assignment(to, from, ctx.owner, ctx.meta))
  }

  final case class Connection(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFBlock.Ref, tags : DFMember.Tags) extends DFNet("<>") {
    def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags))
  }
  object Connection {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Connection = ctx.db.addMember(Connection(to, from, ctx.owner, ctx.meta))
  }
}
