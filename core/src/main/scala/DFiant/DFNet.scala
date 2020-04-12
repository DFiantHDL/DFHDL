package DFiant
import DFiant.internals._
import DFiant.compiler.printer.Printer
sealed abstract class DFNet(op : String) extends DFAny.CanBeAnonymous {
  type TTags = DFMember.Tags.Basic
  type TCustomTag = DFMember.CustomTag
  val toRef : DFNet.ToRef
  val fromRef : DFNet.FromRef
  def codeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
    import printConfig._
    import formatter._
    s"${toRef.refCodeString} ${ALGN(0)}$DF$op ${fromRef.refCodeString}"
  }
  override def show(implicit getSet : MemberGetSet) : String = codeString
}

object DFNet {
  type Context = DFBlock.Context

  type ToRef = DFMember.OwnedRef.Of[ToRef.Type, DFAny]
  object ToRef {
    trait Type extends DFAny.Ref.ProduceTo.Type
    implicit val ev : Type = new Type {}
    def unapply(ref : DFMember.Ref): Boolean = ref.refType match {
      case _ : Type => true
      case _ => false
    }
  }
  type FromRef = DFMember.OwnedRef.Of[FromRef.Type, DFAny]
  object FromRef {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  final case class Assignment(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends DFNet(":=") with CanBeGuarded {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Assignment(toRef, fromRef, _, tags) =>
        this.toRef =~ toRef && this.fromRef =~ fromRef && this.tags =~ tags
      case _ => false
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Assignment {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Assignment = {
      implicit lazy val ret : Assignment with DFMember.RefOwner =
        ctx.db.addMember(Assignment(to, from, ctx.owner, ctx.meta)).asRefOwner
      ret
    }
    object Unref {
      def unapply(arg : Assignment)(implicit getSet: MemberGetSet) : Option[(DFAny, DFAny, DFOwner, DFMember.Tags.Basic)] = arg match {
        case Assignment(toRef, fromRef, ownerRef, tags) => Some(toRef.get, fromRef.get, ownerRef.get, tags)
        case _ => None
      }
    }
  }

  final case class Connection(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends DFNet("<>") {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Connection(toRef, fromRef, _, tags) =>
        this.toRef =~ toRef && this.fromRef =~ fromRef && this.tags =~ tags
      case _ => false
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Connection {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Connection = {
      implicit lazy val ret : Connection with DFMember.RefOwner =
        ctx.db.addMember(Connection(to, from, ctx.owner, ctx.meta)).asRefOwner
      ret
    }
  }

  object Inlined {
    def unapply(arg : DFNet)(implicit getSet : MemberGetSet) : Boolean = arg match {
      case net : DFNet.Connection => net.toRef.get.getOwnerBlock match {
        case DFDesign.Block.Internal(_,_,_,Some(_)) => true
        case _ => false
      }
      case _ => false
    }
  }
}
