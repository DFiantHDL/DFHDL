package DFiant
import compiler.csprinter.CSPrinter
import DFiant.internals._
import compiler.printer.formatter._

sealed abstract class DFNet(op : String) extends DFAny.CanBeAnonymous {
  val toRef : DFNet.ToRef
  val fromRef : DFNet.FromRef
  def codeString(implicit printer: CSPrinter) : String = {
    import printer.config._
    val toRefString = toRef.refCodeString
    val fromRefString = fromRef.refCodeString
    val opString = s"${ALGN(0)}$DF$op"
    this match {
      case _ : DFNet.Connection if hasLateConstruction =>
        if (toRef.getOwner == this.getOwner) s"$toRefString $opString$CMT/*<--*/ $fromRefString"
        else s"$fromRefString $opString$CMT/*-->*/ $toRefString"
      case _ => s"$toRefString $opString $fromRefString"
    }
  }
  override def show(implicit printer: CSPrinter) : String = codeString
}

object DFNet {
  type Context = DFAny.Context

  type ToRef = DFMember.OwnedRef.Of[ToRef.Type, DFAny.Member]
  object ToRef {
    trait Type extends DFAny.Ref.ProduceTo.Type
    implicit val ev : Type = new Type {}
    def unapply(ref : DFMember.Ref): Boolean = ref.refType match {
      case _ : Type => true
      case _ => false
    }
  }
  type FromRef = DFMember.OwnedRef.Of[FromRef.Type, DFAny.Member]
  object FromRef {
    trait Type extends DFAny.Ref.ConsumeFrom.Type
    implicit val ev : Type = new Type {}
  }

  final case class Assignment(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags) extends DFNet(":=") with CanBeGuarded {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Assignment(toRef, fromRef, _, tags) =>
        this.toRef =~ toRef && this.fromRef =~ fromRef && this.tags =~ tags
      case _ => false
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Assignment {
    def apply(to: DFAny.Member, from: DFAny.Member)(implicit ctx: Context)
    : Assignment = {
      implicit lazy val ret : Assignment with DFMember.RefOwner =
        ctx.db.addMemberOf[Assignment](Assignment(to, from, ctx.owner, ctx.meta))
      ret
    }
    object Unref {
      def unapply(arg : Assignment)(implicit getSet: MemberGetSet) : Option[(DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Assignment(toRef, fromRef, ownerRef, tags) => Some(toRef.get, fromRef.get, ownerRef.get, tags)
        case _ => None
      }
    }
  }

  final case class Connection(toRef : DFNet.ToRef, fromRef : DFNet.FromRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags) extends DFNet("<>") {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Connection(toRef, fromRef, _, tags) =>
        this.toRef =~ toRef && this.fromRef =~ fromRef && this.tags =~ tags
      case _ => false
    }

    def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Connection {
    def apply(to: DFAny.Member, from: DFAny.Member)(implicit ctx: Context)
    : Connection = {
      implicit lazy val ret : Connection with DFMember.RefOwner =
        ctx.db.addMemberOf[Connection](Connection(to, from, ctx.owner, ctx.meta))
      ret
    }
    object Unref {
      def unapply(arg : Connection)(implicit getSet: MemberGetSet) : Option[(DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
        case Connection(toRef, fromRef, ownerRef, tags) => Some(toRef.get, fromRef.get, ownerRef.get, tags)
        case _ => None
      }
    }
  }
  case object LazyConnection extends DFMember.CustomTagOf[Connection] {
    def unapply(net : Connection)(implicit getSet: MemberGetSet) : Boolean = net.getTagOf[LazyConnection.type].nonEmpty
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
