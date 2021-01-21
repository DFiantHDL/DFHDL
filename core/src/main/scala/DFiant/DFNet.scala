package DFiant
import DFiant.DFNet.Op
import compiler.csprinter.CSPrinter
import DFiant.internals._
import compiler.printer.formatter._

final case class DFNet(
  toRef : DFNet.ToRef, op : DFNet.Op, fromRef : DFNet.FromRef, ownerRef : DFOwner.Ref, tags : DFMember.Tags
) extends DFAny.CanBeAnonymous with CanBeGuarded {
  protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
    case DFNet(toRef, op, fromRef, _, tags) =>
      this.toRef =~ toRef && this.op == op && this.fromRef =~ fromRef && this.tags =~ tags
    case _ => false
  }

  def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  def codeString(implicit printer: CSPrinter) : String = {
    import printer.config._
    val toRefString = toRef.refCodeString
    val fromRefString = fromRef.refCodeString
    val opString = s"${ALGN(0)}$DF$op"
    if(isConnection && hasLateConstruction) {
      if (toRef.getOwner == this.getOwner) s"$toRefString $opString$CMT/*<--*/ $fromRefString"
      else s"$fromRefString $opString$CMT/*-->*/ $toRefString"
    }
    else s"$toRefString $opString $fromRefString"
  }
  def isAssignment : Boolean = op match {
    case Op.Assignment => true
    case _ => false
  }
  def isConnection : Boolean = op match {
    case Op.Connection | Op.LazyConnection => true
    case _ => false
  }
  def isLazyConnection : Boolean = op match {
    case Op.LazyConnection => true
    case _ => false
  }
  override def show(implicit printer: CSPrinter) : String = codeString
}

object DFNet {
  type Context = DFAny.Context
  sealed trait Op extends Product with Serializable
  object Op {
    case object Assignment extends Op {
      override def toString : String = ":="
    }
    case object Connection extends Op {
      override def toString : String = "<>"
    }
    case object LazyConnection extends Op {
      override def toString : String = "`<LZ>`"
    }
  }
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

  def apply(to: DFAny.Member, op : Op, from: DFAny.Member)(implicit ctx: Context)
  : DFNet = {
    implicit lazy val ret : DFNet with DFMember.RefOwner =
      ctx.db.addMemberOf[DFNet](DFNet(to, op, from, ctx.owner, ctx.meta))
    ret
  }

  object Assignment {
    def apply(to: DFAny.Member, from: DFAny.Member)(implicit ctx: Context)
    : DFNet = DFNet(to, Op.Assignment, from)

    def unapply(arg : DFNet)(
      implicit getSet: MemberGetSet
    ) : Option[(DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
      case DFNet(toRef, Op.Assignment, fromRef, ownerRef, tags) => Some(toRef.get, fromRef.get, ownerRef.get, tags)
      case _ => None
    }
  }

  object Connection {
    def apply(to: DFAny.Member, from: DFAny.Member)(implicit ctx: Context)
    : DFNet = DFNet(to, Op.Connection, from)

    def unapply(arg : DFNet)(
      implicit getSet: MemberGetSet
    ) : Option[(DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
      case DFNet(toRef, Op.Connection | Op.LazyConnection, fromRef, ownerRef, tags) =>
        Some(toRef.get, fromRef.get, ownerRef.get, tags)
      case _ => None
    }
  }

  object LazyConnection {
    def apply(to: DFAny.Member, from: DFAny.Member)(implicit ctx: Context)
    : DFNet = DFNet(to, Op.LazyConnection, from)

    def unapply(arg : DFNet)(
      implicit getSet: MemberGetSet
    ) : Option[(DFAny.Member, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
      case DFNet(toRef, Op.LazyConnection, fromRef, ownerRef, tags) =>
        Some(toRef.get, fromRef.get, ownerRef.get, tags)
      case _ => None
    }
  }

  object Unref {
    def unapply(arg : DFNet)(
      implicit getSet : MemberGetSet
    ) : Option[(DFAny.Member, Op, DFAny.Member, DFOwner, DFMember.Tags)] = arg match {
      case DFNet(toRef, op, fromRef, ownerRef, tags) =>
        Some(toRef.get, op, fromRef.get, ownerRef.get, tags)
      case _ => None
    }
  }

  object Inlined {
    def unapply(arg : DFNet)(implicit getSet : MemberGetSet) : Boolean = arg match {
      case net : DFNet if net.isConnection => net.toRef.get.getOwnerBlock match {
        case DFDesign.Block.Internal(_,_,_,Some(_)) => true
        case _ => false
      }
      case _ => false
    }
  }
}
