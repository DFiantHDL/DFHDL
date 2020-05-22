package DFiant

import DFiant.compiler.printer.Printer

sealed trait DFSimMember extends DFMember {
  type TTags = DFMember.Tags.Basic
  def codeString(implicit getSet: MemberGetSet, printConfig : Printer.Config) : String
}
object DFSimMember {
  final case class Assert(
    condOptionRef : Option[Assert.CondRef], msgRef : Assert.MessageRef, severity : Assert.Severity,
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFSimMember  with CanBeGuarded with DFAny.CanBeAnonymous {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Assert(condOptionRef, msg, severity, _, tags) =>
        val condEq = (this.condOptionRef, condOptionRef) match {
          case (Some(l), Some(r)) => l =~ r
          case (None, None) => true
          case _ => false
        }
        condEq && this.msgRef =~ msg && this.severity == severity && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit getSet: MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      condOptionRef match {
        case Some(c) =>
          s"$DF sim.$DF assert(${c.refCodeString}, ${msgRef.codeString}, ${severity.codeString})"
        case None =>
          s"$DF sim.$DF report(${msgRef.codeString}, ${severity.codeString})"
      }
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Assert {
    def apply(condOption : Option[DFBool], msg : Message, severity : Severity)(implicit ctx: DFAny.Context)
    : Assert = {
      implicit lazy val ret : Assert with DFMember.RefOwner =
        ctx.db.addMember(Assert(condOption.map(e => DFMember.OwnedRef(e)), msgRef, severity, ctx.owner, ctx.meta)).asRefOwner
      lazy val msgRef = MessageRef({
        val eitherSeq : Seq[Either[MsgRef, String]] = msg.seq.map {
          case Left(dfAny) => Left(DFMember.OwnedRef(dfAny))
          case Right(s) => Right(s)
        }
        eitherSeq
      })
      ret
    }
    object Unref {
      def unapply(arg: Assert)(implicit getSet: MemberGetSet)
      : Option[(Option[DFBool], Message, Severity, DFOwner.Ref, DFMember.Tags.Basic)] = {
        import arg._
        Some((condOptionRef.map(c => c.get), msgRef.get, severity, ownerRef, tags))
      }
    }

    type CondRef = DFMember.OwnedRef.Of[CondRef.Type, DFBool]
    object CondRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    type MsgRef = DFMember.OwnedRef.Of[MsgRef.Type, DFAny]
    object MsgRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    final case class MessageRef(seq : Seq[Either[MsgRef, String]]) extends Product with Serializable {
      protected[DFiant] def =~(that : MessageRef)(implicit getSet : MemberGetSet) : Boolean = {
        val notEq = (this.seq lazyZip that.seq).exists {
          case (Left(l), Left(r)) if (l.get =~ r.get) => false
          case (Right(l), Right(r)) if l == r => false
          case _ => true
        }
        !notEq
      }
      def get(implicit getSet : MemberGetSet) : Message = Message(seq.map {
        case Left(ref) => Left(ref.get)
        case Right(s) => Right(s)
      })
      def codeString(implicit ctx : Printer.Context) : String = "msg\"" + seq.collect {
        case Left(x) => s"$${${x.refCodeString}}"
        case Right(x) => x
      }.mkString + "\""
    }
    final case class Message(seq : Seq[Either[DFAny, String]]) extends Product with Serializable
    sealed trait Severity extends Product with Serializable {
      def codeString(implicit printConfig : Printer.Config) : String = {
        import printConfig._
        s"$DF sim.$DF ${this.toString}"
      }
    }
    object Severity {
      case object Note extends Severity
      case object Warning extends Severity
      case object Error extends Severity
      case object Failure extends Severity
    }
  }

  final case class Finish(
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFSimMember with CanBeGuarded with DFAny.CanBeAnonymous {
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Finish(_, tags) => this.tags =~ tags
      case _ => false
    }
    def codeString(implicit getSet: MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      s"$DF sim.$DF finish()"
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Finish {
    def apply()(implicit ctx: DFAny.Context)
    : Finish = {
      implicit lazy val ret : Finish with DFMember.RefOwner =
        ctx.db.addMember(Finish(ctx.owner, ctx.meta)).asRefOwner
      ret
    }
  }
}
