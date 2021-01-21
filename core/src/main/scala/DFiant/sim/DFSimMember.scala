package DFiant
package sim

import compiler.csprinter.CSPrinter

sealed trait DFSimMember extends DFMember {
  def codeString(implicit printer: CSPrinter): String
}
object DFSimMember {
  final case class Assert(
      condOptionRef: Option[Assert.CondRef],
      msgRef: Assert.MessageRef,
      severity: Severity,
      ownerRef: DFOwner.Ref,
      tags: DFMember.Tags
  ) extends DFSimMember
      with CanBeGuarded
      with DFAny.CanBeAnonymous {
    protected[DFiant] def =~(
        that: DFMember
    )(implicit getSet: MemberGetSet): Boolean =
      that match {
        case Assert(condOptionRef, msg, severity, _, tags) =>
          val condEq = (this.condOptionRef, condOptionRef) match {
            case (Some(l), Some(r)) => l =~ r
            case (None, None)       => true
            case _                  => false
          }
          condEq && this.msgRef =~ msg && this.severity == severity && this.tags =~ tags
        case _ => false
      }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      condOptionRef match {
        case Some(c) =>
          s"$DF sim.$DF assert(${c.refCodeString}, ${msgRef.refCodeString}, ${severity.codeString})"
        case None =>
          s"$DF sim.$DF report(${msgRef.refCodeString}, ${severity.codeString})"
      }
    }

    def setTags(tagsFunc: DFMember.Tags => DFMember.Tags)(implicit
        getSet: MemberGetSet
    ): DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Assert {
    def apply(
        condOption: Option[DFAny.Member],
        msg: Message,
        severity: Severity
    )(implicit ctx: DFAny.Context): Assert = {
      implicit lazy val ret: Assert with DFMember.RefOwner =
        ctx.db.addMemberOf[Assert](
          Assert(
            condOption.map(e => DFMember.OwnedRef(e)),
            msgRef,
            severity,
            ctx.owner,
            ctx.meta
          )
        )
      lazy val msgRef = MessageRef({
        val eitherSeq: Seq[Either[MsgRef, String]] = msg.seq.map {
          case Left(dfAny) => Left(DFMember.OwnedRef(dfAny))
          case Right(s)    => Right(s)
        }
        eitherSeq
      })
      ret
    }
    object Unref {
      def unapply(arg: Assert)(implicit getSet: MemberGetSet): Option[
        (Option[DFAny.Member], Message, Severity, DFOwner.Ref, DFMember.Tags)
      ] = {
        import arg._
        Some(
          (condOptionRef.map(c => c.get), msgRef.get, severity, ownerRef, tags)
        )
      }
    }

    type CondRef = DFAny.Ref[CondRef.Type]
    object CondRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev: Type = new Type {}
    }
    type MsgRef = DFAny.Ref[MsgRef.Type]
    object MsgRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev: Type = new Type {}
    }
    final case class MessageRef(seq: Seq[Either[MsgRef, String]])
        extends Product
        with Serializable {
      protected[DFiant] def =~(
          that: MessageRef
      )(implicit getSet: MemberGetSet): Boolean = {
        val notEq = (this.seq lazyZip that.seq).exists {
          case (Left(l), Left(r)) if (l.get =~ r.get) => false
          case (Right(l), Right(r)) if l == r         => false
          case _                                      => true
        }
        !notEq
      }
      def get(implicit getSet: MemberGetSet): Message =
        Message(seq.map {
          case Left(ref) => Left(ref.get)
          case Right(s)  => Right(s)
        })
      def refCodeString(implicit printer: CSPrinter, owner: DFOwner): String =
        "msg\"" + seq.collect {
          case Left(x)  => s"$${${x.refCodeString}}"
          case Right(x) => x
        }.mkString + "\""
    }
    final case class Message(seq : Seq[Either[DFAny.Member, String]]) extends Product with Serializable
    object Message {
      implicit val fromDFAny : DFAny => Message = t => msg"$t"
      implicit val fromString : String => Message = t => msg"$t"
    }
  }

  final case class Finish(
      ownerRef: DFOwner.Ref,
      tags: DFMember.Tags
  ) extends DFSimMember
      with CanBeGuarded
      with DFAny.CanBeAnonymous {
    protected[DFiant] def =~(
        that: DFMember
    )(implicit getSet: MemberGetSet): Boolean =
      that match {
        case Finish(_, tags) => this.tags =~ tags
        case _               => false
      }
    def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"$DF sim.$DF finish()"
    }

    def setTags(tagsFunc: DFMember.Tags => DFMember.Tags)(implicit
        getSet: MemberGetSet
    ): DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Finish {
    def apply()(implicit ctx: DFAny.Context): Finish = {
      implicit lazy val ret: Finish with DFMember.RefOwner =
        ctx.db.addMemberOf[Finish](
          Finish(ctx.owner, ctx.meta)
        )
      ret
    }
  }
}
