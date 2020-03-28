package ZFiant

import ZFiant.compiler.printer.Printer

sealed trait DFSimMember extends DFMember {
  type TTags = DFMember.Tags.Basic
}
object DFSimMember {
  final case class Assert(condOptionRef : Option[Assert.CondRef], msgRef : Assert.MsgRef, severity : Assert.Severity, ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic) extends DFSimMember {
    protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Assert(condOptionRef, msgRef, severity, _, tags) =>
        val condEq = (this.condOptionRef, condOptionRef) match {
          case (Some(l), Some(r)) => l =~ r
          case (None, None) => true
          case _ => false
        }
        condEq && this.msgRef =~ msgRef && this.severity == severity && this.tags =~ tags
      case _ => false
    }
    def codeString(implicit getSet: MemberGetSet, printConfig : Printer.Config) : String = {
      import printConfig._
      condOptionRef match {
        case Some(c) =>
          s"$DF sim.$DF assert(${c.refCodeString}, ${msgRef.refCodeString}, ${severity.codeString})"
        case None =>
          s"$DF sim.$DF report(${msgRef.refCodeString}, ${severity.codeString})"
      }
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Assert {
    type CondRef = DFMember.OwnedRef.Of[CondRef.Type, DFBool]
    object CondRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
    type MsgRef = DFMember.OwnedRef.Of[MsgRef.Type, DFString[Int]]
    object MsgRef {
      trait Type extends DFAny.Ref.ConsumeFrom.Type
      implicit val ev : Type = new Type {}
    }
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


  final case class Finish(ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic) extends DFSimMember {
    protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Finish(_, tags) => this.tags =~ tags
      case _ => false
    }
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      s"$DF sim.$DF finish()"
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
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
