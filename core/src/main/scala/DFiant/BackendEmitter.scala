package DFiant

import DFiant.DFAny.CanBeAnonymous
import DFiant.csprinter.CSPrinter
import compiler.backend.Backend

final case class BackendEmitter(
  seq : Seq[Either[BackendEmitter.Ref, String]], backendStage : Backend.Stage, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
) extends CanBeGuarded with CanBeAnonymous {
  type TTags = DFMember.Tags.Basic
  type TCustomTag = DFMember.CustomTag
  override protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
    case BackendEmitter(seq, backendStage, _, tags) =>
      val notEq = (this.seq lazyZip seq).exists {
        case (Left(l), Left(r)) if (l.get =~ r.get) => false
        case (Right(l), Right(r)) if l == r => false
        case _ => true
      }
      !notEq && this.backendStage == backendStage && this.tags =~ tags
    case _ => false
  }
  def codeString(implicit printer: CSPrinter) : String = backendStage.codeString + "\"" + seq.collect {
    case Left(x) => s"$${${x.refCodeString}}"
    case Right(x) => x
  }.mkString + "\""
  private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
  def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
    implicit getSet : MemberGetSet
  ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
}
object BackendEmitter {
  type Ref = DFAny.Ref.ConsumeFrom[DFAny]
  def apply(seq : Seq[Either[DFAny, String]], backendStage : Backend.Stage)(
    implicit ctx : DFAny.Context
  ) : BackendEmitter = {
    implicit lazy val ret : BackendEmitter with DFMember.RefOwner =
      ctx.db.addMember(ctx.container, BackendEmitter(refSeq, backendStage, ctx.owner, ctx.meta)).asRefOwner
    lazy val refSeq : Seq[Either[Ref, String]] = seq.map {
      case Left(dfAny) => Left(DFMember.OwnedRef(dfAny))
      case Right(s) => Right(s)
    }
    ret
  }
}
