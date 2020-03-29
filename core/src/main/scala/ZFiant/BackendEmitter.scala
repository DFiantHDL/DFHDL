package ZFiant

import ZFiant.compiler.printer.Printer

final case class BackendEmitter(seq : Seq[Either[BackendEmitter.Ref, String]], backendID : String, ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic) extends DFMember {
  type TTags = DFMember.Tags.Basic
  type TCustomTag = DFMember.CustomTag
  override protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
    case BackendEmitter(seq, backendID, _, tags) =>
      val notEq = (this.seq lazyZip seq).exists {
        case (Left(l), Left(r)) if (l.get =~ r.get) => false
        case (Right(l), Right(r)) if l == r => false
        case _ => true
      }
      !notEq && this.backendID == backendID && this.tags =~ tags
    case _ => false
  }
  def codeString(implicit ctx : Printer.Context) : String = backendID + "\"" + seq.collect {
    case Left(x) => s"$${${x.refCodeString}}"
    case Right(x) => x
  }.mkString + "\""
  def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
}
object BackendEmitter {
  type Ref = DFAny.Ref.ConsumeFrom[DFAny]
  def apply(seq : Seq[Either[DFAny, String]], backendID : String)(
    implicit ctx : DFAny.Context
  ) : BackendEmitter = {
    implicit lazy val ret : BackendEmitter with DFMember.RefOwner =
      ctx.db.addMember(BackendEmitter(refSeq, backendID, ctx.owner, ctx.meta)).asRefOwner
    lazy val refSeq : Seq[Either[Ref, String]] = seq.map {
      case Left(dfAny) => Left(DFMember.OwnedRef(dfAny))
      case Right(s) => Right(s)
    }
    ret
  }

}
