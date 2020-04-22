package DFiant

trait DFOwner extends DFMember {
  def connectWith(that : DFOwner)(implicit ctx : DFBlock.Context) : Unit = {
    val leftMembers = this.getMembers
    val rightMembers = that.getMembers
    (leftMembers lazyZip rightMembers).foreach {
      case (left : DFAny.Dcl, right : DFAny.Dcl) if left.dfType == right.dfType => left.connectWith(right)
      case (left : DFInterface.Owner, right : DFInterface.Owner) => left.connectWith(right)
      case (left : DFAny.Dcl, right : DFAny.Dcl) if left.dfType != right.dfType =>
        throw new IllegalArgumentException("mismatch type connection")
      case (left : DFAny.Dcl, right) =>
        throw new IllegalArgumentException("mismatch type connection")
      case (_, _ : DFAny.Dcl) =>
        throw new IllegalArgumentException("mismatch type connection")
      case _ =>
    }
  }

}

object DFOwner {
  implicit class AbstractExt[T <: DFOwner](t : T) {
    def getMembers(implicit getSet: MemberGetSet) : List[DFMember] = getSet.getMembersOf(t)
    //    def <> (r : T)(implicit ctx : DFNet.Context) : Unit = t.owner.connectWith(r.owner)
  }

  type Ref = DFMember.Ref.Of[Ref.Type, DFOwner]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
    def unapply(ref : DFMember.Ref): Boolean = ref.refType match {
      case _ : Type => true
      case _ => false
    }
  }
}