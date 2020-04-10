package DFiant

trait DFOwner extends DFMember {

}

object DFOwner {
  type Ref = DFMember.Ref.Of[Ref.Type, DFOwner]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
  }
}