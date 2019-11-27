package ZFiant

trait DFAnyOwner extends DFAnyMember

//trait DFDesign {
//
//}

case class DFDesign(members : List[DFAnyMember]) extends DFAnyOwner

trait DFInterface