package ZFiant
import DFiant.internals.Meta

trait DFBlock extends DFMember {self =>
  protected implicit def __anyContext(implicit meta0 : Meta) : DFAny.Context = new DFAny.Context {
    val meta: Meta = meta0
    val owner: DFBlock = self
  }
  protected implicit def __blockContext(implicit meta0 : Meta) : DFBlock.Context = new DFBlock.Context {
    val meta: Meta = meta0
    val ownerOption : Option[DFBlock] = Some(self)
  }
}

object DFBlock {
  trait Context extends DFMember.Context {
    val meta : Meta
    val ownerOption : Option[DFBlock]
    final lazy val owner : DFBlock = ownerOption.get
  }

}


//trait DFDesign {
//
//}

//case class DFDesign(members : List[DFAnyMember]) extends DFAnyOwner

trait DFInterface