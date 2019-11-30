package ZFiant
import DFiant.internals.Meta

trait DFBlock extends DFMember {self =>
  protected implicit def __anyContext(implicit meta0 : Meta) : DFAny.Context = new DFAny.Context {
    val meta: Meta = meta0
    val owner: DFBlock = self
  }
  private[ZFiant] var __injectedOwner : DFBlock = self
  protected implicit def __blockContext(implicit meta0 : Meta) : DFBlock.Context = new DFBlock.Context {
    val meta: Meta = meta0
    val ownerOption : Option[DFBlock] = Some(__injectedOwner)
  }
}

object DFBlock {
  trait Context extends DFMember.Context {
    val meta : Meta
    val ownerOption : Option[DFBlock]
    final lazy val owner : DFBlock = ownerOption.get
  }

}

sealed abstract class ConditionalBlock[CB <: ConditionalBlock[CB, RV], RV](returnType : Option[DFType])(prevBlock : Option[CB], block : => RV) extends DFBlock {
  private val originalOwner : DFBlock = owner.__injectedOwner
  owner.__injectedOwner = this
  protected final val returnValue : RV = block
//  returnVar.foreach(rv => {
//    rv.nameFirst = true
//    rv.assign(returnValue.asInstanceOf[DFAny])(ctx.updateOwner(this))
//  })
  owner.__injectedOwner = originalOwner

}

//trait DFDesign {
//
//}

//case class DFDesign(members : List[DFAnyMember]) extends DFAnyOwner

trait DFInterface