package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ownerRef : DFRef[DFBlock]
  val meta : Meta
  final val owner : DFBlock = ownerRef
  final val ownerDesign : DFDesign = owner match {
    case d : DFDesign => d
    case b : DFBlock => b.ownerDesign
  }
  val name : String = meta.name
  val fullName : String = if (owner.isTop) s"${owner.name}.${name}" else s"${owner.fullName}.${name}"
  final private[ZFiant] def getOwnerChain : List[DFBlock] = if (owner.isTop) List(owner) else owner.getOwnerChain :+ owner
  def getRelativeName(implicit ctx : DFMember.Context) : String = {
    if (this isSameOwnerDesignAs ctx.owner) name
    else if (this isOneLevelBelow ctx.owner) s"${owner.name}.$name"
    else {
      //more complex referencing just summons the two owner chains and compares them.
      //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
      val memberChain = this.getOwnerChain
      val ctxChain = ctx.owner.getOwnerChain
      ??? //TODO
    }
  }

  final def isSameOwnerDesignAs(that : DFMember) : Boolean = ownerDesign == that.ownerDesign
  final def isOneLevelBelow(that : DFMember) : Boolean = ownerDesign isSameOwnerDesignAs that

  //  final def isDownstreamMemberOf(that : DFBlock) : Boolean = {
    //      (nonTransparentOwnerOption, that) match {
    //        case (None, _) => false
    //        case (Some(a), b) if a == b => true
    //        case (Some(a), b) => a.isDownstreamMemberOf(that)
    //      }
//  }


  override def toString: String = fullName
}

object DFMember {
  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
    val db : DFDesign.DB.Mutable
  }
}

class DFRef[+T <: DFMember](member : T) {
  def get : T = member
}
object DFRef {
  def apply[T <: DFMember](member: T)(implicit ctx : DFMember.Context) : DFRef[T] = ctx.db.addRef(new DFRef[T](member), member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T]) : T = ref.get
  implicit def refOf[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : DFRef[T] = DFRef(member)
}

