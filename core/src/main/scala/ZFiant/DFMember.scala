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
    val db : DFDesign.DB
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

