package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ownerRef : DFRef[DFBlock]
  val meta : Meta
  final lazy val owner : DFBlock = ownerRef
  final lazy val ownerDesign : DFDesign = owner match {
    case d : DFDesign => d
    case b : DFBlock => b.ownerDesign
  }
  lazy val name : String = meta.name
  lazy val fullName : String = if (owner.isTop) name else s"${owner.fullName}.${name}"

  final def sameDesignAs(that : DFMember) : Boolean = ownerDesign == that.ownerDesign
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
    val compiler : DFCompiler
  }
}

class DFRef[+T <: DFMember](member : T) {
  def get : T = member
}
object DFRef {
  def apply[T <: DFMember](member: T)(implicit ctx : DFMember.Context) : DFRef[T] = ctx.compiler.addRef(new DFRef[T](member), member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T]) : T = ref.get
  implicit def refOf[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : DFRef[T] = DFRef(member)
}

