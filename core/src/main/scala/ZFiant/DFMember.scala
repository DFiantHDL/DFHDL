package ZFiant
import DFiant.internals.Meta

trait DFMember {
  val ownerRef : DFRef[DFBlock]
  val meta : Meta
  final lazy val owner : DFBlock = ownerRef
//  lazy val id = owner.addMember(this)
  override def toString: String = meta.name
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
  def apply[T <: DFMember](member: T): DFRef[T] = new DFRef[T](member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T]) : T = ref.get
  implicit def refOf[T <: DFMember](member : T) : DFRef[T] = DFRef(member)
}

