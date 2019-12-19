package ZFiant
import DFiant.internals.Meta

trait HasTypeName {
  lazy val typeName: String = {
    val cls = this.getClass
    val ifc = cls.getInterfaces
    val clsSimpleName = cls.getSimpleName
    val clsAnon = clsSimpleName.contains("anon$") || clsSimpleName.isEmpty
    if (ifc.isEmpty) { //No interfaces. This is a class
      if (clsAnon) cls.getSuperclass.getSimpleName //For anonymous classes we get the name of the superclass
      else clsSimpleName //get the name of the class
    } else {
      if (clsAnon) ifc.head.getSimpleName //get the name of the head interface
      else clsSimpleName
    }
  }
}
trait DFMember extends HasTypeName with Product with Serializable {
  val ownerRef : DFRef[DFBlock]
  val meta : Meta
  implicit lazy val owner : DFBlock = ownerRef
  final val ownerDesign : DFBlock = owner match {
    case d : DFDesign.Block => d
    case d : DFDesign.TopBlock => d
    case b : DFBlock => b.ownerDesign
  }
  @inline final def getName : String = meta.name
  def getFullName : String = s"${owner.getFullName}.${getName}"
  final private[ZFiant] def getOwnerChain : List[DFBlock] = if (owner.isTop) List(owner) else owner.getOwnerChain :+ owner
  def getRelativeName(implicit callOwner : DFBlock) : String = {
    if (this isSameOwnerDesignAs callOwner) getName
    else if (this isOneLevelBelow callOwner) s"${owner.getName}.$getName"
    else {
      //more complex referencing just summons the two owner chains and compares them.
      //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
      val memberChain = this.getOwnerChain
      val ctxChain = callOwner.getOwnerChain
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


  override def toString: String = s"$getFullName : $typeName"
}

object DFMember {
  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
    val db : DFDesign.DB.Mutable
  }
}

class DFRef[+T] {
  def get : T = ???
  override def toString: String = get.toString
}
object DFRef {
  def apply[T <: DFMember](member: T)(implicit ctx : DFMember.Context) : DFRef[T] = ctx.db.getRef(member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T]) : T = ref.get
  implicit def refOf[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : DFRef[T] = DFRef(member)
}

