package ZFiant
import DFiant.internals.Meta

import scala.annotation.tailrec

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
  implicit def getOwner(implicit getter : MemberGetter) : DFBlock = ownerRef
  final def getOwnerDesign(implicit getter : MemberGetter) : DFBlock = getOwner match {
    case d : DFDesign.Block => d
    case b : DFBlock => b.getOwnerDesign
  }
  @inline final def name : String = meta.name
  def getFullName(implicit getter : MemberGetter) : String = s"${getOwner.getFullName}.${name}"
  final private[ZFiant] def getOwnerChain(implicit getter : MemberGetter) : List[DFBlock] = if (getOwner.isTop) List(getOwner) else getOwner.getOwnerChain :+ getOwner
  def getRelativeName(implicit callOwner : DFBlock, getter : MemberGetter) : String = {
    if (this isSameOwnerDesignAs callOwner) name
    else if (this isOneLevelBelow callOwner) s"${getOwner.name}.$name"
    else {
      //more complex referencing just summons the two owner chains and compares them.
      //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
      val memberChain = this.getOwnerChain
      val ctxChain = callOwner.getOwnerChain
      ??? //TODO
    }
  }

  final def isSameOwnerDesignAs(that : DFMember)(implicit getter : MemberGetter) : Boolean = getOwnerDesign == that.getOwnerDesign
  final def isOneLevelBelow(that : DFMember)(implicit getter : MemberGetter) : Boolean = getOwnerDesign isSameOwnerDesignAs that

  //  final def isDownstreamMemberOf(that : DFBlock) : Boolean = {
    //      (nonTransparentOwnerOption, that) match {
    //        case (None, _) => false
    //        case (Some(a), b) if a == b => true
    //        case (Some(a), b) => a.isDownstreamMemberOf(that)
    //      }
//  }

  def setMeta(meta : Meta) : DFMember
  def setName(value : String) : DFMember = setMeta(meta.copy(meta.name.copy(value = value)))
  def anonymize : DFMember = setMeta(meta.anonymize)
  def show(implicit getter : MemberGetter) : String = s"$getFullName : $typeName"
}


object DFMember {
//  import shapeless._
//  protected val metaP = ^.meta
//  abstract class CC[P <: CC[P]](implicit metaL: metaP.Lens[P, Meta]) extends DFMember {self : P =>
//    def setName(value : String) : DFMember = metaL().modify(self)(s => s.copy(name = s.name.copy(value)))
//  }

  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
    val db : DFDesign.DB.Mutable
  }
}

class DFRef[T <: DFMember] {
  def get(implicit getter: MemberGetter) : T = getter(this)
}
object DFRef {
  def apply[T <: DFMember](member: T)(implicit ctx : DFMember.Context) : DFRef[T] = ctx.db.getRef(member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T])(implicit getter : MemberGetter) : T = getter(ref)
  implicit def refOf[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : DFRef[T] = DFRef(member)
}

trait MemberGetter {
  def apply[T <: DFMember](ref : DFRef[T]) : T
}
object MemberGetter {
  implicit def ev(implicit ctx : DFMember.Context) : MemberGetter = new MemberGetter {
    override def apply[T <: DFMember](ref: DFRef[T]): T = ctx.db.getMember(ref)
  }
}

