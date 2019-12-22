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
  implicit def getOwner(implicit getset : MemberGetSet) : DFBlock = ownerRef
  final def getOwnerDesign(implicit getset : MemberGetSet) : DFDesign.Block = getOwner match {
    case d : DFDesign.Block => d
    case b : DFBlock => b.getOwnerDesign
  }
  final def getThisOrOwnerDesign(implicit getset : MemberGetSet) : DFDesign.Block = this match {
    case d : DFDesign.Block => d
    case x => x.getOwnerDesign
  }
  @inline final def name : String = meta.name
  def getFullName(implicit getset : MemberGetSet) : String = s"${getOwner.getFullName}.${name}"
  final private[ZFiant] def getOwnerChain(implicit getset : MemberGetSet) : List[DFBlock] = if (getOwner.isTop) List(getOwner) else getOwner.getOwnerChain :+ getOwner
  def getRelativeName(implicit callOwner : DFBlock, getset : MemberGetSet) : String = {
    val designOwner = callOwner.getThisOrOwnerDesign
    if (this isMemberOfDesign designOwner) name
    else if (this isOneLevelBelow designOwner) s"${getOwner.name}.$name"
    else {
      //more complex referencing just summons the two owner chains and compares them.
      //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
      val memberChain = this.getOwnerChain
      val ctxChain = designOwner.getOwnerChain
      ??? //TODO
    }
  }

  final def isMemberOfDesign(that : DFDesign.Block)(implicit getset : MemberGetSet) : Boolean = getOwnerDesign == that
  final def isSameOwnerDesignAs(that : DFMember)(implicit getset : MemberGetSet) : Boolean = getOwnerDesign == that.getOwnerDesign
  final def isOneLevelBelow(that : DFMember)(implicit getset : MemberGetSet) : Boolean = getOwnerDesign isSameOwnerDesignAs that

  //  final def isDownstreamMemberOf(that : DFBlock) : Boolean = {
    //      (nonTransparentOwnerOption, that) match {
    //        case (None, _) => false
    //        case (Some(a), b) if a == b => true
    //        case (Some(a), b) => a.isDownstreamMemberOf(that)
    //      }
//  }

  def setMeta(meta : Meta)(implicit getset : MemberGetSet) : DFMember
  def show(implicit getset : MemberGetSet) : String = s"$getFullName : $typeName"
}


object DFMember {
//  import shapeless._
//  protected val metaP = ^.meta
//  abstract class CC[P <: CC[P]](implicit metaL: metaP.Lens[P, Meta]) extends DFMember {self : P =>
//    def setName(value : String) : DFMember = metaL().modify(self)(s => s.copy(name = s.name.copy(value)))
//  }

  implicit class MemberExtender[T <: DFMember](member : T) {
    def setName(value : String)(implicit getset : MemberGetSet) : T =
      member.setMeta(member.meta.copy(member.meta.name.copy(value = value, anonymous = false))).asInstanceOf[T]
    def anonymize(implicit getset : MemberGetSet) : T = member.setMeta(member.meta.anonymize).asInstanceOf[T]
  }

  trait Context extends Product with Serializable {
    val meta : Meta
    val owner : DFBlock
    val db : DFDesign.DB.Mutable
  }
}

class DFRef[T <: DFMember] {
  def get(implicit getset: MemberGetSet) : T = getset(this)
}
object DFRef {
  def apply[T <: DFMember](member: T)(implicit ctx : DFMember.Context) : DFRef[T] = ctx.db.getRef(member)
  implicit def memberOf[T <: DFMember](ref : DFRef[T])(implicit getset : MemberGetSet) : T = getset(ref)
  implicit def refOf[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : DFRef[T] = DFRef(member)
}

trait MemberGetSet {
  def apply[T <: DFMember](ref : DFRef[T]) : T
  def set[T <: DFMember](originalMember : T, newMember : T) : T
}
object MemberGetSet {
  implicit def ev(implicit ctx : DFMember.Context) : MemberGetSet = ctx.db.getset
}

