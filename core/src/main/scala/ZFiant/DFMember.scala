package ZFiant
import DFiant.internals.Meta
import compiler.printer.Printer
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
trait DFMember extends HasTypeName with Product with Serializable {self =>
  type TTags <: DFMember.Tags{type TTags = self.TTags}
  val ownerRef : DFBlock.Ref
  val tags : TTags
  implicit def getOwner(implicit getset : MemberGetSet) : DFBlock = ownerRef
  final def getOwnerDesign(implicit getset : MemberGetSet) : DFDesign.Block = getOwner match {
    case d : DFDesign.Block => d
    case b : DFBlock => b.getOwnerDesign
  }
  final def getThisOrOwnerDesign(implicit getset : MemberGetSet) : DFDesign.Block = this match {
    case d : DFDesign.Block => d
    case x => x.getOwnerDesign
  }
  final val isAnonymous : Boolean = tags.meta.name.anonymous
  final val name : String = if (isAnonymous) s"anon$hashCode()" else tags.meta.name
  final val hasLateConstruction : Boolean = tags.meta.lateConstruction
  def getFullName(implicit getset : MemberGetSet) : String = s"${getOwner.getFullName}.${name}"
  final private[ZFiant] def getOwnerChain(implicit getset : MemberGetSet) : List[DFBlock] = if (getOwner.isTop) List(getOwner) else getOwner.getOwnerChain :+ getOwner
  def getRelativeName(implicit callOwner : DFBlock, getset : MemberGetSet) : String = {
    val designOwner = callOwner.getThisOrOwnerDesign
    if (this isMemberOfDesign designOwner) name
    else if (this isOneLevelBelow designOwner) s"${getOwner.name}.$name"
    else if (callOwner isInsideDesign this.getOwnerDesign) name
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
  final def isOneLevelBelow(that : DFMember)(implicit getset : MemberGetSet) : Boolean =
    getOwnerDesign match {
      case _ : DFDesign.Block.Top => false
      case od => od isSameOwnerDesignAs that
    }
  //true if and only if the member is outside the design at any level
  final def isOutsideDesign(that : DFDesign.Block)(implicit getset : MemberGetSet) : Boolean = !isInsideDesign(that)
  //true if and only if the member is inside the design at any level
  final def isInsideDesign(that : DFDesign.Block)(implicit getset : MemberGetSet) : Boolean = {
    (getOwnerDesign, that) match {
      case (a, b) if a == b => true
      case (_ : DFDesign.Block.Top, _) => false
      case _ => isInsideDesign(that)
    }
  }
//  def isSimilarTo(that : DFMember) : Boolean

  def setTags(tags : TTags)(implicit getset : MemberGetSet) : DFMember
  def show(implicit getset : MemberGetSet) : String = s"$getFullName : $typeName"
}


object DFMember {
  implicit class MemberExtender[M <: DFMember](member : M)(implicit getset : MemberGetSet) {
    def setName(value : String) : M = member.setTags(member.tags.setName(value)).asInstanceOf[M]
    def setNameSuffix(value : String) : M = setName(s"${member.name}$value")
    def setNamePrefix(value : String) : M = setName(s"$value${member.name}")
    def anonymize : M = member.setTags(member.tags.anonymize).asInstanceOf[M]
    def keep : M = member.setTags(member.tags.setKeep(true)).asInstanceOf[M]
    def addCustomTag(customTag : CustomTag) : M = member.setTags(member.tags.addCustomTag(customTag)).asInstanceOf[M]
    def setLateContruction(value : Boolean) : M = member.setTags(member.tags.setLateContruction(value)).asInstanceOf[M]
    def asRefOwner : M with RefOwner = member.asInstanceOf[M with RefOwner]
  }

  trait CustomTag extends Product with Serializable
  trait Tags extends Product with Serializable {
    type TTags <: Tags
    val meta : Meta
    val keep : Boolean
    val customTags : List[CustomTag]
    def setMeta(meta : Meta) : TTags
    def setKeep(keep : Boolean) : TTags
    def addCustomTag(customTag : CustomTag) : TTags
    final def setName(value : String) : TTags = setMeta(meta.copy(name = meta.name.copy(value = value, anonymous = false)))
    final def setLateContruction(value : Boolean) : TTags = setMeta(meta.copy(lateConstruction = value))
    final def anonymize : TTags = setMeta(meta.copy(name = meta.name.copy(anonymous = true)))
  }
  object Tags {
    import shapeless._
    final protected val metaP = ^.meta
    final protected val keepP = ^.keep
    final protected val customTagsP = ^.customTags
    abstract class CC[P <: CC[P]](
      implicit metaL: metaP.Lens[P, Meta], keepL : keepP.Lens[P, Boolean], customTagsL : customTagsP.Lens[P, List[CustomTag]]
    ) extends Tags {self : P =>
      type TTags = P
      final def setMeta(meta : Meta) : P = metaL().set(self)(meta)
      final def setKeep(keep : Boolean) : P = keepL().set(self)(keep)
      final def addCustomTag(customTag : CustomTag) : P = customTagsL().modify(self)(tList => customTag :: tList)
    }

    final case class Basic(meta : Meta, keep : Boolean, customTags : List[CustomTag]) extends Tags.CC[Basic]
    implicit def fromMeta(meta : Meta) : Basic = Basic(meta, keep = false, List())
  }

  trait Context {
    val meta : Meta
    def owner : DFBlock
    val db : DFDesign.DB.Mutable
  }

  final class OwnerInjector(designBlock : DFDesign.Block) {
    private var value : DFBlock = designBlock
    def inject(newOwner : DFBlock) : Unit = value = newOwner
    def get : DFBlock = value
    def injectOwnerAndRun(injectedOwner : DFBlock)(block : => Unit) : Unit = {
      val injectedOwnerBackup = get
      inject(injectedOwner)
      block
      inject(injectedOwnerBackup)
    }
  }

  sealed trait Ref extends HasTypeName {
    type TType <: Ref.Type
    type TMember <: DFMember
    val refType : TType
    def get[M0 >: TMember](implicit getset: MemberGetSet) : M0
    override def toString: String = s"$typeName<${hashCode.toHexString}>"
  }
  object Ref {
    trait Type
    sealed trait Of[T <: Type, +M <: DFMember] extends Ref {
      type TType = T
      type TMember <: M
      def get[M0 >: TMember](implicit getset: MemberGetSet) : M0 = getset(this)
    }

    def newRefFor[M <: DFMember, T <: Type, R <: Ref.Of[T, M]](ref : R, member: M)(implicit ctx : DFMember.Context) : R = ctx.db.newRefFor[M, T, R](ref, member)
    implicit def memberOf[M <: DFMember, T <: Type](ref : Ref.Of[T, M])(implicit getset : MemberGetSet) : M = getset(ref)
    implicit def apply[M <: DFMember, T <: Type](member: M)(implicit ctx : DFMember.Context, rt : T)
    : Ref.Of[T, M] = newRefFor[M, T, Ref.Of[T, M]](new Ref.Of[T, M]{val refType : T = rt}, member)
  }

  sealed trait RefOwner
  object RefOwner {
    trait Type extends Ref.Type
    implicit val ev : Type = new Type {}
  }
  sealed trait OwnedRef extends Ref {
    val owner : Ref.Of[RefOwner.Type, DFMember]
  }
  object OwnedRef {
    trait Type extends Ref.Type
    implicit val ev : Type = new Type {}
    sealed trait Of[T <: Type, +M <: DFMember] extends OwnedRef with Ref.Of[T, M]
    implicit def apply[M <: DFMember, T <: Type, O <: DFMember](member: M)(
      implicit ctx : DFMember.Context, rt : T, refOwner : => O with RefOwner
    ) : OwnedRef.Of[T, M] =
      Ref.newRefFor[M, T, OwnedRef.Of[T, M]](
        new OwnedRef.Of[T, M]{
          val refType: T = rt
          lazy val owner: Ref.Of[RefOwner.Type, DFMember] = refOwner
        },
        member
      )
  }
}


trait MemberGetSet {
  def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0
  def set[M <: DFMember](originalMember : M, newMember : M) : M
}
object MemberGetSet {
  implicit def ev(implicit ctx : DFMember.Context) : MemberGetSet = ctx.db.getset
  implicit def evGetSet(implicit ctx : Printer.Context, lp : shapeless.LowPriority) : MemberGetSet = ctx.getset
}

trait CanBeGuarded extends DFMember
