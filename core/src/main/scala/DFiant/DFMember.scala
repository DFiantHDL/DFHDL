package DFiant
import DFiant.internals.Meta
import compiler.printer.Printer
import scala.annotation.tailrec

trait HasTypeName {
  lazy val typeName: String = {
    val cls = this.getClass
    val ifc = cls.getInterfaces
    val clsSimpleName = cls.getSimpleName
    val clsAnon = clsSimpleName.contains("anon$") || clsSimpleName.isEmpty
    val ret = if (ifc.isEmpty) { //No interfaces. This is a class
      if (clsAnon) cls.getSuperclass.getSimpleName //For anonymous classes we get the name of the superclass
      else clsSimpleName //get the name of the class
    } else {
      if (clsAnon) ifc.head.getSimpleName //get the name of the head interface
      else clsSimpleName
    }
    ret.replace("$", "") //removing $ from object names
  }
}
trait DFMember extends HasTypeName with Product with Serializable {self =>
  type TTags <: DFMember.Tags{type TTags = self.TTags}
  type TCustomTag <: DFMember.CustomTag
  val ownerRef : DFOwner.Ref
  val tags : TTags
  final def getOwner(implicit getSet: MemberGetSet) : DFOwner = ownerRef.get
  implicit def getOwnerBlock(implicit getSet : MemberGetSet) : DFBlock = ownerRef.get match {
    case b : DFBlock => b
    case o => o.getOwnerBlock
  }
  final def getOwnerDesign(implicit getSet : MemberGetSet) : DFDesign.Block = getOwnerBlock match {
    case d : DFDesign.Block => d
    case b : DFBlock => b.getOwnerDesign
  }
  final def getThisOrOwnerDesign(implicit getSet : MemberGetSet) : DFDesign.Block = this match {
    case d : DFDesign.Block => d
    case x => x.getOwnerDesign
  }
  final val isAnonymous : Boolean = tags.meta.name.anonymous
  final val name : String = if (isAnonymous) s"anon${hashCode.toHexString}" else tags.meta.name
  final val hasLateConstruction : Boolean = tags.meta.lateConstruction
  def getFullName(implicit getSet : MemberGetSet) : String = s"${getOwnerBlock.getFullName}.${name}"
  final private[DFiant] def getOwnerChain(implicit getSet : MemberGetSet) : List[DFBlock] = if (getOwnerBlock.isTop) List(getOwnerBlock) else getOwnerBlock.getOwnerChain :+ getOwnerBlock
  def getRelativeName(implicit callOwner : DFBlock, getSet : MemberGetSet) : String = {
    val designOwner = callOwner.getThisOrOwnerDesign
    if (this isMemberOfDesign designOwner) name
    else if (getOwnerDesign isOneLevelBelow designOwner) s"${getOwnerDesign.name}.$name"
    else if (callOwner isInsideOwner this.getOwnerDesign) name
    else {
      //more complex referencing just summons the two owner chains and compares them.
      //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
      val memberChain = this.getOwnerChain
      val ctxChain = designOwner.getOwnerChain
      ??? //TODO
    }
  }

  final def isMemberOfDesign(that : DFDesign.Block)(implicit getSet : MemberGetSet) : Boolean = getOwnerDesign == that
  final def isSameOwnerDesignAs(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = getOwnerDesign == that.getOwnerDesign
  final def isOneLevelBelow(that : DFMember)(implicit getSet : MemberGetSet) : Boolean =
    this match {
      case _ : DFDesign.Block.Top => false
      case _ => getOwnerDesign isSameOwnerDesignAs that
    }
  //true if and only if the member is outside the design at any level
  final def isOutsideOwner(that : DFOwner)(implicit getSet : MemberGetSet) : Boolean = !isInsideOwner(that)
  //true if and only if the member is inside the design at any level
  final def isInsideOwner(that : DFOwner)(implicit getSet : MemberGetSet) : Boolean = {
    (getOwner, that) match {
      case (a, b) if a == b => true
      case (_ : DFDesign.Block.Top, _) => false
      case (od, _) => od.isInsideOwner(that)
    }
  }
  //true if and only if the two members are equivalent in relation to their design construction context
  protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean

  private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember
  private[DFiant] final def updateOwner(implicit ctx : DFMember.Context) : this.type = setOwnerRef(ctx.owner).asInstanceOf[this.type]
  def setTags(tagsFunc : TTags => TTags)(implicit getSet : MemberGetSet) : DFMember
  def show(implicit getSet : MemberGetSet) : String = s"$getFullName : $typeName"
}


object DFMember {
  implicit class MemberExtender[M <: DFMember](member : M)(implicit getSet : MemberGetSet) {
    def setName(value : String) : M = member.setTags(_.setName(value)).asInstanceOf[M]
    def setNameSuffix(value : String) : M = setName(s"${member.name}$value")
    def setNamePrefix(value : String) : M = setName(s"$value${member.name}")
    def anonymize : M = member.setTags(_.anonymize).asInstanceOf[M]
    def keep : M = member.setTags(_.setKeep(true)).asInstanceOf[M]
    def !!(customTag : member.TCustomTag) : M = member.setTags(_.!!(customTag)).asInstanceOf[M]
    def setLateContruction(value : Boolean) : M = member.setTags(_.setLateContruction(value)).asInstanceOf[M]
    def asRefOwner : M with RefOwner = member.asInstanceOf[M with RefOwner]
  }

  trait CustomTag extends Product with Serializable
  trait Tags extends Product with Serializable {
    type TTags <: Tags
    val meta : Meta
    val keep : Boolean
    val customTags : Set[CustomTag]
    def setMeta(meta : Meta) : TTags
    def setKeep(keep : Boolean) : TTags
    def !!(customTag : CustomTag) : TTags
    def =~(that : Tags) : Boolean = this.meta.name == that.meta.name && this.customTags == that.customTags
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
      implicit metaL: metaP.Lens[P, Meta], keepL : keepP.Lens[P, Boolean], customTagsL : customTagsP.Lens[P, Set[CustomTag]]
    ) extends Tags {self : P =>
      type TTags = P
      final def setMeta(meta : Meta) : P = metaL().set(self)(meta)
      final def setKeep(keep : Boolean) : P = keepL().set(self)(keep)
      final def !!(customTag : CustomTag) : P = customTagsL().modify(self)(tList => tList + customTag)
    }

    final case class Basic(meta : Meta, keep : Boolean, customTags : Set[CustomTag]) extends Tags.CC[Basic]
    implicit def fromMeta(meta : Meta) : Basic = Basic(meta, keep = false, Set())
  }

  trait Context {
    val meta : Meta
    val container : DFOwner.Container
    val db : DFDesign.DB.Mutable
    final def owner : DFOwner = db.Ownership.getCurrentOwner(container)
  }

  sealed trait Ref extends HasTypeName {
    type TType <: Ref.Type
    type TMember <: DFMember
    val refType : TType
    def get[M0 >: TMember](implicit getSet: MemberGetSet) : M0
    override def toString: String = s"$typeName<${hashCode.toHexString}>"
  }
  object Ref {
    trait Type
    sealed trait Of[T <: Type, +M <: DFMember] extends Ref {
      type TType = T
      type TMember <: M
      def get[M0 >: TMember](implicit getSet: MemberGetSet) : M0 = getSet(this)
    }

    def newRefFor[M <: DFMember, T <: Type, R <: Ref.Of[T, M]](ref : R, member: M)(implicit ctx : DFMember.Context) : R = ctx.db.newRefFor[M, T, R](ref, member)
    implicit def memberOf[M <: DFMember, T <: Type](ref : Ref.Of[T, M])(implicit getSet : MemberGetSet) : M = getSet(ref)
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
  def designDB : DFDesign.DB
  def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0
  def set[M <: DFMember](originalMember : M)(newMemberFunc : M => M) : M
  def replace[M <: DFMember](originalMember : M)(newMember : M) : M
  def remove[M <: DFMember](member : M) : M
  def getMembersOf(owner : DFOwner) : List[DFMember]
}
object MemberGetSet {
  implicit def ev(implicit ctx : DFMember.Context) : MemberGetSet = ctx.db.getSet
  implicit def evGetSet(implicit ctx : Printer.Context, lp : shapeless.LowPriority) : MemberGetSet = ctx.getSet
}

trait CanBeGuarded extends DFMember
