package DFiant
import DFiant.internals.Meta
import csprinter.CSPrinter

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

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
  type TCustomTag <: DFMember.CustomTag
  val ownerRef : DFOwner.Ref
  val tags : DFMember.Tags
  final def getOwner(implicit getSet: MemberGetSet) : DFOwner = this match {
    case top : DFDesign.Block.Top => top
    case _ => ownerRef.get
  }
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
  final val isAnonymous : Boolean = tags.meta.isAnonymous
  final val isNameForced : Boolean = tags.meta.isNameForced
  final val name : String = if (isAnonymous) s"anon${hashCode.toHexString}" else tags.meta.name
  final val hasLateConstruction : Boolean = tags.meta.lateConstruction
  def getFullName(implicit getSet : MemberGetSet) : String = s"${getOwnerBlock.getFullName}.${name}"
  final private[DFiant] def getOwnerChain(implicit getSet : MemberGetSet) : List[DFBlock] =
    if (getOwnerBlock.isTop) List(getOwnerBlock) else getOwnerBlock.getOwnerChain :+ getOwnerBlock
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
  @tailrec private def isInsideOwner(thisMember : DFMember, thatOwner : DFOwner)(implicit getSet : MemberGetSet) : Boolean = {
    (thisMember.getOwner, thatOwner) match {
      case (a, b) if a == b => true
      case (_ : DFDesign.Block.Top, _) => false
      case (od, _) => isInsideOwner(od, thatOwner)
    }
  }
  //true if and only if the member is inside the design at any level
  final def isInsideOwner(that : DFOwner)(implicit getSet : MemberGetSet) : Boolean = isInsideOwner(this, that)
  //true if and only if the two members are equivalent in relation to their design construction context
  protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean

  private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember
  private[DFiant] final def updateOwner(implicit ctx : DFMember.Context) : this.type = setOwnerRef(ctx.owner).asInstanceOf[this.type]
  def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember
  def show(implicit printer: CSPrinter) : String = s"$getFullName : $typeName"
}


object DFMember {
  implicit class MemberExtender[M <: DFMember](member : M)(implicit getSet : MemberGetSet) {
    def setName(value : String) : M = member.setTags(_.setName(value)).asInstanceOf[M]
    def setNameSuffix(value : String) : M = setName(s"${member.name}$value")
    def setNamePrefix(value : String) : M = setName(s"$value${member.name}")
    def anonymize : M = member.setTags(_.anonymize).asInstanceOf[M]
    def keep : M = member.setTags(_.setKeep(true)).asInstanceOf[M]
    def !![CT <: member.TCustomTag : ClassTag](customTag : CT) : M = member.setTags(_.!!(customTag)).asInstanceOf[M]
    def removeTagOf[CT <: member.TCustomTag : ClassTag] : M = member.setTags(_.removeTagOf[CT]).asInstanceOf[M]
    def getTagOf[CT <: member.TCustomTag : ClassTag] : Option[CT] = member.tags.getTagOf[CT]
    def isTaggedWith[CT <: member.TCustomTag : ClassTag](ct : CT) : Boolean = getTagOf[CT].isDefined
    def setLateContruction(value : Boolean) : M = member.setTags(_.setLateContruction(value)).asInstanceOf[M]
    def asRefOwner : M with RefOwner = member.asInstanceOf[M with RefOwner]
  }

  trait CustomTag extends Product with Serializable
  type CustomTagMap = Map[ClassTag[_], CustomTag]
  final case class Tags(meta : Meta, keep : Boolean, customTags : CustomTagMap) extends Product with Serializable {
    def setMeta(meta : Meta) : Tags = copy(meta = meta)
    def setKeep(keep : Boolean) : Tags = copy(keep = keep)
    def !![CT <: CustomTag : ClassTag](customTag : CT) : Tags = copy(customTags = customTags + (classTag[CT] -> customTag))
    def removeTagOf[CT <: CustomTag : ClassTag] : Tags = copy(customTags = customTags - classTag[CT])
    def getTagOf[CT <: CustomTag : ClassTag] : Option[CT] =
      customTags.get(classTag[CT]).asInstanceOf[Option[CT]]
    def =~(that : Tags) : Boolean = this.meta.name == that.meta.name && this.customTags == that.customTags
    def setName(value : String) : Tags = setMeta(meta.setName(value))
    def setLateContruction(value : Boolean) : Tags = setMeta(meta.copy(lateConstruction = value))
    def anonymize : Tags = setMeta(meta.anonymize)
  }
  object Tags {
    implicit def fromMeta(meta : Meta) : Tags = Tags(meta, keep = false, Map())
  }

  trait Context {
    val meta : Meta
    val container : DFOwner.Container
    val db : DFDesign.DB.Mutable
    final def owner : DFOwner = db.OwnershipContext.getCurrentOwner(container)
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
  val designDB : DFDesign.DB
  def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0
  def set[M <: DFMember](originalMember : M)(newMemberFunc : M => M) : M
  def replace[M <: DFMember](originalMember : M)(newMember : M) : M
  def remove[M <: DFMember](member : M) : M
  def getMembersOf(owner : DFOwner) : List[DFMember]
  def setGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any, tag : CT) : Unit
  def getGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any) : Option[CT]
}
object MemberGetSet {
  import DFiant.printer.Printer
  implicit def ev(implicit ctx : DFMember.Context) : MemberGetSet = ctx.db.getSet
  implicit def evGetSet[C <: Printer.Config](implicit printer : Printer[C], lp : shapeless.LowPriority) : MemberGetSet = printer.getSet
}

trait CanBeGuarded extends DFMember
