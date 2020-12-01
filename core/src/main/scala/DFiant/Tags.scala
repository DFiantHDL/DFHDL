package DFiant

import scala.reflect.{ClassTag, classTag}
import internals.DisallowExternalExtensions

abstract class Tags extends TagsOf[DFDesign]

abstract class TagsOf[-D <: DFDesign] extends DisallowExternalExtensions {
  private[this] var _dsn : () => D = _
  protected[this] lazy val dsn : D = _dsn()
  private[DFiant] def attachDesign(dsn : => D) : this.type = {
    _dsn = () => dsn
    this
  }
  private[DFiant] var taggedMembers : List[TaggedMember[DFMember]] = Nil
  final def getTagMap = taggedMembers.foldRight(Map.empty[DFMember, DFMember.CustomTagMap]) {
    case (tm, tagMap) => tagMap.get(tm.member) match {
      case Some(memberTags) => tagMap + (tm.member -> (memberTags + tm.tag))
      case None => tagMap + (tm.member -> Map(tm.tag))
    }
  }

  private[DFiant] def tagMember[M <: DFMember, CT <: DFMember.CustomTagOf[M] : ClassTag](
    member : => M, tag : CT
  ) : TaggedMember[M] = {
    val taggedMember = new TaggedMember[M](this, member, (classTag[CT], tag))
    taggedMembers = taggedMember :: taggedMembers
    taggedMember
  }
  implicit class __MemberExt[M <: DFMember](member : => M) {
    def !!![CT <: DFMember.CustomTagOf[M] : ClassTag](tag : CT) : TaggedMember[M] =
      tagMember(member, tag)
  }
  implicit class __DesignExt[D2 <: DFDesign](design : => D2) {
    def !!![CT <: DFMember.CustomTagOf[DFDesign.Block] : ClassTag](tag : CT) : TaggedMember[DFDesign.Block] =
      tagMember(design.owner, tag)
    def !!!(tags : TagsOf[D2]) : __DesignExt[D2] = {
      tags.attachDesign(design)
      taggedMembers = taggedMembers ++ tags.taggedMembers
      this
    }
  }

  def !! [D2 <: D](moreTags : TagsOf[D2]) : TagsOf[D] = new TagsOf[D] {
    moreTags.attachDesign(dsn.asInstanceOf[D2])
    taggedMembers = this.taggedMembers ++ moreTags.taggedMembers
  }
}
protected class TaggedMember[+M <: DFMember](container : TagsOf[_], _member : => M, val tag : (ClassTag[_], DFMember.CustomTag)) {
  def !![CT <: DFMember.CustomTagOf[M] : ClassTag](tag : CT) : TaggedMember[M] = container.tagMember(_member, tag)
  lazy val member : DFMember = _member
}

