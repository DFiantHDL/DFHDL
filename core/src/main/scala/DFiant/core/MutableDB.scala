package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir.{DFOwner, DFMember, DFRef, DFTag, DB, MemberGetSet}
import scala.reflect.{ClassTag, classTag}
import collection.mutable

class MutableDB(val duringTest: Boolean = false):
  private val self = this
  private var members
  //                          Member    RefSet      Ignore
      : mutable.ArrayBuffer[(DFMember, Set[DFRef], Boolean)] =
    mutable.ArrayBuffer()
//  def top: Block.Top = members.head._1 match
//    case m: Block.Top => m
  private var memberTable: mutable.Map[DFMember, Int] = mutable.Map()
  private var refTable: mutable.Map[DFRef, DFMember] = mutable.Map()

  object global_tags:
    private[MutableDB] var tagMap: mutable.Map[(Any, ClassTag[_]), DFTag] =
      mutable.Map()
    def set[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit =
      tagMap += ((taggedElement, classTag[CT]) -> tag)
    def get[CT <: DFTag: ClassTag](
        taggedElement: Any
    ): Option[CT] =
      tagMap.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
  end global_tags

  def addMember[M <: DFMember](member: M): M =
//    elaborateFSMHistoryHead()
    //        println(f"""${"addMember"}%-20s ${s"${member.name} : ${member.typeName}"}%-30s ${member.getOwner.nameAndType}""")
    memberTable += (member -> members.length)
    members += Tuple3(member, Set(), false)
    member

////  def addMemberOf[M <: DFMember](member: DFMember)(implicit
////      ctx: DFMember.Context
////  ): M with DFMember.RefOwner =
////    addMember(ctx.container, member).asInstanceOf[M with DFMember.RefOwner]
//
  //same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
  def plantMember[M <: DFMember](owner: DFOwner, member: M): M =
    newRefFor[DFOwner, DFOwner.Ref](
      member.ownerRef,
      owner
    ) //now this reference will refer to meta design owner
    addMember(member)

  def newRefFor[M <: DFMember, R <: DFRef.Of[M]](ref: R, member: M): R =
    memberTable.get(member) match {
      //The member already exists, but it might have been updated
      case Some(idx) =>
        //get the newest member at index
        val (newestMember, refSet, ignore) = members(idx)
        members.update(idx, (newestMember, refSet + ref, ignore))
        refTable += (ref -> newestMember)
      //In case where we do meta programming and planting one design into another,
      //we may not have the member available at the table. This is OK.
      //So we only add the reference here.
      case _ =>
        refTable += (ref -> member)
    }
    ref

  def getMembers: Iterator[DFMember] = members.view.map(e => e._1).iterator
  def getMembersOf(owner: DFOwner): List[DFMember] = ???
//    val ret = memberTable.get(owner) match {
//      case Some(idx) =>
//        var list = List.empty[DFMember]
//        var i = idx + 1
//        while (i < members.length) {
//          val m = members(i)._1
//          if (m.getOwner == owner) list = m :: list
//          else if (m.isOutsideOwner(owner)) i = members.length
//          i = i + 1
//        }
//        list
//      case None => Nil
//    }
//    ret.reverse

  def getMember[M <: DFMember, M0 <: M](
      ref: DFRef.Of[M]
  ): M0 = refTable(ref).asInstanceOf[M0]

  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
    val idx = memberTable(originalMember)
    //get the most updated member currently positioned at the index of the original member
    val originalMemberUpdated = members(idx)._1.asInstanceOf[M]
    //apply function to get the new member
    val newMember = newMemberFunc(originalMemberUpdated)
    val (_, refSet, ignore) = members(idx)
    //update all references to the new member
    refSet.foreach(r => refTable.update(r, newMember))
    //add the member to the table with the position index
    //(we don't remove the old member since it might still be used as a user-reference in a mutable DB)
    memberTable.update(newMember, idx)
    //update the member in the member position array
    members.update(idx, (newMember, refSet, ignore))
    newMember

  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
    ignoreMember(
      newMember
    ) //marking the newMember slot as 'ignore' in case it exists
    setMember[M](originalMember, _ => newMember)
    newMember

  def ignoreMember[M <: DFMember](
      member: M
  ): M = //ignoring it means removing it for the immutable DB
    memberTable.get(member).foreach { idx =>
      members.update(idx, (member, members(idx)._2, true))
    }
    member

  def immutable: DB = {
    var size = -1
    //Touching all lazy origin refs to force their addition.
    //During this procedure it is possible that new reference are added. If so, we re-iterate
    while (refTable.size != size) {
      size = refTable.size
      refTable.keys.foreach {
        case or: DFRef.TwoWay[_] => or.originRef
        case _                   => //do nothing
      }
    }
    val notIgnoredMembers =
      members.iterator.filterNot(e => e._3).map(e => e._1).toList
    DB(notIgnoredMembers, refTable.toMap, global_tags.tagMap.toMap)
  }

  given getSet: MemberGetSet with
    val designDB: DB = immutable
    def apply[M <: DFMember, M0 <: M](
        ref: DFRef.Of[M]
    ): M0 = getMember(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      setMember(originalMember, newMemberFunc)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M =
      replaceMember(originalMember, newMember)
    def remove[M <: DFMember](member: M): M = ignoreMember(member)
    def getMembersOf(owner: DFOwner): List[DFMember] = self.getMembersOf(owner)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = global_tags.set(taggedElement, tag)
    def getGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any
    ): Option[CT] = global_tags.get(taggedElement)

end MutableDB
