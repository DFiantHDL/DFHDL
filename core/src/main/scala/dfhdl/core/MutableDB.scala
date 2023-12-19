package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir.{
  DB,
  DFDesignInst,
  DFDesignBlock,
  DFMember,
  DFOwner,
  DFRef,
  DFRefAny,
  DFTag,
  DFVal,
  MemberGetSet,
  SourceFile,
  MemberView
}

import scala.reflect.{ClassTag, classTag}
import collection.mutable

private case class MemberEntry(
    irValue: DFMember,
    refSet: Set[DFRefAny],
    ignore: Boolean
)

class MutableDB(val duringTest: Boolean = false):
  private val self = this
  private[MutableDB] val members: mutable.ArrayBuffer[MemberEntry] = mutable.ArrayBuffer()
  def top: DFDesignBlock = members.head.irValue match
    case o @ DFDesignBlock.Top() => o
    case x => throw new IllegalArgumentException(s"Unexpected member head, $x")
  private[MutableDB] val memberTable: mutable.Map[DFMember, Int] = mutable.Map()
  private[MutableDB] val refTable: mutable.Map[DFRefAny, DFMember] = mutable.Map()
  // meta programming external MemberGetSet DB access
  private[MutableDB] var metaGetSetOpt: Option[MemberGetSet] = None
  def setMetaGetSet(metaGetSet: MemberGetSet): Unit =
    metaGetSetOpt = Some(metaGetSet)
    portsByName ++= metaGetSet.designDB.portsByName
  object OwnershipContext:
    private var stack: List[DFOwner] = Nil
    private var lateStack: List[Boolean] = Nil
    private var defInputsStack: List[List[DFValAny]] = Nil
    def enter(owner: DFOwner): Unit =
//      println(s"enter ${owner}")
      stack = owner :: stack
      lateStack = false :: lateStack
      owner match
        case design: DFDesignBlock =>
          defInputsStack = Nil :: defInputsStack
        case _ =>
    def exit(): Unit =
      // println(s"exit ${owner}")
      owner match
        case design: DFDesignBlock =>
          defInputsStack = defInputsStack.drop(1)
        case _ =>
      stack = stack.drop(1)
      lateStack = lateStack.drop(1)
    def enterLate(): Unit =
      lateStack = true :: lateStack
    def exitLate(): Unit =
      lateStack = lateStack.drop(1)
    def saveDefInputs(inputs: List[DFValAny]): Unit =
      defInputsStack = inputs :: defInputsStack.drop(1)
    def getDefInput(idx: Int): DFValAny =
      defInputsStack.head(idx)
    def owner: DFOwner = stack.head
    def lateConstruction: Boolean = lateStack.headOption.getOrElse(false)
    def replaceOwner(originalOwner: DFOwner, newOwner: DFOwner): Unit =
      stack = stack.map { o =>
        if (o == originalOwner) newOwner
        else o
      }
    def ownerOption: Option[DFOwner] = stack.headOption
  end OwnershipContext

  object global_tags:
    private[MutableDB] val tagMap: mutable.Map[(Any, ClassTag[?]), DFTag] =
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

  def injectDB(injected: MutableDB): Unit =
    println("injected")
    injected.touchLazyRefs()
    // The injected ref table may reference existing members due to port connections across hierarchies.
    // In this case we first update the existing member entries with the additional references.
    // Additionally, we add the injected reference to the reference table in any case.
    injected.refTable.foreach:
      case rec @ (ref, member) if memberTable.contains(member) =>
        val idx = memberTable(member)
        val memberEntry = members(idx)
        members.update(idx, memberEntry.copy(refSet = memberEntry.refSet + ref))
        refTable += rec
      case rec =>
        refTable += rec
    val offset = members.length
    // Add the injected member table while taking into account the offset from the existing
    // member entries.
    memberTable ++= injected.memberTable.view.mapValues(_ + offset)
    // Add the injected member entries
    members ++= injected.members
    // Add the injected global tags
    global_tags.tagMap ++= injected.global_tags.tagMap
    // Add the injected errors
    logger.injectErrors(injected.logger)
    // The owner of the injected top will be this top
    newRefFor[DFOwner | DFMember.Empty, DFOwner.Ref](
      injected.top.ownerRef,
      this.top
    )
  end injectDB

  def addMember[M <: DFMember](member: M): M =
    dirtyDB()
    //        println(f"""${"addMember"}%-20s ${s"${member.name} : ${member.typeName}"}%-30s ${member.getOwner.nameAndType}""")
    memberTable += (member -> members.length)
    members += MemberEntry(member, Set(), false)
    // caching ports for quick by-name access
    member match
      case design: DFDesignInst =>
        portsByName += design -> Map()
      case port: DFVal.Dcl if port.isPort =>
        val design = member.getOwnerDesign
        val namePathMap = portsByName(design)
        val namePath = port.getRelativeName(design)
        portsByName += design -> namePathMap.updated(namePath, port)
      case _ =>
    member
  end addMember

  val logger = new Logger

  // same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
  def plantMember[M <: DFMember](owner: DFOwner, member: M): M =
    newRefFor[DFOwner | DFMember.Empty, DFOwner.Ref](
      member.ownerRef,
      owner
    ) // now this reference will refer to meta design owner
    addMember(member)

  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R =
    memberTable.get(member) match
      // The member already exists, but it might have been updated
      case Some(idx) =>
        // get the newest member at index
        val memberEntry = members(idx)
        members.update(idx, memberEntry.copy(refSet = memberEntry.refSet + ref))
        refTable += (ref -> memberEntry.irValue)
      // In case where we do meta programming and planting one design into another,
      // we may not have the member available at the table. This is OK.
      // So we only add the reference here.
      case _ =>
        refTable += (ref -> member)
    ref
  end newRefFor

  def getMembers: List[DFMember] = members.view.map(e => e._1).toList
  def getMembersSize: Int = members.size
  def getMembers(from: Int, until: Int): List[DFMember] =
    members.view.slice(from, until).filterNot(e => e._3).map(e => e._1).toList

  def getMembersOf(owner: DFOwner, memberView: MemberView): List[DFMember] =
    val ret = memberTable.get(owner) match
      case Some(idx) =>
        var list = List.empty[DFMember]
        var i = idx + 1
        while (i < members.length) do
          val m = members(i)._1
          memberView match
            case MemberView.Folded if m.getOwner == owner => list = m :: list
            case MemberView.Flattened if m.isInsideOwner(owner) =>
              list = m :: list
            case _ if m.isOutsideOwner(owner) => i = members.length
            case _                            => // do nothing
          i = i + 1
        list
      case None => Nil
    ret.reverse
  end getMembersOf

  def getMember[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): M0 = refTable.getOrElse(ref, metaGetSetOpt.get(ref)).asInstanceOf[M0]

  val portsByName = mutable.Map.empty[DFDesignInst, Map[String, DFVal.Dcl]]
  def getMemberByName[M <: DFMember, M0 <: M](
      designRef: DFRef.ByName[M, ?],
      namePath: String
  ): M0 =
    val designInst = getMember(designRef).asInstanceOf[DFDesignInst]
    portsByName(designInst)(namePath).asInstanceOf[M0]

  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
    dirtyDB()
    val idx = memberTable(originalMember)
    // get the most updated member currently positioned at the index of the original member
    val originalMemberUpdated = members(idx)._1.asInstanceOf[M]
    // apply function to get the new member
    val newMember = newMemberFunc(originalMemberUpdated)
    // in case the member is an owner, we check the owner stack to replace it
    (originalMember, newMember) match
      case (o: DFOwner, n: DFOwner) => OwnershipContext.replaceOwner(o, n)
      case _                        =>
    val memberEntry = members(idx)
    // update all references to the new member
    memberEntry.refSet.foreach(r => refTable.update(r, newMember))
    // add the member to the table with the position index
    // (we don't remove the old member since it might still be used as a user-reference in a mutable DB)
    memberTable.update(newMember, idx)
    // update the member in the member position array
    members.update(idx, memberEntry.copy(irValue = newMember))
    // update design ports cache
    originalMember match
      case origDesign: DFDesignInst =>
        val namePathMap = portsByName(origDesign)
        val newDesign = newMember.asInstanceOf[DFDesignInst]
        portsByName -= origDesign
        portsByName += newDesign -> namePathMap
      case origPort: DFVal.Dcl if origPort.isPort =>
        val newPort = newMember.asInstanceOf[DFVal.Dcl]
        val design = newPort.getOwnerDesign
        val namePathMap = portsByName(design)
        val namePath = newPort.getRelativeName(design)
        portsByName += design -> namePathMap.updated(namePath, newPort)
      case _ =>
    newMember
  end setMember

  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
    if (originalMember == newMember) return newMember // nothing to do
    // marking the newMember slot as 'ignore' in case it exists
    ignoreMember(newMember)
    // replace the member by setting a new one at its position
    setMember[M](originalMember, _ => newMember)
    newMember

  def ignoreMember[M <: DFMember](
      member: M
  ): M = // ignoring it means removing it for the immutable DB
    dirtyDB()
    memberTable.get(member).foreach { idx =>
      members.update(idx, members(idx).copy(irValue = member, ignore = true))
    }
    member

  private def dirtyDB(): Unit =
    memoizedDB = None
  private var memoizedDB: Option[DB] = None

  private[MutableDB] def touchLazyRefs(): Unit =
    var size = -1
    // Touching all lazy origin refs to force their addition.
    // During this procedure it is possible that new reference are added. If so, we re-iterate
    while (refTable.size != size) do
      size = refTable.size
      refTable.keys.foreach {
        case or: DFRef.TwoWayAny => or.originRef
        case _                   => // do nothing
      }

  def immutable: DB = memoizedDB.getOrElse {
    touchLazyRefs()
    val notIgnoredMembers =
      members.iterator.filterNot(e => e.ignore).map(e => e.irValue).toList
    val db = DB(notIgnoredMembers, refTable.toMap, global_tags.tagMap.toMap, Nil)
    memoizedDB = Some(db)
    db
  }

  given getSet: MemberGetSet with
    def designDB: DB = immutable
    def apply[M <: DFMember, M0 <: M](
        ref: DFRef[M]
    ): M0 = getMember(ref)
    def apply[M <: DFMember, M0 <: M](designRef: DFRef.ByName[M, ?], namePath: String): M0 =
      getMemberByName(designRef, namePath)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      setMember(originalMember, newMemberFunc)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M =
      replaceMember(originalMember, newMember)
    def remove[M <: DFMember](member: M): M = ignoreMember(member)
    def getMembersOf(owner: DFOwner, memberView: MemberView): List[DFMember] =
      self.getMembersOf(owner, memberView)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = global_tags.set(taggedElement, tag)
    def getGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any
    ): Option[CT] = global_tags.get(taggedElement)
  end getSet

end MutableDB
