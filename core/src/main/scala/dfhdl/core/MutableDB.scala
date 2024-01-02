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

final class MutableDB():
  private val self = this

  // error logger
  val logger = new Logger

  // meta programming external MemberGetSet DB access
  private[MutableDB] var metaGetSetOpt: Option[MemberGetSet] = None
  def setMetaGetSet(metaGetSet: MemberGetSet): Unit =
    metaGetSetOpt = Some(metaGetSet)

  class DesignContext:
    val members: mutable.ArrayBuffer[MemberEntry] = mutable.ArrayBuffer()
    val memberTable: mutable.Map[DFMember, Int] = mutable.Map()
    val refTable: mutable.Map[DFRefAny, DFMember] = mutable.Map()
    var defInputs: List[DFValAny] = Nil

    def addMember[M <: DFMember](member: M): M =
      memberTable += (member -> members.length)
      members += MemberEntry(member, Set(), false)
      member
    end addMember

    // same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
    def plantMember[M <: DFMember](owner: DFOwner, member: M): M =
      newRefFor[DFOwner | DFMember.Empty, DFOwner.Ref](
        member.ownerRef,
        owner
      ) // now this reference will refer to meta design owner
      addMember(member)
    end plantMember

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

    def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
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
      newMember
    end setMember

    def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
      if (originalMember == newMember) return newMember // nothing to do
      // marking the newMember slot as 'ignore' in case it exists
      ignoreMember(newMember)
      // replace the member by setting a new one at its position
      setMember[M](originalMember, _ => newMember)
      newMember
    end replaceMember

    def ignoreMember[M <: DFMember](
        member: M
    ): M = // ignoring it means removing it for the immutable DB
      memberTable.get(member).foreach { idx =>
        members.update(idx, members(idx).copy(irValue = member, ignore = true))
      }
      member
    end ignoreMember

    def touchLazyRefs(): Unit =
      var size = -1
      // Touching all lazy origin refs to force their addition.
      // During this procedure it is possible that new reference are added. If so, we re-iterate
      while (refTable.size != size) do
        size = refTable.size
        refTable.keys.foreach {
          case or: DFRef.TwoWayAny => or.originRef
          case _                   => // do nothing
        }
    end touchLazyRefs

    def getMemberList: List[DFMember] =
      members.view.filterNot(e => e.ignore).map(e => e.irValue).toList
    def getRefTable: Map[DFRefAny, DFMember] = refTable.toMap

    def injectDC(injected: DesignContext): Unit =
      injected.touchLazyRefs()
      // The injected ref table may reference existing members due to references across hierarchies.
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
      // val offset = members.length
      // // Add the injected member table while taking into account the offset from the existing
      // // member entries.
      // memberTable ++= injected.memberTable.view.mapValues(_ + offset)
      // // Add the injected member entries
      // members ++= injected.members
    end injectDC
  end DesignContext

  object DesignContext:
    var current: DesignContext = new DesignContext
    var stack: List[DesignContext] = Nil
    val designMembers: mutable.Map[DFDesignBlock, List[DFMember]] = mutable.Map()
    def startDesign(design: DFDesignBlock): Unit =
      stack = current :: stack
      current = new DesignContext
    def endDesign(design: DFDesignBlock): Unit =
      designMembers += design -> current.getMemberList.drop(1)
      stack.head.injectDC(current)
      stack.head.addMember(design)
      // stack.head.refTable += design.ownerRef -> current.refTable(design.ownerRef)
      current = stack.head
      stack = stack.drop(1)
    def saveDefInputs(inputs: List[DFValAny]): Unit =
      current.defInputs = inputs
    def getDefInput(idx: Int): DFValAny =
      current.defInputs(idx)
    // for testing purposes only
    def getMembersNum: Int = current.members.size
    def getMembers(from: Int, until: Int): List[DFMember] =
      current.members.view.slice(from, until).filterNot(e => e._3).map(e => e._1).toList
    def getLastDesignInst: DFDesignBlock =
      current.members.view.reverse.collectFirst { case MemberEntry(d: DFDesignBlock, _, _) =>
        d
      }.get
  end DesignContext

  object OwnershipContext:
    private var stack: List[DFOwner] = Nil
    private var lateStack: List[Boolean] = Nil
    def enter(owner: DFOwner): Unit =
//      println(s"enter ${owner}")
      stack = owner :: stack
      lateStack = false :: lateStack
      owner match
        case design: DFDesignBlock =>
        // DesignContext.startDesign(design)
        case _ =>
    def exit(): Unit =
      // println(s"exit ${owner}")
      owner match
        case design: DFDesignBlock =>
          DesignContext.endDesign(design)
        case _ =>
      stack = stack.drop(1)
      lateStack = lateStack.drop(1)
    def exitLastDesign(): Unit =
      stack match
        case (design: DFDesignBlock) :: Nil => exit()
        case _                              =>
    def enterLate(): Unit =
      lateStack = true :: lateStack
    def exitLate(): Unit =
      lateStack = lateStack.drop(1)
    def owner: DFOwner = stack.head
    def lateConstruction: Boolean = lateStack.headOption.getOrElse(false)
    def replaceOwner(originalOwner: DFOwner, newOwner: DFOwner): Unit =
      stack = stack.map { o =>
        if (o == originalOwner) newOwner
        else o
      }
    def ownerOption: Option[DFOwner] = stack.headOption
  end OwnershipContext

  object GlobalTagContext:
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
  end GlobalTagContext

  def addMember[M <: DFMember](member: M): M =
    dirtyDB()
    member match
      case design: DFDesignBlock =>
        DesignContext.startDesign(design)
      case _ =>
    DesignContext.current.addMember(member)

  // same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
  def plantMember[M <: DFMember](owner: DFOwner, member: M): M =
    dirtyDB()
    DesignContext.current.plantMember(owner, member)

  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R =
    dirtyDB()
    DesignContext.current.newRefFor(ref, member)

  def getMember[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): M0 =
    // by default the current design context is searched
    val member = DesignContext.current.refTable.get(ref) match
      case Some(member) => member
      // if we didn't find it, then we go up the design context stack
      case None =>
        DesignContext.stack.view
          .map(_.refTable.get(ref))
          .collectFirst { case Some(member) => member }
          // finally, if still no member is available, then we check the
          // external injected meta-programming context
          .getOrElse(metaGetSetOpt.get(ref))
    member.asInstanceOf[M0]
  end getMember

  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
    dirtyDB()
    DesignContext.current.setMember(originalMember, newMemberFunc)

  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
    dirtyDB()
    DesignContext.current.replaceMember(originalMember, newMember)

  def ignoreMember[M <: DFMember](
      member: M
  ): M = // ignoring it means removing it for the immutable DB
    dirtyDB()
    DesignContext.current.ignoreMember(member)

  private def dirtyDB(): Unit = memoizedDB = None
  private var memoizedDB: Option[DB] = None

  def getFlattenedMemberList(topMemberList: List[DFMember]): List[DFMember] =
    def flattenMembers(owner: DFMember): List[DFMember] = owner match
      case o: DFDesignBlock =>
        o :: DesignContext.designMembers.getOrElse(o, Nil).flatMap(flattenMembers)
      case member => List(member)

    topMemberList.flatMap(flattenMembers)

  def immutable: DB = memoizedDB.getOrElse {
    // meta programming designs are not automatically exited
    // if (metaGetSetOpt.nonEmpty)
    //   OwnershipContext.exitLastDesign()
    // println(DesignContext.designMembers.map { case (d, m) =>
    //   s"""${d}:${m.mkString("\n  ", "\n  ", "\n")}"""
    // }.mkString("\n"))
    val members =
      if (metaGetSetOpt.nonEmpty)
        DesignContext.current.touchLazyRefs()
        DesignContext.current.getMemberList
      else getFlattenedMemberList(DesignContext.current.getMemberList)
    val refTable = DesignContext.current.refTable.toMap
    val globalTags = GlobalTagContext.tagMap.toMap
    val db = DB(members, refTable, globalTags, Nil)
    memoizedDB = Some(db)
    db
  }

  given getSet: MemberGetSet with
    def designDB: DB = immutable
    def apply[M <: DFMember, M0 <: M](
        ref: DFRef[M]
    ): M0 = getMember(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      setMember(originalMember, newMemberFunc)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M =
      replaceMember(originalMember, newMember)
    def remove[M <: DFMember](member: M): M = ignoreMember(member)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = GlobalTagContext.set(taggedElement, tag)
    def getGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any
    ): Option[CT] = GlobalTagContext.get(taggedElement)
  end getSet

end MutableDB
