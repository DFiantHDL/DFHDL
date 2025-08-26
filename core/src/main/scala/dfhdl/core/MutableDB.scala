package dfhdl.core
import dfhdl.internals.*
import dfhdl.hw
import dfhdl.compiler.ir.{
  DB,
  DuplicateTag,
  DFDesignInst,
  DFDesignBlock,
  DFMember,
  DFOwner,
  DFRef,
  DFRefAny,
  DFTag,
  DFVal,
  DFType,
  DomainBlock,
  MemberGetSet,
  SourceFile,
  MemberView,
  RTDomainCfg,
  DFTags,
  annotation
}
import dfhdl.compiler.analysis.filterPublicMembers

import scala.reflect.{ClassTag, classTag}
import collection.mutable
import dfhdl.compiler.ir.Meta

private case class MemberEntry(
    irValue: DFMember,
    refSet: Set[DFRefAny],
    ignore: Boolean
)

class DesignContext:
  val members = mutable.ArrayBuffer.empty[MemberEntry]
  val memberTable = mutable.Map.empty[DFMember, Int]
  val refTable = mutable.Map.empty[DFRefAny, DFMember]
  val originRefTable = mutable.Map.empty[DFRef.TwoWayAny, DFMember]
  val unreachableNamedValues = mutable.Map.empty[DFVal, DFVal]
  val unreachableDFTypes = mutable.Map.empty[DFType, DFType]
  var defInputs = List.empty[DFValAny]
  val loopIterMap = mutable.Map.empty[Meta, DFValAny]
  var isDuplicate = false

  def setOriginRefs(member: DFMember): Unit =
    member.getRefs.foreach { r => originRefTable += r -> member }

  def addMember[M <: DFMember](member: M): M =
    memberTable += (member -> members.length)
    members += MemberEntry(member, Set(), false)
    setOriginRefs(member)
    member
  end addMember

  // same as addMember, but if the member is at design-level scope,
  // the ownerRef needs to be added, referring to the meta designer owner.
  def plantMember[M <: DFMember](
      owner: DFOwner | DFMember.Empty,
      member: M,
      updateOwnerCond: DFOwner => Boolean = _.isInstanceOf[DFDesignBlock]
  )(using MemberGetSet): M =
    if (owner == DFMember.Empty || updateOwnerCond(member.getOwner))
      // now this reference will refer to meta design owner
      newRefFor[DFOwner | DFMember.Empty, DFOwner.Ref](
        member.ownerRef,
        owner
      )
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
    val memberEntry = members(idx)
    // update all references to the new member
    memberEntry.refSet.foreach(r => refTable.update(r, newMember))
    // add the member to the table with the position index
    // (we don't remove the old member since it might still be used as a user-reference in a mutable DB)
    memberTable.update(newMember, idx)
    // update the member in the member position array
    members.update(idx, memberEntry.copy(irValue = newMember))
    // update the origin references to the new member
    setOriginRefs(newMember)
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

  def getLatestMember: DFMember =
    members.view.filterNot(e => e.ignore).map(e => e.irValue).head

  def inject(sourceCtx: DesignContext): Unit =
    sourceCtx.getImmutableMemberList.foreach { m =>
      if (!memberTable.contains(m))
        addMember(m)
    }
    refTable ++= sourceCtx.refTable
    originRefTable ++= sourceCtx.originRefTable
  end inject

  def getImmutableMemberList: List[DFMember] =
    members.view.filterNot(e => e.ignore).map(e => e.irValue).toList

  def getImmutableRefTable: Map[DFRefAny, DFMember] =
    refTable.toMap

  def getReachableNamedValue(dfVal: DFVal, cf: => DFVal): DFVal =
    unreachableNamedValues.getOrElseUpdate(dfVal, cf)

  def getReachableDFType(dfType: DFType, cf: => DFType): DFType =
    unreachableDFTypes.getOrElseUpdate(dfType, cf)
end DesignContext

final class MutableDB():
  private val self = this

  // error logger
  val logger = new Logger

  // meta programming external MemberGetSet DB access
  private[MutableDB] var metaGetSetList: List[MemberGetSet] = Nil
  def inMetaProgramming: Boolean = metaGetSetList.nonEmpty
  def injectMetaGetSet(metaGetSet: MemberGetSet): Unit =
    metaGetSetList = metaGetSet :: metaGetSetList

  object DesignContext:
    val global: DesignContext = new DesignContext
    var current: DesignContext = global
    var stack = List.empty[DesignContext]
    val designMembers = mutable.Map.empty[DFDesignBlock, List[DFMember]]
    val uniqueDesigns = mutable.Map.empty[String, List[List[DFDesignBlock]]]
    def startDesign(design: DFDesignBlock): Unit =
      stack = current :: stack
      current = new DesignContext
    def endDesign(design: DFDesignBlock): Unit =
      val currentMembers = current.getImmutableMemberList.drop(1)
      val currentRefTable = current.getImmutableRefTable
      val designType = design.dclName
      var isDuplicate = false
      def sameDesignAs(groupDesign: DFDesignBlock): Boolean =
        if (design.dclMeta == groupDesign.dclMeta)
          val groupMembers = designMembers(groupDesign)
          if (currentMembers.length == groupMembers.length)
            (currentMembers lazyZip groupMembers).forall { case (l, r) => l =~ r }
          else false
        else false
      uniqueDesigns.get(designType) match
        // this design type already exists and has at least one group
        case Some(groupList) =>
          // searching for the first group of designs that has the same members
          val updatedGroupList = groupList.map { group =>
            if (current.isDuplicate || !isDuplicate && sameDesignAs(group.head))
              isDuplicate = true
              // the head of each group will always be the first design discovered
              // from that group and it keeps all its elements and not marked as a duplicate.
              group.head :: design :: group.drop(1)
            else group
          }
          if (isDuplicate) uniqueDesigns += designType -> updatedGroupList
          // a new group was discovered so we add it to the group list
          else uniqueDesigns += designType -> (List(design) :: groupList)
        // first time encountering this design type, so add the first group
        case None => uniqueDesigns += designType -> List(List(design))
      end match
      // generally, if this design is a duplicate we want to add only the by-name members and their
      // owner references. however, if current design context is known to be a duplicate (as a result
      // of a `hw.pure` annotation), then we can skip this extra step since the design context is
      // already minimized to the named members.
      if (isDuplicate && !current.isDuplicate)
        // public members are ports, design design parameters, and
        // design domains. for design parameters we also get dependencies.
        // all these members are interacted with outside the design,
        // so they are kept as duplicates in the design instances
        val publicMembers = currentMembers.filterPublicMembers
        designMembers += design -> publicMembers
        val transferredRefs = publicMembers.view.flatMap(m =>
          (m.ownerRef -> currentRefTable(m.ownerRef)) ::
            m.getRefs.map(r => r -> currentRefTable(r))
        )
        stack.head.refTable ++= transferredRefs
      else
        designMembers += design -> currentMembers
        stack.head.refTable ++= currentRefTable
      end if

      stack.head.addMember(design)
      current = stack.head
      stack = stack.drop(1)
    end endDesign
    val pureDesignDefOutCache = mutable.Map.empty[(Position, List[DFType]), DFValAny]
    def runFuncWithInputs[V <: DFValAny](func: => V, inputs: List[DFValAny]): (Boolean, V) =
      current.defInputs = inputs
      val currentDesign = OwnershipContext.currentDesign
      val isPure = currentDesign.dclMeta.annotations.exists {
        case annotation.Pure => true
        case _               => false
      }
      if (isPure)
        val key = (currentDesign.dclMeta.position, inputs.map(_.dfType.asIR))
        pureDesignDefOutCache.get(key) match
          case Some(ret) =>
            current.isDuplicate = true
            (true, ret.asInstanceOf[V])
          case None =>
            val ret = func
            pureDesignDefOutCache += key -> ret
            (false, ret)
      else (false, func)
    end runFuncWithInputs
    def getDefInput(idx: Int): DFValAny =
      current.defInputs(idx)
    def addLoopIter(meta: Meta, iter: DFValAny): Unit =
      current.loopIterMap += meta -> iter
    def getLoopIter(meta: Meta): DFValAny =
      current.loopIterMap(meta)
    // for testing purposes only
    def getMembersNum: Int = current.members.size
    def getMembers(from: Int, until: Int): List[DFMember] =
      current.members.view.slice(from, until).filterNot(e => e._3).map(e => e._1).toList
    def getLastMembers(cnt: Int): List[DFMember] =
      current.members.view.reverse.filterNot(e => e._3).map(e => e._1).take(cnt).toList.reverse
    def getLastDesignInst: DFDesignBlock =
      current.members.view.reverse.collectFirst { case MemberEntry(irValue = d: DFDesignBlock) =>
        d
      }.get
    def getReachableNamedValue(dfVal: DFVal, cf: => DFVal): DFVal =
      current.getReachableNamedValue(dfVal, cf)
    def getReachableDFType(dfType: DFType, cf: => DFType): DFType =
      current.getReachableDFType(dfType, cf)
  end DesignContext

  val injectedCtx = mutable.Set.empty[DesignContext]
  def injectGlobals(sourceCtx: DesignContext): Unit =
    // preventing meta-programming global injection to avoid duplicates
    if (!inMetaProgramming && !injectedCtx.contains(sourceCtx))
      injectedCtx += sourceCtx
      DesignContext.global.inject(sourceCtx)

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
    def currentDesign: DFDesignBlock = stack.collectFirst { case d: DFDesignBlock => d }.get
    def lateConstruction: Boolean = lateStack.headOption.getOrElse(false)
    def replaceOwner(originalOwner: DFOwner, newOwner: DFOwner): Unit =
      stack = stack.map { o =>
        if (o == originalOwner) newOwner
        else o
      }
    def ownerOption: Option[DFOwner] = stack.headOption
  end OwnershipContext

  object ResourceOwnershipContext:
    import dfhdl.platforms.resources.*
    private var topResourceOwners: List[ResourceOwner] = Nil
    private var stack: List[ResourceOwner] = Nil
    private val connectedResourceMap = mutable.Map.empty[DFVal.Dcl, List[(Range, Resource)]]
    def getConnectedResourceMap: Map[DFVal.Dcl, List[(Range, Resource)]] =
      connectedResourceMap.toMap
    def connectResource(dcl: DFVal.Dcl, range: Range, resource: Resource): Unit =
      connectedResourceMap.updateWith(dcl) {
        case Some(connections) => Some((range, resource) :: connections)
        case None              => Some(List((range, resource)))
      }
    def updateDcl(fromPort: DFVal.Dcl, toPort: DFVal.Dcl): Unit =
      connectedResourceMap.get(fromPort) match
        case Some(connections) =>
          connectedResourceMap -= fromPort
          connectedResourceMap += toPort -> connections
        case None => // do nothing
    def getConstrainedDcls(): Map[DFVal.Dcl, DFVal.Dcl] =
      import dfhdl.compiler.ir.annotation.HWAnnotation
      import dfhdl.compiler.ir.constraints.SigConstraint
      connectedResourceMap.map { case (dcl, connections) =>
        // separate existing constraints from other annotations
        val (existingSigConstraints, otherAnnotations) = dcl.meta.annotations.partition {
          case cs: SigConstraint => true
          case _                 => false
        }.asInstanceOf[(List[SigConstraint], List[HWAnnotation])]
        // collect all constraints from the resources that are connected to this dcl
        val newSigConstraints = connections.flatMap { case (range, resource) =>
          if (range.length != dcl.width) resource.allSigConstraints.flatMap { cs =>
            for (i <- range) yield cs.updateBitIdx(i)
          }
          else resource.allSigConstraints
        }
        // merge the existing constraints with the new constraints
        val updatedSigConstraints =
          (existingSigConstraints ++ newSigConstraints).merge.consolidate(dcl.width)
        // merge all other annotations
        val updatedAnnotations = updatedSigConstraints ++ otherAnnotations
        dcl -> dcl.copy(meta = dcl.meta.copy(annotations = updatedAnnotations))
      }.toMap
    end getConstrainedDcls
    def getTopResourceOwners: List[ResourceOwner] = topResourceOwners
    def enter(owner: ResourceOwner): Unit =
      if (stack.isEmpty) topResourceOwners = owner :: topResourceOwners
      stack = owner :: stack
    def exit(): Unit =
      stack = stack.drop(1)
    def owner: ResourceOwner = stack.head
    def ownerOpt: Option[ResourceOwner] = stack.headOption
  end ResourceOwnershipContext

  object GlobalTagContext:
    private[MutableDB] var tags: DFTags = DFTags.empty
    def set[CT <: DFTag: ClassTag](tag: CT): Unit = tags = tags.tag(tag)
    def get[CT <: DFTag: ClassTag]: Option[CT] = tags.getTagOf[CT]
  end GlobalTagContext

  def addMember[M <: DFMember](member: M): M =
    dirtyDB()
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        dfVal.globalCtx = DesignContext.global
      case design: DFDesignBlock =>
        DesignContext.startDesign(design)
      case _ =>
    DesignContext.current.addMember(member)

  // same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
  def plantMember[M <: DFMember](
      owner: DFOwner | DFMember.Empty,
      member: M,
      updateOwnerCond: DFOwner => Boolean = _.isInstanceOf[DFDesignBlock]
  ): M =
    dirtyDB()
    DesignContext.current.plantMember(owner, member, updateOwnerCond) // (using metaGetSetList.last)

  def newRefFor[M <: DFMember, R <: DFRef[M]](ref: R, member: M): R =
    dirtyDB()
    DesignContext.current.newRefFor(ref, member)

  def getMemberOption[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): Option[M0] =
    // by default the current design context is searched
    val memberOption: Option[DFMember] = DesignContext.current.refTable.get(ref) match
      case some: Some[DFMember] => some
      // if we didn't find it, then we go up the design context stack
      case None =>
        DesignContext.stack.view
          .map(_.refTable.get(ref))
          .collectFirst { case Some(member) => member }
          // finally, if still no member is available, then we check the
          // external injected meta-programming context
          .orElse(metaGetSetList.view.flatMap(_.getOption(ref)).headOption)
    memberOption.asInstanceOf[Option[M0]]
  end getMemberOption

  def getMember[M <: DFMember, M0 <: M](
      ref: DFRef[M]
  ): M0 = getMemberOption(ref).getOrElse(
    throw new IllegalArgumentException(s"Missing ref $ref")
  )

  def getOriginMember(
      ref: DFRef.TwoWayAny
  ): DFMember =
    // by default the current design context is searched
    val member = DesignContext.current.originRefTable.get(ref) match
      case Some(member) => member
      // if we didn't find it, then we go up the design context stack
      case None =>
        DesignContext.stack.view
          .map(_.originRefTable.get(ref))
          .collectFirst { case Some(member) => member }
          // finally, if still no member is available, then we check the
          // external injected meta-programming context
          .getOrElse(
            metaGetSetList.view.flatMap(_.getOption(ref)).headOption.getOrElse(
              throw new IllegalArgumentException(s"Missing ref $ref")
            )
          )
    member
  end getOriginMember

  private def globalMemberCtxUpdate(member: DFMember): Unit =
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        dfVal.globalCtx = DesignContext.global
      case _ =>

  // if the original member is global, then injects its context into
  // the current context
  private def globalMemberCtxInject(member: DFMember): Unit =
    member match
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal =>
        injectGlobals(dfVal.globalCtx.asInstanceOf[DesignContext])
      case _ =>

  def setMember[M <: DFMember](originalMember: M, newMemberFunc: M => M): M =
    if (inMetaProgramming) newMemberFunc(originalMember)
    else
      dirtyDB()
      globalMemberCtxInject(originalMember)
      val newMember = DesignContext.current.setMember(originalMember, newMemberFunc)
      globalMemberCtxUpdate(newMember)
      // in case the member is an owner, we check the owner stack to replace it.
      // and for ports, we update the connected resource map.
      (originalMember, newMember) match
        case (o: DFOwner, n: DFOwner)                 => OwnershipContext.replaceOwner(o, n)
        case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
          ResourceOwnershipContext.updateDcl(fromPort, toPort)
        case _ =>
      newMember
  end setMember

  def replaceMember[M <: DFMember](originalMember: M, newMember: M): M =
    dirtyDB()
    globalMemberCtxInject(originalMember)
    DesignContext.current.replaceMember(originalMember, newMember)
    globalMemberCtxUpdate(newMember)
    // in case the member is an owner, we check the owner stack to replace it.
    // and for ports, we update the connected resource map.
    (originalMember, newMember) match
      case (o: DFOwner, n: DFOwner)                 => OwnershipContext.replaceOwner(o, n)
      case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
        ResourceOwnershipContext.updateDcl(fromPort, toPort)
      case _ =>
    newMember

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
    // if in meta-programming (indicated by the existence of an external context),
    // then we need to just get the current hierarchy members and refTable
    val (members, refTable) =
      if (inMetaProgramming)
        (DesignContext.current.getImmutableMemberList, DesignContext.current.getImmutableRefTable)
      // otherwise we first flatten the hierarchy and then make sure all design
      // declarations are unique and tag duplicate instances accordingly.
      else
        val members =
          getFlattenedMemberList(DesignContext.current.getImmutableMemberList)
        val refTable = DesignContext.current.getImmutableRefTable
        // removing unused type references due to `dropUnreachableRefs`
        val usedTypeRefs = members.view.flatMap {
          case dfVal: DFVal => dfVal.getRefs.collect { case r: DFRef.TypeRef => r }
          case _            => Nil
        }.toSet
        val fixedRefTable = refTable.view.filter {
          case (ref: DFRef.TypeRef, m) => usedTypeRefs.contains(ref)
          case _                       => true
        }.toMap
        val duplicateDesignRepMap = DesignContext.uniqueDesigns.view.flatMap {
          case (designType, groupList) =>
            groupList.view.reverse.zipWithIndex.flatMap {
              case (group, i) if group.length > 1 || groupList.length > 1 =>
                val updatedDclName =
                  if (groupList.length > 1) s"${designType}_${i.toPaddedString(groupList.length)}"
                  else designType
                var first = true
                group.view.map(design =>
                  val tags =
                    if (first)
                      first = false
                      design.tags
                    else design.tags.tag(DuplicateTag)
                  design -> design.copy(
                    dclMeta = design.dclMeta.copy(nameOpt = Some(updatedDclName)),
                    tags = tags
                  )
                )
              case _ => Nil
            }
        }.toMap
        // apply connected resource constraints to their connected ports
        val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
        // apply the final fixes to the members:
        // 1. replace duplicate design instances
        // 2. apply connected resource constraints to their connected ports
        val finalFixFunc: DFMember => DFMember = {
          case design: DFDesignBlock => duplicateDesignRepMap.getOrElse(design, design)
          case dcl: DFVal.Dcl        => constrainedDcls.getOrElse(dcl, dcl)
          case m                     => m
        }
        (members.map(finalFixFunc), fixedRefTable.view.mapValues(finalFixFunc).toMap)
    val membersNoGlobalCtx = members.map {
      case m: DFVal.CanBeGlobal => m.copyWithoutGlobalCtx
      case m                    => m
    }
    val globalTags = GlobalTagContext.tags
    val db = DB(membersNoGlobalCtx, refTable, globalTags, Nil)
    memoizedDB = Some(db)
    db
  }

  given getSet: MemberGetSet with
    def designDB: DB = immutable
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 = getMember(ref)
    def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] = getMemberOption(ref)
    def getOrigin(ref: DFRef.TwoWayAny): DFMember = getOriginMember(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      setMember(originalMember, newMemberFunc)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M =
      replaceMember(originalMember, newMember)
    def remove[M <: DFMember](member: M): M = ignoreMember(member)
    def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit = GlobalTagContext.set(tag)
    def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT] = GlobalTagContext.get[CT]
  end getSet

end MutableDB
