package dfhdl.core
import dfhdl.internals.*
import dfhdl.hw
import dfhdl.compiler.ir.{
  DB,
  DFDesignInst,
  DFDesignInstOld,
  DFDesignBlock,
  DFMember,
  DFOwner,
  DFRef,
  DFRefAny,
  StaticRef,
  DFTag,
  DFVal,
  DFType,
  DomainBlock,
  MemberGetSet,
  SourceFile,
  MemberView,
  DFTags,
  DFDomainOwner,
  Meta
}
import dfhdl.compiler.analysis.filterPublicMembers

import scala.reflect.{ClassTag, classTag}
import collection.mutable
import collection.immutable.ListMap

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
  var defParams = List.empty[DFValAny]
  val loopIterMap = mutable.Map.empty[Meta, DFValAny]

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
    // For DFDesignBlock, `copy` creates a fresh instance whose private
    // `designInstCache` is None. Transfer the pre-copy cache so elaboration
    // lookups via `designBlock.getDesignInst` keep working across replaces
    // (e.g., `tag`/`setName` updating a design block's meta/tags).
    (originalMemberUpdated, newMember) match
      case (orig: DFDesignBlock, upd: DFDesignBlock) =>
        upd.copyDesignInstCacheFrom(orig)
      case _ =>
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

  def hasMember(member: DFMember): Boolean = memberTable.contains(member)

  def getMemberRefs(member: DFMember): Set[DFRefAny] =
    memberTable.get(member).map(idx => members(idx).refSet).getOrElse(Set.empty)

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
      // the group head a (previously ended) child design belongs to, by identity
      def groupHeadOf(d: DFDesignBlock): Option[DFDesignBlock] =
        uniqueDesigns.get(d.dclName).flatMap(_.find(_.exists(_ eq d))).map(_.head)
      def sameDesignAs(groupDesign: DFDesignBlock): Boolean =
        if (design.dclMeta == groupDesign.dclMeta)
          val otherMembers = designMembers(groupDesign)
          currentMembers =~ otherMembers &&
          // a `=~` member comparison sees only the child design HEADERS (same dclMeta),
          // not their bodies, which may have diverged (e.g. impure-param data folding),
          // so corresponding child designs must also belong to the same body group
          currentMembers.lazyZip(otherMembers).forall {
            case (a: DFDesignBlock, b: DFDesignBlock) =>
              (a eq b) || groupHeadOf(a).exists(ah => groupHeadOf(b).exists(_ eq ah))
            case _ => true
          }
        else false
      uniqueDesigns.get(designType) match
        // this design type already exists and has at least one group
        case Some(groupList) =>
          // searching for the first group of designs that has the same members
          val updatedGroupList = groupList.map { group =>
            if (!isDuplicate && sameDesignAs(group.head))
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
      // If this design is a duplicate, we retain only the public members (ports, design
      // parameters, domain blocks, and their dependencies) during elaboration, because
      // user code may still reference them (e.g., connecting to a port requires the Dcl
      // before a PortByNameSelect is created). These public members are later removed
      // during immutable DB creation (see `immutable`).
      if (isDuplicate)
        val publicMembers = currentMembers.filterPublicMembers
        designMembers += design -> publicMembers
        val transferredRefs =
          // getting the design references to parameters
          design.getRefs.map(r => r -> currentRefTable(r)) ++
            publicMembers.view.flatMap(m =>
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
    def getDefInput(idx: Int): DFValAny =
      current.defInputs(idx)
    def getDefParam(idx: Int): DFValAny =
      current.defParams(idx)
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
    // containers are frontend for IR owners, which may change in the course of the elaboration.
    // however, once a reference of an owner is constructed, it is guaranteed to be valid until the elaboration is complete.
    // this map is used to store the most recent containerized owner for each reference.
    private var refContainerizedOwnerMap = mutable.Map.empty[DFRefAny, DFDomainOwner]
    def enter(owner: DFOwner): Unit =
//      println(s"enter ${owner}")
      owner match
        case domainOwner: DFDomainOwner =>
          refContainerizedOwnerMap += domainOwner.ownerRef -> domainOwner
        case _ =>
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
      originalOwner match
        case domainOwner: DFDomainOwner =>
          refContainerizedOwnerMap.update(
            domainOwner.ownerRef,
            newOwner.asInstanceOf[DFDomainOwner]
          )
        case _ =>
    def containerizedOwnerOfRef(ref: DFRefAny): DFDomainOwner = refContainerizedOwnerMap(ref)
    def ownerOption: Option[DFOwner] = stack.headOption
  end OwnershipContext

  object ResourceOwnershipContext:
    import dfhdl.platforms.resources.*
    import dfhdl.compiler.ir.annotation.HWAnnotation
    import dfhdl.compiler.ir.constraints.SigConstraint
    private var topResourceOwners: List[ResourceOwner] = Nil
    private var stack: List[ResourceOwner] = Nil
    private val connectedDclResourceMap = mutable.Map.empty[DFVal.Dcl, List[(Range, Resource)]]
    private val connectedDomainOwnerMap = mutable.Map.empty[DFRefAny, ClkResource]
    def getConnectedDclResourceMap: Map[DFVal.Dcl, List[(Range, Resource)]] =
      connectedDclResourceMap.toMap
    def connectDclResource(dcl: DFVal.Dcl, range: Range, resource: Resource): Unit =
      connectedDclResourceMap.updateWith(dcl) {
        case Some(connections) => Some((range, resource) :: connections)
        case None              => Some(List((range, resource)))
      }
    def connectDomainOwner(domainOwner: DFDomainOwner, clkResource: ClkResource): Unit =
      connectedDomainOwnerMap.updateWith(domainOwner.ownerRef) {
        case Some(clkResource) =>
          throw new IllegalArgumentException(
            s"Domain owner ${domainOwner.getFullName} already has a clock resource ${clkResource.getFullId}"
          )
        case None => Some(clkResource)
      }
    def replaceDcl(fromPort: DFVal.Dcl, toPort: DFVal.Dcl): Unit =
      connectedDclResourceMap.get(fromPort) match
        case Some(connections) =>
          connectedDclResourceMap -= fromPort
          connectedDclResourceMap += toPort -> connections
        case None => // do nothing
    /** The effective signal constraints for a declaration: those already on its `meta` merged with
      * those contributed by any connected platform resources. The resource-derived constraints are
      * only written onto the member during [[getConstrainedDcls]] at DB commit; this exposes the
      * same result on demand (e.g. for elaboration-time inspection of a port's pin locations).
      */
    def getDclSigConstraints(dcl: DFVal.Dcl): List[SigConstraint] =
      // assuming constrained dcls have known width
      val dclWidth = dcl.widthIntOpt.get
      // existing constraints already on the declaration's meta
      val existingSigConstraints = dcl.meta.annotations.collect { case cs: SigConstraint => cs }
      // collect all constraints from the resources that are connected to this dcl
      val newSigConstraints = connectedDclResourceMap.getOrElse(dcl, Nil).flatMap {
        case (range, resource) =>
          if (range.length != dclWidth) resource.allSigConstraints.flatMap { cs =>
            for (i <- range) yield cs.updateBitIdx(i)
          }
          else resource.allSigConstraints
      }
      // merge the existing constraints with the new constraints
      (existingSigConstraints ++ newSigConstraints).merge.consolidate(dclWidth)
    end getDclSigConstraints
    def getConstrainedDcls(): Map[DFVal.Dcl, DFVal.Dcl] =
      connectedDclResourceMap.map { case (dcl, _) =>
        // preserve non-SigConstraint annotations, replacing the SigConstraints with the merged set
        val otherAnnotations = dcl.meta.annotations.filterNot {
          case cs: SigConstraint => true
          case _                 => false
        }
        val updatedAnnotations = getDclSigConstraints(dcl) ++ otherAnnotations
        dcl -> dcl.copy(meta = dcl.meta.copy(annotations = updatedAnnotations))
      }.toMap
    end getConstrainedDcls
    def getConstrainedDomainOwner(domainOwner: DFDomainOwner): DFDomainOwner =
      connectedDomainOwnerMap.get(domainOwner.ownerRef) match
        case Some(clkResource) =>
          // separate existing constraints from other annotations
          val (existingSigConstraints, otherAnnotations) = domainOwner.meta.annotations.partition {
            case cs: SigConstraint => true
            case _                 => false
          }.asInstanceOf[(List[SigConstraint], List[HWAnnotation])]
          val newSigConstraints = clkResource.allSigConstraints
          // merge the existing constraints with the new constraints
          val updatedSigConstraints = (existingSigConstraints ++ newSigConstraints).merge
          // preserve non-SigConstraint annotations (e.g. global constraints such as
          // DeviceID/DeviceProperties/DeviceConfig/ToolOptions) which would otherwise be dropped
          val updatedAnnotations = updatedSigConstraints ++ otherAnnotations
          val updatedMeta = domainOwner.meta.copy(annotations = updatedAnnotations)
          val updatedDomainOwner = domainOwner match
            case design: DFDesignBlock => design.copy(meta = updatedMeta)
            case domain: DomainBlock   => domain.copy(meta = updatedMeta)
          updatedDomainOwner
        case None => domainOwner
    end getConstrainedDomainOwner
    def getTopResourceOwners: List[ResourceOwner] = topResourceOwners
    def emptyTopResourceOwners(): Unit = topResourceOwners = Nil
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
        case (o: DFOwner, n: DFOwner) =>
          OwnershipContext.replaceOwner(o, n)
        case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
          ResourceOwnershipContext.replaceDcl(fromPort, toPort)
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
      case (o: DFOwner, n: DFOwner) =>
        OwnershipContext.replaceOwner(o, n)
      case (fromPort: DFVal.Dcl, toPort: DFVal.Dcl) =>
        ResourceOwnershipContext.replaceDcl(fromPort, toPort)
      case _ =>
    newMember
  end replaceMember

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

  // The dclName uniquification and duplicate-design canonicalization maps, derived from
  // the whole-run `uniqueDesigns` groups: (duplicate design -> its canonical group head,
  // design -> its dclName-renamed block copy). Shared by the flat `immutable` path and
  // the hierarchical by-construction assembly (`hierarchical`).
  private def designDedupMaps
      : (Map[DFDesignBlock, DFDesignBlock], Map[DFDesignBlock, DFDesignBlock]) =
    val dupToOrigDesignMap = mutable.Map.empty[DFDesignBlock, DFDesignBlock]
    val duplicateDesignRepMap = DesignContext.uniqueDesigns.view.flatMap {
      case (designType, groupList) =>
        groupList.view.reverse.zipWithIndex.flatMap {
          case (group, i) if group.length > 1 || groupList.length > 1 =>
            val updatedDclName =
              if (groupList.length > 1) s"${designType}_${i.toPaddedString(groupList.length)}"
              else designType
            var first = true
            val orig = group.head
            group.view.map(design =>
              if (first) first = false
              else dupToOrigDesignMap += design -> orig
              design -> design.copy(meta = design.meta.copy(nameOpt = Some(updatedDclName)))
            )
          case _ => Nil
        }
    }.toMap
    (dupToOrigDesignMap.toMap, duplicateDesignRepMap)
  end designDedupMaps

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
        val (dupToOrigDesignMap, duplicateDesignRepMap) = designDedupMaps
        // replacement map for domain owners that includes both duplicated designs and constrained domain owners
        val domainOwnerRepMap = members.collect {
          case design: DFDesignBlock =>
            design -> ResourceOwnershipContext.getConstrainedDomainOwner(
              duplicateDesignRepMap.getOrElse(design, design)
            )
          case domainOwner: DFDomainOwner =>
            domainOwner -> ResourceOwnershipContext.getConstrainedDomainOwner(domainOwner)
        }.toMap
        // apply connected resource constraints to their connected ports
        val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
        // apply the final fixes to the members:
        // 1. replace duplicate design instances and constrained domain owners
        // 2. apply connected resource constraints to their connected ports
        val finalFixFunc: DFMember => DFMember = {
          case domainOwner: DFDomainOwner => domainOwnerRepMap(domainOwner)
          case dcl: DFVal.Dcl             => constrainedDcls.getOrElse(dcl, dcl)
          case m                          => m
        }
        // Remove all remaining public members (ports, domain blocks, and their
        // dependencies) from duplicate designs. During elaboration these were kept
        // so user code could reference them, but in the immutable DB they are no
        // longer needed.
        val redundantRefs = mutable.Set.empty[DFRefAny]
        // Unify a DFDesignInst's `designRef` with its (canonical) target block's
        // `ownerRef` — the design's hierarchy / `subDBs` key. This replaces the old
        // `dupRefs` remap: a duplicate inst now points directly at the canonical
        // design's key. The previous distinct `designRef -> block` entry is no longer
        // emitted by any member and is swept by `cleanedRefTable` below. Recomputed
        // per occurrence (not memoized by object identity) so that the member-list
        // and refTable occurrences of the same inst — which may be distinct objects —
        // both map to EQUAL unified copies.
        def unifyInst(inst: DFDesignInst): DFDesignInst =
          val target =
            dupToOrigDesignMap.getOrElse(inst.designRef.asRef.get, inst.designRef.asRef.get)
          inst.copy(designRef = StaticRef(target.ownerRef))
        val finalMembers = members.flatMap {
          case m: DFVal if m.isGlobal => Some(finalFixFunc(m))
          case m: (DomainBlock | DFVal) if dupToOrigDesignMap.contains(m.getOwnerDesign) =>
            redundantRefs += m.ownerRef
            redundantRefs ++= m.getRefs
            None
          // Duplicate DFDesignBlocks are eliminated entirely from the
          // immutable DB. Their DFDesignInsts have been rewired to the
          // canonical above, so the duplicate block itself has no remaining
          // role. Its refs (ownerRef + meta/domainType refs) are dropped
          // alongside its members below.
          case d: DFDesignBlock if dupToOrigDesignMap.contains(d) =>
            redundantRefs += d.ownerRef
            redundantRefs ++= d.getRefs
            None
          case designInst: DFDesignInst => Some(unifyInst(designInst))
          case m                        => Some(finalFixFunc(m))
        }
        // Every non-top sub-design should behave as a Top in the immutable
        // DB. We don't change the block instance itself; instead we remap
        // its ownerRef in the refTable to DFMember.Empty. The DFDesignInst
        // is the sole per-use-site marker that remains owned by the parent
        // design.
        val designBlockOwnerRefs = finalMembers.iterator.collect {
          case d: DFDesignBlock if !d.ownerRef.isInstanceOf[DFRef.Empty] =>
            d.ownerRef: DFRefAny
        }.toSet
        val finalRefTable = fixedRefTable.view.flatMap { case (ref, member) =>
          if (redundantRefs.contains(ref)) None
          else if (designBlockOwnerRefs.contains(ref)) Some(ref -> DFMember.Empty)
          else
            member match
              case inst: DFDesignInst => Some(ref -> unifyInst(inst))
              case _                  => Some(ref -> finalFixFunc(member))
        }.toMap
        (finalMembers, finalRefTable)
    val membersNoGlobalCtx = members.map {
      case m: DFVal.CanBeGlobal  => m.copyWithoutGlobalCtx
      case design: DFDesignBlock =>
        design.clearDesignInstCache()
        design
      case m => m
    }
    val globalTags = GlobalTagContext.tags
    // Drop orphan OneWay.Gen refs — refTable entries whose key is no live
    // member's ownerRef. Elaboration scaffolding (especially meta-design /
    // cloneAnon paths) can leak these; they're safe to drop because no
    // member emits them.
    val cleanedRefTable =
      val memberOwnerRefs = mutable.Set.empty[DFRefAny]
      membersNoGlobalCtx.foreach { m =>
        memberOwnerRefs += m.ownerRef
        m match
          // DFDesignInst's designRef is a OneWay.Gen ref outside of getRefs
          // (which only carries TwoWay refs); keep it from being swept away.
          case inst: DFDesignInst => memberOwnerRefs += inst.designRef.asRef
          case _                  =>
      }
      refTable.filter { (r, _) =>
        r match
          case _: DFRef.OneWay.Gen[?] => memberOwnerRefs.contains(r)
          case _                      => true
      }
    end cleanedRefTable
    val db = DB(membersNoGlobalCtx, cleanedRefTable, globalTags, Nil)
    memoizedDB = Some(db)
    db
  }

  // ~~~ hierarchical DB by construction ~~~
  // Assembles the final hierarchical DB DIRECTLY from the mutable model: the per-design
  // member snapshots that `endDesign` finalized (`DesignContext.designMembers`), the
  // merged run refTable, and the global context. The whole-run final fixes (dclName
  // uniquification, duplicate dropping, instance unification, resource constraints)
  // apply per member and per ref target at assembly time. No flat DB is involved: this
  // replaces the flatten + re-partition round trip (`immutable` followed by
  // `DB.oldToNew`) and proves the substrate for caching a design at its end: a design's
  // end-of-elaboration snapshot IS its final sub-DB content, modulo those fixes.
  // VALIDATION PHASE: the flat path remains as the REFERENCE ONLY; this assembly must
  // equal `immutable.oldToNew` exactly (`verifyHierarchicalConstruction`, soaked
  // suite-wide). The end state makes `immutable` itself return this hierarchical DB and
  // retires the flat form.
  def hierarchical: DB =
    require(!inMetaProgramming, "hierarchical DB construction is undefined in meta-programming")
    // the run's merged state: the (ended) top-level context member list, which holds
    // the injected globals and the top design block, and the run-wide merged refTable
    val topMemberList = DesignContext.current.getImmutableMemberList
    val rawRefTable = DesignContext.current.getImmutableRefTable
    val (dupToOrigDesignMap, duplicateDesignRepMap) = designDedupMaps
    val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
    // the whole-run final fix of a single member (mirrors `immutable`'s finalFixFunc,
    // applied per snapshot member instead of over the flat list)
    def fixedMember(m: DFMember): DFMember = m match
      case design: DFDesignBlock =>
        ResourceOwnershipContext.getConstrainedDomainOwner(
          duplicateDesignRepMap.getOrElse(design, design)
        )
      case domainOwner: DFDomainOwner =>
        ResourceOwnershipContext.getConstrainedDomainOwner(domainOwner)
      case dcl: DFVal.Dcl => constrainedDcls.getOrElse(dcl, dcl)
      case m              => m
    // a DFDesignInst points at its canonical design's key (see `immutable`'s unifyInst)
    def unifyInst(inst: DFDesignInst): DFDesignInst =
      val target =
        dupToOrigDesignMap.getOrElse(inst.designRef.asRef.get, inst.designRef.asRef.get)
      inst.copy(designRef = StaticRef(target.ownerRef))
    def isLive(d: DFDesignBlock): Boolean = !dupToOrigDesignMap.contains(d)
    def snapshotOf(d: DFDesignBlock): List[DFMember] =
      DesignContext.designMembers.getOrElse(d, Nil)
    // ~~~ parent-to-children claims ~~~
    // The canonical owner of a child's sub-DB is the design containing the FIRST
    // DFDesignInst (in elaboration order) targeting the child. The scan visits members
    // in exactly the flat elaboration order: a design's snapshot in order, recursing
    // into live child designs at their block's position (duplicate children hold only
    // their public snapshot, with no instances or child blocks, and are skipped).
    val parentToChildren =
      mutable.LinkedHashMap.empty[DFDesignBlock, mutable.ListBuffer[DFDesignBlock]]
    val claimed = mutable.Set.empty[DFDesignBlock]
    def scanClaims(d: DFDesignBlock): Unit =
      snapshotOf(d).foreach {
        case b: DFDesignBlock   => if (isLive(b)) scanClaims(b)
        case inst: DFDesignInst =>
          val target = inst.designRef.asRef.get
          val child = dupToOrigDesignMap.getOrElse(target, target)
          if (claimed.add(child))
            parentToChildren.getOrElseUpdate(d, mutable.ListBuffer.empty) += child
        case _ =>
      }
    // ~~~ globals and refs, from the mutable model directly ~~~
    // Globals are injected into the top-level context on first use, so their raw
    // elaboration order is their order there. The closure walks the RAW refTable and
    // raw global objects; global-ctx cleanup applies at emission (as the flat path's
    // final members pass does).
    val allGlobalsOrderedRaw: List[DFVal.CanBeGlobal] = topMemberList.collect {
      case g: DFVal.CanBeGlobal if g.isGlobal => g
    }
    def cleanedGlobals(globals: List[DFVal.CanBeGlobal]): List[DFMember] =
      globals.map(_.copyWithoutGlobalCtx)
    def globalsClosure(localMembers: Iterable[DFMember]): List[DFMember] =
      val reachable = mutable.Set.empty[DFMember]
      def pull(target: DFMember): Unit = target match
        case g: DFVal.CanBeGlobal if g.isGlobal && !reachable.contains(g) =>
          reachable += g
          g.getRefs.foreach(r => rawRefTable.get(r).foreach(pull))
        case _ =>
      localMembers.foreach { m =>
        m.getRefs.foreach(r => rawRefTable.get(r).foreach(pull))
      }
      cleanedGlobals(allGlobalsOrderedRaw.filter(reachable.contains))
    // a ref target resolved against the raw refTable with the whole-run fixes applied
    // (the flat path applies the same fixes over its refTable entries)
    def resolveFixed(r: DFRefAny): Option[DFMember] =
      rawRefTable.get(r).map {
        case inst: DFDesignInst => unifyInst(inst)
        case m                  => fixedMember(m)
      }
    // Every ref emitted by the DB's members, fix-resolved. The sub-DB's own design
    // block behaves as a Top: its ownerRef resolves to DFMember.Empty (unless it
    // already is an empty ref, i.e. the true top); the DFDesignInst in the parent is
    // the sole remaining per-use-site marker. `DFDesignInst.designRef` is deliberately
    // NOT collected: it is unified with the child's `subDBs` key and resolved
    // structurally, not through the refTable.
    def refsFor(dTop: DFDesignBlock, dbMembers: Iterable[DFMember]): Map[DFRefAny, DFMember] =
      val result = mutable.Map.empty[DFRefAny, DFMember]
      dbMembers.foreach { m =>
        val ownerTarget =
          if ((m eq dTop) && !m.ownerRef.isInstanceOf[DFRef.Empty])
            if (rawRefTable.contains(m.ownerRef)) Some(DFMember.Empty) else None
          else resolveFixed(m.ownerRef)
        ownerTarget.foreach(t => result(m.ownerRef) = t)
        m.getRefs.foreach(r => resolveFixed(r).foreach(t => result(r) = t))
      }
      result.toMap
    val globalTags = GlobalTagContext.tags
    // ~~~ sub-DB assembly, top-down in claim order ~~~
    val builtSubDBs = mutable.LinkedHashMap.empty[StaticRef, DB]
    def build(d: DFDesignBlock): Unit =
      val dFinal = fixedMember(d).asInstanceOf[DFDesignBlock]
      val locals = snapshotOf(d).flatMap {
        // child designs live in their own sub-DBs; globals join through the closure
        case _: DFDesignBlock                   => None
        case g: DFVal.CanBeGlobal if g.isGlobal => None
        case inst: DFDesignInst                 => Some(unifyInst(inst))
        case m                                  =>
          fixedMember(m) match
            case cbg: DFVal.CanBeGlobal => Some(cbg.copyWithoutGlobalCtx)
            case fixed                  => Some(fixed)
      }
      val closure = globalsClosure(dFinal :: locals)
      val dbMembers = closure ::: dFinal :: locals
      builtSubDBs(StaticRef(d.ownerRef)) =
        DB(dbMembers, refsFor(dFinal, dbMembers), globalTags, Nil)
      parentToChildren.getOrElse(d, Nil).foreach(build)
    end build
    val topOrig = topMemberList.collectFirst {
      case d: DFDesignBlock => d
    }.get
    scanClaims(topOrig)
    build(topOrig)
    // orphan globals (reached by no sub-DB closure) anchor at the top design's sub-DB
    val coveredGlobals = mutable.Set.empty[DFMember]
    builtSubDBs.valuesIterator.foreach { sub =>
      sub.members.foreach {
        case g: DFVal.CanBeGlobal if g.isGlobal => coveredGlobals += g
        case _                                  =>
      }
    }
    val orphanGlobalsRaw = allGlobalsOrderedRaw.filterNot(g =>
      coveredGlobals.contains(g.copyWithoutGlobalCtx)
    )
    if (orphanGlobalsRaw.nonEmpty)
      val orphanGlobals = cleanedGlobals(orphanGlobalsRaw)
      val topKey = StaticRef(topOrig.ownerRef)
      val topSub = builtSubDBs(topKey)
      builtSubDBs(topKey) = topSub.update(
        members = orphanGlobals ::: topSub.members,
        refTable = topSub.refTable ++ refsFor(topOrig, orphanGlobals)
      )
    DB(Nil, Map.empty, globalTags, Nil, ListMap.from(builtSubDBs))
  end hierarchical

  // VALIDATION PHASE ONLY: asserts that the by-construction assembly (`hierarchical`)
  // equals the flat path (`immutable.oldToNew`) EXACTLY, piecewise for actionable
  // failure messages. Wired into the test-suite bases (StageSpec, DesignSpec) to soak
  // the equivalence across every elaborated design; removed when the flip makes
  // `hierarchical` the only path.
  def verifyHierarchicalConstruction(): Unit =
    val flat = immutable.oldToNew
    val byConstruction = hierarchical
    def mismatch(msg: String): Nothing =
      throw new AssertionError(s"Hierarchical-by-construction mismatch! $msg")
    if (!byConstruction.equals(flat))
      val bKeys = byConstruction.subDBs.keys.toList
      val fKeys = flat.subDBs.keys.toList
      if (!bKeys.equals(fKeys))
        mismatch(s"subDB keys/order:\n  built: $bKeys\n  flat:  $fKeys")
      byConstruction.subDBs.lazyZip(flat.subDBs).foreach { case ((_, b), (_, f)) =>
        val name = f.top.dclName
        b.members.lazyZip(f.members).zipWithIndex.foreach { case ((bm, fm), i) =>
          if (!bm.equals(fm)) mismatch(s"member $i of `$name`:\n  built: $bm\n  flat:  $fm")
        }
        if (b.members.length != f.members.length)
          mismatch(s"member count of `$name`: built ${b.members.length}, flat ${f.members.length}")
        if (!b.refTable.equals(f.refTable)) mismatch(s"refTable of `$name`")
      }
      mismatch("root DBs differ")
    end if
  end verifyHierarchicalConstruction

  given getSet: MemberGetSet with
    val isMutable: Boolean = true
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
