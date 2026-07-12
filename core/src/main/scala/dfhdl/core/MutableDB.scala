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
  DFUnit,
  DomainBlock,
  MemberGetSet,
  SourceFile,
  MemberView,
  DFTags,
  DFDomainOwner,
  Meta,
  PhantomTag,
  RefGen
}

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
  // on a design-load hit (`DesignLoadGate`), the canonical design whose body this context
  // duplicates; `endDesign` joins the duplicate to the CANONICAL's group (a design may
  // have several structurally-distinct groups, one per key)
  private var _duplicateOf: Option[DFDesignBlock] = None
  def duplicateOf: Option[DFDesignBlock] = _duplicateOf
  // this context duplicates `canonical` and joins its group at `endDesign`
  def markDuplicateOf(canonical: DFDesignBlock): Unit = _duplicateOf = Some(canonical)

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
    // memoized self-contained forest per design: a design's subtree is final once it has
    // ended, so its forest is stable and the same instance serves every later lookup/store
    private val forestMemo = mutable.Map.empty[DFDesignBlock, DB]

    def startDesign(design: DFDesignBlock): Unit =
      stack = current :: stack
      current = new DesignContext
    def endDesign(design: DFDesignBlock): Unit =
      val currentMembers = current.getImmutableMemberList.drop(1)
      val currentRefTable = current.getImmutableRefTable
      val designType = design.dclName
      // ~~~ design unification ~~~
      // Designs unify ONLY through the design load gate's key (there is no structural
      // body comparison): a gate hit recorded the canonical design this context
      // duplicates (`duplicateOf`), so the ended design joins the canonical's group
      // (the group head is always the canonical); any other design, including every
      // KEYLESS one (impure or otherwise unloadable), starts a group of its own.
      // Same-dclName groups only iterate the emitted dclName (`designDedupMaps`), so a
      // keyless design instantiated N times simply emits N enumerated designs.
      current.duplicateOf match
        case Some(canonical) =>
          val groupList = uniqueDesigns(designType)
          uniqueDesigns += designType -> groupList.map { group =>
            if (group.head == canonical) group.head :: design :: group.drop(1)
            else group
          }
        case None =>
          uniqueDesigns.updateWith(designType) {
            case Some(groupList) => Some(List(design) :: groupList)
            case None            => Some(List(List(design)))
          }
      // A duplicate's members are NOT minimized here: the final assembly drops duplicate
      // designs wholesale (they are never `isLive`, their instances unify to the
      // canonical), so a duplicate's retained snapshot is simply never read.
      designMembers += design -> currentMembers
      stack.head.refTable ++= currentRefTable

      stack.head.addMember(design)
      current = stack.head
      stack = stack.drop(1)
    end endDesign
    // ~~~ the cached artifact: a design's self-contained forest at its end ~~~
    // Built (from the design-end snapshots this context owns) right after the design ends
    // (all descendants ended, snapshots final), BEFORE any whole-run fixes: natural
    // dclNames (the loading run resolves naming through the adopted shell's header), no
    // resource constraints (a cacheable def is device-independent). Content is
    // canonicalized: only canonical snapshots are walked and instances are unified to
    // their canonical, which may live OUTSIDE this design's subtree; the canonical's full
    // definition is then embedded, keeping the artifact self-contained. Refs resolve
    // through the mutable run state (the design's refTables merged into the enclosing
    // contexts); globals join each sub-DB through the same closure the final assembly
    // computes.
    def buildDesignForestDB(design: DFDesignBlock): DB =
      forestMemo.getOrElseUpdate(design, buildForest(design))
    private def buildForest(design: DFDesignBlock): DB =
      val (dupToOrigDesignMap, _) = designDedupMaps
      def canonicalDsn(d: DFDesignBlock): DFDesignBlock = dupToOrigDesignMap.getOrElse(d, d)
      def resolve(r: DFRefAny): Option[DFMember] =
        self.getMemberOption(r.asInstanceOf[DFRef[DFMember]])
      val allGlobalsOrderedRaw: List[DFVal.CanBeGlobal] =
        global.getImmutableMemberList.collect {
          case g: DFVal.CanBeGlobal if g.isGlobal => g
        }
      def globalsClosure(localMembers: Iterable[DFMember]): List[DFMember] =
        val reachable = mutable.Set.empty[DFMember]
        def pull(target: DFMember): Unit = target match
          case g: DFVal.CanBeGlobal if g.isGlobal && !reachable.contains(g) =>
            reachable += g
            g.getRefs.foreach(r => resolve(r).foreach(pull))
          case _ =>
        localMembers.foreach { m => m.getRefs.foreach(r => resolve(r).foreach(pull)) }
        allGlobalsOrderedRaw.filter(reachable.contains).map(_.copyWithoutGlobalCtx)
      // a DFDesignInst points at its canonical design's key (the child's `subDBs` key).
      // ALWAYS rewritten, even when the target already IS the canonical: the sub-DB key
      // form is what resolves structurally (`designRef` is deliberately absent from the
      // refTable), so leaving an instance's original parent-side ref in place would leave
      // it dangling in the emitted forest.
      def unifyInst(inst: DFDesignInst): DFDesignInst =
        val target = canonicalDsn(inst.designRef.asRef.get)
        inst.copy(designRef = StaticRef(target.ownerRef))
      def refsFor(dTop: DFDesignBlock, dbMembers: Iterable[DFMember]): Map[DFRefAny, DFMember] =
        val result = mutable.Map.empty[DFRefAny, DFMember]
        dbMembers.foreach { m =>
          val ownerTarget =
            if ((m eq dTop) && !m.ownerRef.isInstanceOf[DFRef.Empty])
              if (resolve(m.ownerRef).nonEmpty) Some(DFMember.Empty) else None
            else resolve(m.ownerRef)
          ownerTarget.foreach(t => result(m.ownerRef) = t)
          m.getRefs.foreach(r => resolve(r).foreach(t => result(r) = t))
        }
        result.toMap
      val built = mutable.LinkedHashMap.empty[StaticRef, DB]
      def build(c: DFDesignBlock): Unit = // c is always a canonical design
        val key = StaticRef(c.ownerRef)
        if (!built.contains(key))
          val snapshot = designMembers.getOrElse(c, Nil)
          val locals = snapshot.flatMap {
            case _: DFDesignBlock                   => None
            case g: DFVal.CanBeGlobal if g.isGlobal => None
            case inst: DFDesignInst                 => Some(unifyInst(inst))
            case cbg: DFVal.CanBeGlobal             => Some(cbg.copyWithoutGlobalCtx)
            case m                                  => Some(m)
          }
          // NOTE: the design block's transient elaboration-time instance cache is NOT
          // cleared here; the design is still live in this run (the cache is not
          // serialized into the artifact anyway)
          val closure = globalsClosure(c :: locals)
          val dbMembers = closure ::: c :: locals
          built(key) = DB(dbMembers, refsFor(c, dbMembers), GlobalTagContext.tags, Nil)
          // children: the canonicals of every instantiated design, in instance order
          // (resolved from the RAW instances; a unified designRef resolves only
          // structurally)
          snapshot.foreach {
            case inst: DFDesignInst => build(canonicalDsn(inst.designRef.asRef.get))
            case _                  =>
          }
        end if
      end build
      build(canonicalDsn(design))
      DB(Nil, Map.empty, GlobalTagContext.tags, Nil, ListMap.from(built))
    end buildForest
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

  // ~~~ the design load gate ~~~
  // Decides, per design-def instantiation, whether to run the elaboration body live or
  // skip it and reuse an already-loaded elaboration of the same key. The invariant that
  // makes skipping sound: the design's public interface (its design parameters, bound
  // fresh to this call's applied values, and its ports) is created by the HARNESS
  // (`designFromDef`) outside the body, on hit and miss alike; the body is a skippable
  // thunk. Class designs route through the gate as well, keyed at their END (their
  // body always runs live for now; the key unifies identical instantiations without
  // any structural comparison). Body-skipping and service caching for classes are the
  // next increment. Designs the gate CANNOT key (impure or otherwise unloadable) NEVER
  // unify: each instantiation emits its own (dclName-enumerated) design. The key
  // information differentiates designs even when caching is disabled.
  object DesignLoadGate:
    // Code identity within a run is the FULL dclMeta plus the codeString-normalized input
    // DFTypes and impure applied data, and the plain Scala arguments by their own value
    // equality (see `DesignLoadKey`, which builds keys off the ambient elaboration
    // context). The applied design parameter values are deliberately NOT part of the key:
    // a pure body cannot depend on them (forcing a parameter's data to affect elaboration
    // is impure by definition), so all applications share one loaded body and differ only
    // in their instance parameter bindings, which the harness constructs afresh on hit and
    // miss alike.
    // this run's canonical design per key: intra-run repeats join it as duplicates
    private val canonicalOf = mutable.Map.empty[DesignLoadKey, DFDesignBlock]
    // The sub-design cache service consulted when `ElaborationOptions.CacheEnable` is set.
    // Per-elaboration (each gate owns its instance, so swapping it, the testing seam,
    // cannot race other elaborations); the default disk service shares its underlying
    // stores process-wide through thread-safe companion state.
    var subDesignCache: SubDesignCache = new SubDesignDiskCache
    // Joins the current (ending) class design to this key's canonical design when one
    // exists: the ended context is marked as its duplicate and `endDesign` joins the
    // canonical's group (the design's own members are then reduced to its public
    // interface and dropped at final assembly). Returns false on a miss (the caller
    // records the design as the key's canonical through `completed`).
    def joinCanonicalOf(key: DesignLoadKey): Boolean =
      canonicalOf.get(key) match
        case Some(canonical) =>
          DesignContext.current.markDuplicateOf(canonical)
          true
        case None => false
    // ~~~ the sub-design cache service tier ~~~
    // Externally-loaded (adopted) designs of this run, keyed by the shell block's
    // ownerRef token: the token-freshened cached forest that the final assembly
    // (`hierarchical`) emits as the shell's content.
    private val adopted =
      mutable.LinkedHashMap.empty[StaticRef, List[(StaticRef, DB)]]
    def adoptedForestOf(design: DFDesignBlock): Option[List[(StaticRef, DB)]] =
      adopted.get(StaticRef(design.ownerRef))
    // Looks this key up in the run's loads and then, when enabled, in the sub-design
    // cache service. On a hit the body is skipped and the loaded design's self-contained
    // DB is returned; the caller reads whatever it needs from it (e.g. its return DFType
    // via `subDesignRetDFType`, to create the fresh output port). An intra-run hit marks
    // the current (shell) context as a duplicate of the key's canonical design and returns
    // the canonical's forest DB; a service hit makes this shell the canonical of its group
    // with an EXTERNAL body (the adopted forest), seeds the intra-run tier so same-run
    // repeats unify with it, and returns the cached DB.
    def lookup(key: DesignLoadKey, ownerClass: Class[?], cacheEnable: Boolean)(using
        RefGen
    )
        : Option[DB] =
      val ctx = DesignContext.current
      canonicalOf.get(key) match
        case Some(canonical) =>
          ctx.markDuplicateOf(canonical)
          Some(DesignContext.buildDesignForestDB(canonical))
        case None if cacheEnable =>
          val currentDesign = OwnershipContext.currentDesign
          subDesignCache.lookup(ownerClass, key.localKey) match
            // guard against key collisions and stale entries: the cached top must
            // declare the same design (name-insensitive: dclName dedup-renaming may
            // differ between the storing and loading runs); a mismatch is a miss
            case Some(cachedDB)
                if cachedDB.subDBs.headOption.exists { (_, sub) =>
                  val cTop = sub.top
                  cTop.instMode == currentDesign.instMode &&
                  cTop.domainType == currentDesign.domainType &&
                  cTop.dclMeta.position == currentDesign.dclMeta.position
                } =>
              val forest =
                cachedDB.freshenSubDesignForest(StaticRef(currentDesign.ownerRef))
              adopted += StaticRef(currentDesign.ownerRef) -> forest
              canonicalOf += key -> currentDesign
              Some(cachedDB)
            case _ => None
          end match
        case None => None
      end match
    end lookup
    // Records an ended clean live run as this key's canonical and, when enabled,
    // stores its self-contained forest in the sub-design cache service. A body that
    // AUTO-created design parameters (a capture path the rigging cannot see, resolved
    // at runtime through `cloneUnreachable`) must not be recorded: a hit could not
    // re-create such parameters since the body is skipped (running live is always
    // correct; structural dedup unifies identical bodies afterwards).
    def completed(
        key: DesignLoadKey,
        design: DFDesignBlock,
        ownerClass: Class[?],
        cacheEnable: Boolean
    ): Unit =
      canonicalOf.getOrElseUpdate(key, design)
      if (cacheEnable)
        subDesignCache.store(ownerClass, key.localKey, DesignContext.buildDesignForestDB(design))
  end DesignLoadGate

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

  // The dclName uniquification and duplicate-design canonicalization maps, derived from
  // the whole-run `uniqueDesigns` groups: (duplicate design -> its canonical group head,
  // design -> its dclName-renamed block copy). Consumed by the hierarchical
  // by-construction assembly (`hierarchical`).
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

  // The current-context FLAT snapshot: the context's member list (with ended child
  // designs expanded in place through their end-of-design snapshots when `flatten` is
  // set) and the context refTable, after the global-ctx/ref cleanup. Serves
  // meta-programming (a meta-design's DB is a flat member container to inject through
  // the patch system, unflattened) and `designDB` access DURING elaboration (the
  // hierarchical DB only exists by construction once the design tree is complete).
  private def currentContextDB(flatten: Boolean): DB =
    val rawMembers = DesignContext.current.getImmutableMemberList
    val members = if (flatten) getFlattenedMemberList(rawMembers) else rawMembers
    val refTable = DesignContext.current.getImmutableRefTable
    val membersNoGlobalCtx = members.map {
      case m: DFVal.CanBeGlobal  => m.copyWithoutGlobalCtx
      case design: DFDesignBlock =>
        design.clearDesignInstCache()
        design
      case m => m
    }
    // Drop orphan OneWay.Gen refs: refTable entries whose key is no live
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
    DB(membersNoGlobalCtx, cleanedRefTable, GlobalTagContext.tags, Nil)
  end currentContextDB

  private def getFlattenedMemberList(topMemberList: List[DFMember]): List[DFMember] =
    def flattenMembers(owner: DFMember): List[DFMember] = owner match
      case o: DFDesignBlock =>
        o :: DesignContext.designMembers.getOrElse(o, Nil).flatMap(flattenMembers)
      case member => List(member)
    topMemberList.flatMap(flattenMembers)

  // The immutable DB of this elaboration, memoized until the next mutation.
  // In meta-programming (indicated by the existence of an external context) it is the
  // current context's member view: a meta-design's DB is just the freshly created
  // members to inject through the patch system, with no design hierarchy. DURING
  // elaboration (open design contexts) it is the current subtree's flat snapshot,
  // serving mid-run `designDB` consumers (error printing, test utilities). Once the
  // design tree is complete it IS the final hierarchical DB, assembled by construction
  // (`hierarchical`): the flat form of the complete design no longer exists.
  def immutable: DB = memoizedDB.getOrElse {
    val db =
      if (inMetaProgramming) currentContextDB(flatten = false)
      else if (DesignContext.stack.nonEmpty) currentContextDB(flatten = true)
      else hierarchical
    memoizedDB = Some(db)
    db
  }

  // ~~~ hierarchical DB by construction ~~~
  // The final hierarchical DB is the top design's self-contained NATURAL forest (built and
  // memoized by `buildDesignForestDB`) with the whole-run final fixes applied ON TOP.
  // Those fixes (dclName enumeration, resource constraints) are structure-preserving, so
  // the natural forest already carries the exact sub-DB set, order, instance unification,
  // and globals closures; this pass only rewrites each member and re-resolves each ref
  // (`fixedMember`/`resolveFixed`), swaps each service-cached shell for its adopted forest,
  // and anchors orphan globals at the top. BACKS `immutable` (the flat form and its
  // `DB.oldToNew` re-partition round trip no longer exist).
  private def hierarchical: DB =
    require(!inMetaProgramming, "hierarchical DB construction is undefined in meta-programming")
    // the run's merged state: the (ended) top-level context member list, which holds
    // the injected globals and the top design block, and the run-wide merged refTable
    val topMemberList = DesignContext.current.getImmutableMemberList
    val rawRefTable = DesignContext.current.getImmutableRefTable
    val topDesign = topMemberList.collectFirst { case d: DFDesignBlock => d }.get
    val natural = DesignContext.buildDesignForestDB(topDesign)
    val (dupToOrigDesignMap, duplicateDesignRepMap) = designDedupMaps
    val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
    val globalTags = GlobalTagContext.tags
    // the whole-run final fix of a single member
    def fixedMember(m: DFMember): DFMember = m match
      case design: DFDesignBlock =>
        ResourceOwnershipContext.getConstrainedDomainOwner(
          duplicateDesignRepMap.getOrElse(design, design)
        )
      case domainOwner: DFDomainOwner =>
        ResourceOwnershipContext.getConstrainedDomainOwner(domainOwner)
      case dcl: DFVal.Dcl => constrainedDcls.getOrElse(dcl, dcl)
      case m              => m
    def unifyInst(inst: DFDesignInst): DFDesignInst =
      val target =
        dupToOrigDesignMap.getOrElse(inst.designRef.asRef.get, inst.designRef.asRef.get)
      inst.copy(designRef = StaticRef(target.ownerRef))
    // a ref target resolved against the raw refTable with the whole-run fixes applied
    def resolveFixed(r: DFRefAny): Option[DFMember] =
      rawRefTable.get(r).map {
        case inst: DFDesignInst => unifyInst(inst)
        case m                  => fixedMember(m)
      }
    // Every ref emitted by the DB's members, fix-resolved. The sub-DB's own design block
    // behaves as a Top: its ownerRef resolves to DFMember.Empty (unless it already is an
    // empty ref, i.e. the true top). `DFDesignInst.designRef` is deliberately NOT
    // collected: it is unified with the child's `subDBs` key and resolved structurally.
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
    // fix one natural sub-DB: rename/constrain every member (the sub-DB's own top design
    // also drops its stale elaboration-time instance cache) and re-resolve every ref (a
    // constraint may add refs the natural pass never saw, so refs are re-derived here)
    def fixSubDB(sub: DB): DB =
      val naturalTop = sub.top
      val dFinal = fixedMember(naturalTop).asInstanceOf[DFDesignBlock]
      dFinal.clearDesignInstCache()
      val fixedMembers = sub.members.map {
        case d: DFDesignBlock if d eq naturalTop => dFinal
        case m                                   => fixedMember(m)
      }
      DB(fixedMembers, refsFor(dFinal, fixedMembers), globalTags, Nil)
    // ~~~ apply the fixes over the natural forest, sub-DB by sub-DB (in forest order) ~~~
    val builtSubDBs = mutable.LinkedHashMap.empty[StaticRef, DB]
    natural.subDBs.foreach { (key, sub) =>
      DesignLoadGate.adoptedForestOf(sub.top) match
        // a service-cached shell: its natural (public-only) sub-DB is discarded and the
        // adopted (token-freshened) forest takes its place, the placeholder top header
        // replaced by the shell's final (renamed/constrained) block; every sub-DB takes
        // this run's globalTags. The adopted children follow right after the shell.
        case Some(forest) =>
          val dFinal = fixedMember(sub.top).asInstanceOf[DFDesignBlock]
          dFinal.clearDesignInstCache()
          forest match
            case (topKey, topSub) :: children =>
              val cachedTop = topSub.top
              // header replacement by EQUALITY, not identity: a JSON round trip through
              // the service deserializes equal-but-distinct member instances into the
              // members list and the refTable targets
              val newMembers =
                topSub.members.map(m => if (m == (cachedTop: DFMember)) dFinal else m)
              val newRefTable = topSub.refTable.map { (r, t) =>
                val newR = if (r == cachedTop.ownerRef) dFinal.ownerRef else r
                newR -> (if (t == (cachedTop: DFMember)) (dFinal: DFMember) else t)
              }
              builtSubDBs(topKey) = DB(newMembers, newRefTable, globalTags, Nil)
              children.foreach { (k, s) => builtSubDBs(k) = s.update(globalTags = globalTags) }
            case Nil =>
          end match
        case None => builtSubDBs(key) = fixSubDB(sub)
    }
    // orphan globals (reached by no sub-DB closure) anchor at the top design's sub-DB
    val allGlobalsOrderedRaw: List[DFVal.CanBeGlobal] = topMemberList.collect {
      case g: DFVal.CanBeGlobal if g.isGlobal => g
    }
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
      val orphanGlobals = orphanGlobalsRaw.map(_.copyWithoutGlobalCtx)
      val topKey = StaticRef(topDesign.ownerRef)
      val topSub = builtSubDBs(topKey)
      builtSubDBs(topKey) = topSub.update(
        members = orphanGlobals ::: topSub.members,
        refTable = topSub.refTable ++ refsFor(topDesign, orphanGlobals)
      )
    DB(Nil, Map.empty, globalTags, Nil, ListMap.from(builtSubDBs))
  end hierarchical

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
