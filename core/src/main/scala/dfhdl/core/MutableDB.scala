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
  RefGen,
  SubDesignEntry,
  SubDesignRef
}

import scala.reflect.{ClassTag, classTag}
import scala.util.control.NonFatal
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
  // A design DEF's phantom members (its harness-created ports and parameters materializing the
  // values its body captures), keyed by the captured value each one materializes. A nested def
  // call in this body captures a value of an ENCLOSING design, which the call site cannot
  // reference (this def's own design sits between the two); the plugin propagates that capture
  // to this def as well, so the value the call binds to here is this design's own phantom
  // member for it (see `r__For_Plugin.designFromDefImpl`).
  val defPhantoms = mutable.Map.empty[DFVal, DFValAny]
  // the pre-built, self-contained sub-DBs of static functions called at GLOBAL scope while this
  // (global) context was active. A referencing run loads them (`MutableDB.injectGlobals`): the
  // def block is a global member, but its private body is not, so the built sub-DB carries it.
  val globalDefSubDBs = mutable.LinkedHashMap.empty[StaticRef, DB]
  val loopIterMap = mutable.Map.empty[Meta, DFValAny]
  // on a design-load hit (`DesignLoadGate`), the canonical design whose body this context
  // duplicates; `endDesign` joins the duplicate to the CANONICAL's group (a design may
  // have several structurally-distinct groups, one per key)
  private var _duplicateOf: Option[StaticRef] = None
  def duplicateOf: Option[StaticRef] = _duplicateOf
  // this context duplicates `canonical` and joins its group at `endDesign`
  def markDuplicateOf(canonical: StaticRef): Unit = _duplicateOf = Some(canonical)

  // ~~~ the class-design body-skip gate (see `Design.__clsBodyGate`) ~~~
  // A design class's body is guarded by the compiler plugin: the gate runs at the body's head and,
  // on a design-load hit, the guarded statements do not run. What the shell context then holds is
  // exactly the design's public interface: its parameters (which the gate itself creates) and its
  // port, constant and interface declarations (which the plugin leaves unguarded), just as a design
  // def's interface is created by its harness rather than by its body.

  // the design's parameters, created by the harness (the gate) and NOT by the class body: keyed
  // by the declaring class and the parameter's index in it, since a base class in the chain
  // contributes its own (see `Design.__clsGetParam`)
  val clsParams = mutable.Map.empty[(Class[?], Int), DFValAny]
  // a class in the chain has run its body statements live into this design, so no gate below it
  // may skip (the design would hold half a body)
  var clsBodyRanLive: Boolean = false
  // the key the gate computed at the body's head; None when the class has no gate (the
  // plugin found it unskippable) or the gate stood down (a base class's body, a top design
  // or a keyless design)
  var clsLoadKey: Option[DesignLoadKey] = None
  // the design's declaring class, as the gate resolved it (the cache anchor of both the
  // gate's lookup and the design's store)
  var clsDclClass: Option[Class[?]] = None
  // the number of design parameters the gate saw; a body that creates parameters of its
  // own (an auto-created capture parameter, `cloneUnreachable`) invalidates the gate's key
  var clsGateParamNum: Int = 0
  // the gate's decision: this design's body was skipped
  var clsSkipBody: Boolean = false
  def designParamNum: Int = members.view.count {
    case MemberEntry(irValue = _: DFVal.DesignParam, ignore = false) => true
    case _                                                           => false
  }

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
    // a static function called at GLOBAL scope carries its pre-built, self-contained sub-DB
    // here (its def block is a global member, but its private body is not part of the global
    // members). Merging them with the rest of the context makes them ride every injection.
    globalDefSubDBs ++= sourceCtx.globalDefSubDBs
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

  // A design's identity through the run: the `ownerRef` token minted once when its block is
  // created. The block itself is a VALUE that gets replaced repeatedly (a tag or a name change
  // revises it through `setMember`, the dclName enumeration revises it again at assembly), so it
  // is never an identity; its `ownerRef` survives all of it, and IS the design's `subDBs` key in
  // the hierarchical DB. One identity therefore runs from elaboration through to the final DB,
  // which is why every table of the run below is keyed by it and never by the block.
  extension (design: DFDesignBlock) private def refId: StaticRef = StaticRef(design.ownerRef)

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
    // ~~~ the designs of this run, keyed by identity (`refId`) and never by the block value ~~~
    // the end-of-design member snapshot of a design, and the design block itself as it stood then
    val designMembers = mutable.Map.empty[StaticRef, List[DFMember]]
    private val designOf = mutable.Map.empty[StaticRef, DFDesignBlock]
    // the dclName groups feeding the emitted-name enumeration (`dclNameEnumeration`); the head of
    // a group is its canonical design
    val uniqueDesigns = mutable.Map.empty[String, List[List[StaticRef]]]
    // a duplicate design (a design-load hit) and the canonical design it duplicates. Recorded
    // where it is KNOWN — at the hit — rather than derived back out of the groups later.
    private val canonicalOfDuplicate = mutable.Map.empty[StaticRef, StaticRef]
    // memoized sub-DB per design: a design's members are final once it has ended, so its
    // sub-DB is stable and the same instance serves every later lookup/store/assembly
    private val subDBMemo = mutable.Map.empty[StaticRef, DB]
    def designAt(design: StaticRef): DFDesignBlock = designOf.get(design) match
      case Some(d) => d
      // a pre-unification `DFDesignInst.designRef` is a distinct parent-side ref (not a
      // design's `ownerRef` identity key), resolved through the live refTable instead of
      // the registry (e.g. test utilities printing live members mid-elaboration)
      case None => self.getMember(design.asRef)
    // the canonical design of a design: itself, unless it duplicates one
    def canonicalOf(design: StaticRef): StaticRef = canonicalOfDuplicate.getOrElse(design, design)

    // a design starts a group of its own in its dclName's group list (the group head is
    // its canonical; same-dclName groups only iterate the emitted dclName, see
    // `dclNameEnumeration`)
    private def startDesignGroup(design: DFDesignBlock): Unit =
      uniqueDesigns.updateWith(design.dclName) {
        case Some(groupList) => Some(List(design.refId) :: groupList)
        case None            => Some(List(List(design.refId)))
      }
    // An externally-loaded (adopted) design that this run never elaborates: it is a design
    // of this run all the same, so it takes part in the dclName enumeration like any live
    // one. Registered in forest order at adoption (children before the adopting shell,
    // which registers at its own end), matching the order the same designs would have
    // taken had the bodies run live.
    def adoptDesign(design: DFDesignBlock): Unit =
      designOf += design.refId -> design
      startDesignGroup(design)

    def startDesign(design: DFDesignBlock): Unit =
      stack = current :: stack
      current = new DesignContext
    def endDesign(design: DFDesignBlock): Unit =
      val currentMembers = current.getImmutableMemberList.drop(1)
      val currentRefTable = current.getImmutableRefTable
      val designType = design.dclName
      designOf += design.refId -> design
      // ~~~ design unification ~~~
      // Designs unify ONLY through the design load gate's key (there is no structural
      // body comparison): a gate hit recorded the canonical design this context
      // duplicates (`duplicateOf`), so the ended design joins the canonical's group
      // (the group head is always the canonical); any other design, including every
      // KEYLESS one (impure or otherwise unloadable), starts a group of its own.
      // Same-dclName groups only iterate the emitted dclName (`dclNameEnumeration`), so a
      // keyless design instantiated N times simply emits N enumerated designs.
      current.duplicateOf match
        case Some(canonical) =>
          canonicalOfDuplicate += design.refId -> canonical
          val groupList = uniqueDesigns(designType)
          uniqueDesigns += designType -> groupList.map { group =>
            if (group.head == canonical) group.head :: design.refId :: group.drop(1)
            else group
          }
        case None => startDesignGroup(design)
      // A duplicate's members are NOT minimized here: the final assembly drops duplicate
      // designs wholesale (they are never `isLive`, their instances unify to the
      // canonical), so a duplicate's retained snapshot is simply never read.
      designMembers += design.refId -> currentMembers
      stack.head.refTable ++= currentRefTable
      // a static function called at GLOBAL scope (its parent context IS the global one): carry
      // its pre-built, self-contained sub-DB on the global context, so a run referencing the
      // resulting global value can LOAD it. Its body is private to this def's context and does
      // not ride the global member injection, so the built sub-DB is how the body travels.
      if ((stack.head eq global) && design.instMode == DFDesignBlock.InstMode.Def)
        global.globalDefSubDBs += design.refId -> buildDesignSubDB(design.refId)
      stack.head.addMember(design)
      current = stack.head
      stack = stack.drop(1)
    end endDesign
    // the canonical designs this design instantiates or calls, in instance order. An
    // instance's RAW snapshot `designRef` still resolves through the refTable (only the
    // emitted form of an instance carries the design's `refId` directly, see `unifyInst`);
    // a method call's key already IS the canonical design's identity (minted so at the
    // call site, never rewritten).
    def childDesignsOf(design: StaticRef): List[StaticRef] =
      designMembers.getOrElse(design, Nil).collect {
        case inst: DFDesignInst =>
          canonicalOf(self.getMember(inst.designRef.asRef).refId)
        case DFVal.Func.Call(_, designKey) => designKey
      }.distinct

    // ~~~ a design's OWN sub-DB ~~~
    // Built from the design's end-of-design snapshot (final once the design has ended),
    // BEFORE any whole-run fixes: natural dclNames (the loading run applies its own
    // naming) and no resource constraints (a cacheable def is device-independent). It is
    // both the design's slot in the final forest and, for a cacheable def, the body of its
    // cross-run cache entry (`SubDesignEntry`, which references its children by cache
    // key rather than embedding them). Instances are unified to their canonical design (the
    // child's `subDBs` key), refs resolve through the mutable run state (the design's
    // refTable merged into the enclosing contexts), and the globals it reaches join it
    // through the same closure the final assembly computes.
    def buildDesignSubDB(design: StaticRef): DB =
      subDBMemo.getOrElseUpdate(design, buildSubDB(designAt(design)))
    private def buildSubDB(c: DFDesignBlock): DB =
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
        inst.copy(designRef = canonicalOf(self.getMember(inst.designRef.asRef).refId))
      // a ref target resolved against the run's refTable with the instance unification
      // applied: refs INTO an instance (a port selection's `designInstRef`) must land on
      // the very same instance value the member list carries, since consumers compare
      // instances by value (e.g. `ConnectPoint.Via`)
      def resolveUnified(r: DFRefAny): Option[DFMember] =
        resolve(r).map {
          case inst: DFDesignInst => unifyInst(inst)
          case m                  => m
        }
      def refsFor(dbMembers: Iterable[DFMember]): Map[DFRefAny, DFMember] =
        val result = mutable.Map.empty[DFRefAny, DFMember]
        dbMembers.foreach { m =>
          val ownerTarget =
            if ((m eq c) && !m.ownerRef.isInstanceOf[DFRef.Empty])
              if (resolve(m.ownerRef).nonEmpty) Some(DFMember.Empty) else None
            else resolveUnified(m.ownerRef)
          ownerTarget.foreach(t => result(m.ownerRef) = t)
          m.getRefs.foreach(r => resolveUnified(r).foreach(t => result(r) = t))
        }
        result.toMap
      val locals = designMembers.getOrElse(c.refId, Nil).flatMap {
        case _: DFDesignBlock                   => None
        case g: DFVal.CanBeGlobal if g.isGlobal => None
        case inst: DFDesignInst                 => Some(unifyInst(inst))
        case cbg: DFVal.CanBeGlobal             => Some(cbg.copyWithoutGlobalCtx)
        case m                                  => Some(m)
      }
      // NOTE: the design block's transient elaboration-time instance cache is NOT cleared
      // here; the design is still live in this run (it is not serialized into an entry)
      val dbMembers = globalsClosure(c :: locals) ::: c :: locals
      DB(dbMembers, refsFor(dbMembers), GlobalTagContext.tags, Nil)
    end buildSubDB

    // ~~~ the run's design forest ~~~
    // The design and its descendants as a hierarchical (root) DB, in depth-first instance
    // order. A live design contributes its own sub-DB; an externally-loaded (adopted) one
    // contributes the sub-DB the design load gate cloned for it, and the walk descends into
    // the designs the gate resolved for its instances.
    def buildDesignForestDB(design: StaticRef): DB =
      val built = mutable.LinkedHashMap.empty[StaticRef, DB]
      def build(c: StaticRef): Unit = // c is always a canonical design
        if (!built.contains(c))
          DesignLoadGate.adoptedDBOf(c) match
            case Some(adoptedDB) =>
              built(c) = adoptedDB
              DesignLoadGate.adoptedChildrenOf(c).foreach(build)
            case None =>
              built(c) = buildDesignSubDB(c)
              childDesignsOf(c).foreach(build)
      build(canonicalOf(design))
      DB(Nil, Map.empty, GlobalTagContext.tags, Nil, ListMap.from(built))
    end buildDesignForestDB
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
    // see `DesignContext.defPhantoms`
    def addDefPhantoms(phantoms: IterableOnce[(DFVal, DFValAny)]): Unit =
      current.defPhantoms ++= phantoms
    def getDefPhantoms: Map[DFVal, DFValAny] = current.defPhantoms.toMap
  end DesignContext

  // ~~~ the design load gate ~~~
  // Decides, per method instantiation, whether to run the elaboration body live or
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
    private val canonicalOf = mutable.Map.empty[DesignLoadKey, StaticRef]
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
    // The run's design per cache key (`SubDesignRef`), live or adopted: a cached entry
    // references its children by cache key, so a child already loaded under that key is
    // REUSED (a design instantiated by several parents is loaded once, and an adopted
    // design unifies with a live elaboration of the same key).
    private val designByRef = mutable.Map.empty[SubDesignRef, StaticRef]
    // the cache key of a design whose entry is IN the service (live-stored or adopted): a
    // design can only be stored if every child of it can be referenced this way
    private val storedRefOf = mutable.Map.empty[StaticRef, SubDesignRef]
    // The externally-loaded (adopted) designs of this run: the sub-DB cloned from the
    // design's cache entry (the final assembly emits it as the design's content) and the
    // designs this run resolved for its instances (the forest walk descends into them).
    private val adoptedDB = mutable.Map.empty[StaticRef, DB]
    private val adoptedChildren = mutable.Map.empty[StaticRef, List[StaticRef]]
    def adoptedDBOf(design: StaticRef): Option[DB] = adoptedDB.get(design)
    def adoptedChildrenOf(design: StaticRef): List[StaticRef] =
      adoptedChildren.getOrElse(design, Nil)
    def isAdopted(design: StaticRef): Boolean = adoptedDB.contains(design)

    // a def's owner class, resolved by name for a child entry (the child may be declared in
    // a different class than its parent); `None` when it cannot be loaded, which makes the
    // adoption fail over to a live elaboration
    private def classOf(name: String, loader: ClassLoader): Option[Class[?]] =
      try Some(Class.forName(name, false, loader))
      catch case NonFatal(_) => None

    // The design this run uses for a cached entry's child: an already-loaded design of that
    // key, or a fresh adoption of the child's own entry. `None` (an unloadable child) fails
    // the whole adoption.
    private def childDesignOf(childRef: SubDesignRef, loader: ClassLoader)(using
        RefGen
    ): Option[StaticRef] =
      designByRef.get(childRef).orElse {
        for
          cls <- classOf(childRef.ownerClassName, loader)
          entry <- subDesignCache.lookup(cls, childRef.localKey)
          design <- adopt(entry, childRef, loader)
        yield design
      }

    // Adopts an entry as a design of this run: its children are resolved FIRST (depth first,
    // so a child is a design of this run before its parent is), then the entry's sub-DB is
    // cloned onto freshly minted tokens with the instances retargeted at the resolved
    // children. The cloned design block IS the run's design for this entry, and it joins the
    // dclName enumeration like any live one.
    //
    // The entry's own ref tokens are RE-MINTED from this run's generator (`freshenLocalRefs`),
    // because a stored token means nothing here: the storing run minted it from ITS generator,
    // whose ids and group ids restart per run, so a cached design carries tokens that this run
    // will mint again for its own members. That is not a remote possibility but the norm, since
    // the two runs elaborate the same code: the very token this entry uses for a port's owner is
    // the one the run mints for the adopted design's structural key, and the two bindings then
    // collapse onto one key. Re-minting puts the entry in this run's namespace once, at load,
    // rather than leaving every later consumer to cope with an alias.
    private def adopt(entry: SubDesignEntry, ref: SubDesignRef, loader: ClassLoader)(using
        refGen: RefGen
    ): Option[StaticRef] =
      val childOpts = entry.children.map((storedRef, childRef) =>
        childDesignOf(childRef, loader).map(storedRef -> _)
      )
      Option.when(childOpts.forall(_.isDefined)) {
        val children = childOpts.map(_.get)
        val db = entry.cloneForAdoption(
          refGen.genOneWay[DFDesignBlock].asInstanceOf[DFOwner.Ref],
          children.map((sRef, c) => sRef -> c.asRef).toMap
        )
        val design = db.top
        adoptedDB(design.refId) = db
        adoptedChildren(design.refId) = children.map(_._2).distinct
        designByRef(ref) = design.refId
        storedRefOf(design.refId) = ref
        DesignContext.adoptDesign(design)
        design.refId
      }
    end adopt

    // Looks this key up in the run's loads and then, when enabled, in the sub-design cache
    // service. On a hit the body is skipped and the loaded design's own sub-DB is returned;
    // the caller reads whatever it needs from it (e.g. its return DFType via
    // `subDesignRetDFType`, to create the fresh output port). Either way the current (shell)
    // context is marked a duplicate of the key's canonical design and drops out of the final
    // assembly: on a service hit that canonical is the freshly adopted design (whose body is
    // EXTERNAL), seeded into the intra-run tier so same-run repeats unify with it too.
    def lookup(key: DesignLoadKey, ownerClass: Class[?], cacheEnable: Boolean)(using
        RefGen
    )
        : Option[DB] =
      canonicalOf.get(key) match
        case Some(canonical) =>
          DesignContext.current.markDuplicateOf(canonical)
          Some(adoptedDBOf(canonical).getOrElse(DesignContext.buildDesignSubDB(canonical)))
        case None if cacheEnable =>
          val shell = OwnershipContext.currentDesign
          val ref = SubDesignRef(ownerClass.getName, key.localKey)
          subDesignCache.lookup(ownerClass, key.localKey)
            // guard against key collisions and stale entries: the stored design must be
            // the same declaration (name-insensitive: dclName enumeration may differ
            // between the storing and loading runs); a mismatch is a miss
            .filter { entry =>
              val stored = entry.db.top
              stored.instMode == shell.instMode && stored.domainType == shell.domainType &&
              stored.dclMeta.position == shell.dclMeta.position
            }
            .flatMap(adopt(_, ref, ownerClass.getClassLoader))
            .map { adoptedDesign =>
              DesignContext.current.markDuplicateOf(adoptedDesign)
              canonicalOf += key -> adoptedDesign
              adoptedDB(adoptedDesign)
            }
        case None => None
      end match
    end lookup

    // Records an ended clean live run as this key's canonical and, when enabled, stores its
    // cache entry: the design's own sub-DB plus its children BY CACHE KEY. Storing requires
    // every child to be a stored entry itself (a keyless child, e.g. an impure design or a
    // class design, cannot be referenced, so its parent is not cacheable either) — children
    // end before their parent, so this simply propagates up the tree.
    def completed(
        key: DesignLoadKey,
        design: DFDesignBlock,
        ownerClass: Class[?],
        cacheEnable: Boolean
    ): Unit =
      canonicalOf.getOrElseUpdate(key, design.refId)
      val ref = SubDesignRef(ownerClass.getName, key.localKey)
      designByRef.getOrElseUpdate(ref, design.refId)
      if (cacheEnable)
        val childRefOpts =
          DesignContext.childDesignsOf(design.refId).map(c => storedRefOf.get(c).map(c -> _))
        if (childRefOpts.forall(_.isDefined))
          subDesignCache.store(
            ownerClass,
            key.localKey,
            SubDesignEntry(DesignContext.buildDesignSubDB(design.refId), childRefOpts.map(_.get))
          )
          storedRefOf(design.refId) = ref
      end if
    end completed
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

  // The emitted dclName of every design that gets one of its own, derived from the whole-run
  // `uniqueDesigns` groups: same-dclName groups enumerate (`Foo_0`, `Foo_1`, ...), a lone group
  // keeps the declared name. Keyed by design IDENTITY and holding the NAME rather than a rewritten
  // block, so it applies to whatever revision of a design block the assembly hands it. The
  // duplicate -> canonical direction is NOT derived here: the design load gate already knows it at
  // the hit (`DesignContext.canonicalOf`).
  private def dclNameEnumeration: Map[StaticRef, String] =
    DesignContext.uniqueDesigns.view.flatMap { case (designType, groupList) =>
      groupList.view.reverse.zipWithIndex.flatMap {
        case (group, i) if groupList.length > 1 =>
          val updatedDclName = s"${designType}_${i.toPaddedString(groupList.length)}"
          group.view.map(_ -> updatedDclName)
        case _ => Nil
      }
    }.toMap

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
        o :: DesignContext.designMembers.getOrElse(o.refId, Nil).flatMap(flattenMembers)
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
  // and anchors orphan globals at the top.
  private def hierarchical: DB =
    require(!inMetaProgramming, "hierarchical DB construction is undefined in meta-programming")
    // the run's merged state: the (ended) top-level context member list, which holds
    // the injected globals and the top design block, and the run-wide merged refTable
    val topMemberList = DesignContext.current.getImmutableMemberList
    val rawRefTable = DesignContext.current.getImmutableRefTable
    // a def-design block (a static function called at global scope) may be injected into the
    // top-level context ahead of the real top design, so skip Def blocks when finding the top
    val topDesign = topMemberList.collectFirst {
      case d: DFDesignBlock if d.instMode != DFDesignBlock.InstMode.Def => d
    }.get
    val natural = DesignContext.buildDesignForestDB(topDesign.refId)
    val dclNames = dclNameEnumeration
    val constrainedDcls = ResourceOwnershipContext.getConstrainedDcls()
    val globalTags = GlobalTagContext.tags
    // The DB's block for a design: the enumerated dclName (which applies to whatever revision of
    // the block arrives here) and the resource constraints, over a COPY — never the live block
    // itself. The live block carries the elaboration-time instance cache, which the mutable DB
    // still serves from (the simulation API resolves instance paths through it), so the assembly
    // takes its own block and clears the cache there. Memoized per design, so every occurrence of
    // a design across the forest is the same object.
    val fixedDesignOf = mutable.Map.empty[StaticRef, DFDesignBlock]
    def fixedDesign(design: DFDesignBlock): DFDesignBlock =
      fixedDesignOf.getOrElseUpdate(
        design.refId, {
          val renamed = dclNames.get(design.refId) match
            case Some(dclName) if dclName != design.dclName =>
              design.copy(meta = design.meta.copy(nameOpt = Some(dclName)))
            case _ => design.copy()
          val fixed =
            ResourceOwnershipContext.getConstrainedDomainOwner(renamed).asInstanceOf[DFDesignBlock]
          fixed.clearDesignInstCache()
          fixed
        }
      )
    // the whole-run final fix of a single member
    def fixedMember(m: DFMember): DFMember = m match
      case design: DFDesignBlock      => fixedDesign(design)
      case domainOwner: DFDomainOwner =>
        ResourceOwnershipContext.getConstrainedDomainOwner(domainOwner)
      case dcl: DFVal.Dcl => constrainedDcls.getOrElse(dcl, dcl)
      case m              => m
    def unifyInst(inst: DFDesignInst): DFDesignInst =
      inst.copy(designRef = DesignContext.canonicalOf(self.getMember(inst.designRef.asRef).refId))
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
      val dFinal = fixedDesign(naturalTop)
      val fixedMembers = sub.members.map {
        case d: DFDesignBlock if d eq naturalTop => dFinal
        case m                                   => fixedMember(m)
      }
      DB(fixedMembers, refsFor(dFinal, fixedMembers), globalTags, Nil)
    // fix one ADOPTED sub-DB: its refs are self-contained (they were cloned onto this run's
    // tokens at adoption, resolving within the sub-DB), so only the design block itself is
    // renamed here, wherever it appears
    def fixAdoptedSubDB(sub: DB): DB =
      val adoptedTop = sub.top
      val dFinal = fixedDesign(adoptedTop)
      if (dFinal == adoptedTop) sub.update(globalTags = globalTags)
      else
        val newMembers = sub.members.map(m => if (m eq (adoptedTop: DFMember)) dFinal else m)
        val newRefTable = sub.refTable.view.mapValues { t =>
          if (t eq (adoptedTop: DFMember)) (dFinal: DFMember) else t
        }.toMap
        DB(newMembers, newRefTable, globalTags, Nil)
    // ~~~ apply the fixes over the natural forest, sub-DB by sub-DB (in forest order) ~~~
    val builtSubDBs = mutable.LinkedHashMap.empty[StaticRef, DB]
    natural.subDBs.foreach { (key, sub) =>
      builtSubDBs(key) = if (DesignLoadGate.isAdopted(key)) fixAdoptedSubDB(sub) else fixSubDB(sub)
    }
    // append the global-scope def sub-DBs that arrived through global injection (static
    // functions called at global scope): they are forest roots — referenced by a global `Func`,
    // not by any design in the natural forest — and self-contained, so they are fixed like
    // adopted designs (only the block itself renamed; refs already resolve within the sub-DB)
    DesignContext.global.globalDefSubDBs.foreach { (key, sub) =>
      if (!builtSubDBs.contains(key)) builtSubDBs(key) = fixAdoptedSubDB(sub)
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
    def getDesignBlockByKey(key: StaticRef): DFDesignBlock =
      DesignContext.designAt(key)
  end getSet

end MutableDB
