package dfhdl.compiler.ir

import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{ListMap, ListSet, BitSet}
import dfhdl.internals.*
import dfhdl.compiler.printing.{Printer, DefaultPrinter}
import dfhdl.compiler.analysis.*
import DFDesignBlock.InstMode
import upickle.default.*

final case class DB private (
    members: List[DFMember],
    refTable: Map[DFRefAny, DFMember],
    globalTags: DFTags,
    srcFiles: List[SourceFile],
    // Keyed by the sub-DB's `designBlock.ownerRef` — a lightweight, stable
    // identity for the design instance that isn't invalidated when a stage
    // replaces the DFDesignBlock object via a patch (the patch preserves
    // ownerRef). Each sub-DB carries its own `designBlock: Some(d)` so the
    // actual block is always accessible without a map lookup. Only the
    // new-style root DB has a populated `subDBs`: a flat ListMap of
    // every design in elaboration order (top first, then descendants). Sub-DBs
    // and old-style flat DBs both have an empty `subDBs`.
    subDBs: ListMap[StaticRef, DB] = ListMap.empty
)(_rootDB: => DB) derives CanEqual:
  private val self = this
  lazy val rootDB: DB = _rootDB
  def update(
      members: List[DFMember] = members,
      refTable: Map[DFRefAny, DFMember] = refTable,
      globalTags: DFTags = globalTags,
      srcFiles: List[SourceFile] = srcFiles,
      subDBs: ListMap[StaticRef, DB] = subDBs
  ): DB = DB(members, refTable, globalTags, srcFiles, subDBs)

  given getSet: MemberGetSet with
    val isMutable: Boolean = false
    val designDB: DB = self
    // The new-style root DB has empty `members` and empty `refTable` — all
    // design content lives in sub-DBs. Calls to resolve refs against the
    // root's `getSet` indicate a caller bug (stages must dispatch via
    // sub-DBs, which is what `HierarchyStage` does). Fail loudly rather than
    // silently returning `None` from the empty refTable. On sub-DBs and on
    // old-style flat DBs, behave as before.
    private def assertNotRoot(): Unit =
      if (isRoot)
        throw new IllegalStateException(
          "MemberGetSet is not defined on the new-style root DB; dispatch via sub-DBs"
        )
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 =
      assertNotRoot()
      refTable(ref).asInstanceOf[M0]
    def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] =
      assertNotRoot()
      refTable.get(ref).asInstanceOf[Option[M0]]
    def getOrigin(ref: DFRef.TwoWayAny): DFMember =
      assertNotRoot()
      originRefTable(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      newMemberFunc(originalMember)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M = newMember
    def remove[M <: DFMember](member: M): M = member
    def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit = throw new Exception(
      "Cannot set global tag on immutable DB"
    )
    def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT] = globalTags.getTagOf[CT]
    def getDesignBlockByKey(key: StaticRef): DFDesignBlock =
      val root = rootDB
      val resolved =
        if (root.isRoot) root.subDBs.get(key).map(_.top)
        else root.designBlockByKey.get(key.asRef)
      resolved.getOrElse(apply(key.asRef))
  end getSet
  def atGetSet[T](block: MemberGetSet ?=> T): T = block(using getSet)

  // True for the new-style root DB: a hierarchy container with empty members
  // and empty refTable, holding all designs (including the top) in
  // `subDBs`. False for sub-DBs and old-style flat DBs.
  lazy val isRoot: Boolean = subDBs.nonEmpty

  // TODO: remove this once we don't need the old-style anymore
  lazy val isOldStyleFlatDB: Boolean = rootDB.eq(this) && !isRoot

  // The sub-DB representing the top design. Only meaningful on the new-style
  // root DB, where the top design's sub-DB is by construction the first entry
  // of `subDBs`.
  lazy val topDB: DB =
    if (isRoot) subDBs.head._2
    // old-style flat DB: it is its own root and holds the (flattened) top design
    // directly, so it *is* the top DB. Guard against `rootDB.topDB` re-entering
    // this same lazy val (self-recursive init → CountDownLatch deadlock).
    else if (rootDB.eq(this)) this
    else rootDB.topDB

  lazy val top: DFDesignBlock =
    if (isRoot) topDB.top
    else
      membersNoGlobals.head match
        case m: DFDesignBlock => m
        case invalidTop       =>
          throw new IllegalArgumentException(s"Unexpected member as Top:\n$invalidTop")

  lazy val toptop: DFDesignBlock =
    if (isRoot) top
    else rootDB.top

  lazy val toptopIOs: List[DFVal.Dcl] =
    val members = if (isRoot) topDB.membersNoGlobals else designMemberTable(top)
    members.collect {
      case dcl: DFVal.Dcl if dcl.isPort => dcl
    }

  // considered to be in simulation if the top design has no ports
  lazy val inSimulation: Boolean = toptopIOs.isEmpty

  // considered to be in build if not in simulation and has a device constraint
  lazy val inBuild: Boolean = !inSimulation && toptop.isDeviceTop

  lazy val membersNoGlobals: List[DFMember] = members.filter {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => false
    case _                                          => true
  }

  lazy val membersGlobals: List[DFVal.CanBeGlobal] = members.collect {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => dfVal
  }

  lazy val memberTable: Map[DFMember, Set[DFRefAny]] = refTable.invert

  // Flat-DB analog of `subDBs` lookup: maps each design block's `ownerRef`
  // (which, once unified with the instantiating `DFDesignInst.designRef`, is the
  // design's hierarchy key) to the block itself. Used by `getDesignBlock` to
  // resolve a parent's inst to its child block in the flat (round-trip) DB
  // without a parent-side refTable entry for `designRef`.
  lazy val designBlockByKey: Map[DFRefAny, DFDesignBlock] =
    members.view.collect { case d: DFDesignBlock => (d.ownerRef: DFRefAny) -> d }.toMap

  lazy val originRefTable: Map[DFRef.TwoWayAny, DFMember] =
    members.view.flatMap(origMember => origMember.getRefs.map(_ -> origMember)).toMap

  //                                                             to         From
  private def _originMemberTable(excludeTypeRef: Boolean): Map[DFMember, Set[DFMember]] =
    val tbl = mutable.Map.empty[DFMember, Set[DFMember]]
    members.foreach(origMember =>
      origMember.getRefs.foreach {
        case _: DFRef.Empty                     =>
        case _: DFRef.TypeRef if excludeTypeRef =>
        case r                                  =>
          tbl.updateWith(
            refTable.getOrElse(
              r,
              throw new NoSuchElementException(
                s"Missing member of reference $r:\n$origMember\n${origMember.getOwnerNamed.getFullName}"
              )
            )
          ) {
            case Some(set) => Some(set + origMember)
            case None      => Some(Set(origMember))
          }
      }
    )
    tbl.toMap
  end _originMemberTable

  lazy val originMemberTable: Map[DFMember, Set[DFMember]] = _originMemberTable(false)
  lazy val originMemberTableNoTypeRef: Map[DFMember, Set[DFMember]] = _originMemberTable(true)

  // Map of all named types in the design with their design block owners.
  // If the named type is global (used in IO, by a global member, or more than one design block),
  // then its owner is set to None.
  private lazy val namedDFTypes: ListMap[NamedDFType, Option[DFDesignBlock]] =
    members.foldLeft(ListMap.empty[NamedDFType, Option[DFDesignBlock]]) {
      case (namedDFTypeMap, namedDFTypeMember @ NamedDFTypes(dfTypes)) =>
        if (namedDFTypeMember.isPort)
          namedDFTypeMap ++ dfTypes.map(t => (t -> None)) // IO means a global named type
        else
          dfTypes.foldLeft(namedDFTypeMap) { case (namedDFTypeMap, dfType) =>
            namedDFTypeMap.get(dfType) match
              case Some(Some(owner)) => // named type already found
                if (owner == namedDFTypeMember.getOwnerDesign)
                  namedDFTypeMap // same design block -> nothing to do
                else
                  namedDFTypeMap +
                    (dfType -> None) // used in more than one block -> global named type
              case Some(None) => namedDFTypeMap // known to be a global type
              // found new named type
              case None =>
                // if referenced by a global member -> global named type
                if (namedDFTypeMember.isGlobal)
                  namedDFTypeMap + (dfType -> None)
                else
                  namedDFTypeMap + (dfType -> Some(namedDFTypeMember.getOwnerDesign))
          }
      case (namedDFTypeMap, _) => namedDFTypeMap // not a named type member
    }

  private lazy val invertedNamedDFTypes = namedDFTypes.invert
  lazy val getGlobalNamedDFTypes: ListSet[NamedDFType] =
    invertedNamedDFTypes.getOrElse(None, ListSet())
  private lazy val localNamedDFTypes: Map[DFDesignBlock, ListSet[NamedDFType]] =
    invertedNamedDFTypes.flatMap {
      case (Some(b), set) => Some(b -> set)
      case _              => None
    }
  def getLocalNamedDFTypes(design: DFDesignBlock): Set[NamedDFType] =
    localNamedDFTypes.getOrElse(design, Set())

  // Cross-design global named types for the hierarchical root. Each sub-DB only
  // sees one design, so a type used as IO in a child but as a plain local in a
  // parent gets classified global by the child and local by the parent — the
  // union of every sub-DB's `getGlobalNamedDFTypes` is the authoritative global
  // set. The printer prints these once globally and excludes them from every
  // design's local type declarations. Empty on non-root DBs (a flat DB already
  // classifies named types directly).
  // TODO: a named type used as a plain local in more than one design (never as
  // IO/global) is global in the flat model but not captured here; add the
  // multi-design count if such a case arises.
  lazy val hierGlobalNamedDFTypes: ListSet[NamedDFType] =
    if (!isRoot) ListSet.empty
    else subDBs.view.values.flatMap(_.getGlobalNamedDFTypes).to(ListSet)

  @tailrec private def OMLGen[O <: DFOwner: ClassTag](
      getOwnerFunc: DFMember => O
  )(
      oml: List[(O, List[DFMember])],
      globalMembers: List[DFMember],
      localStack: List[(O, List[DFMember])]
  ): List[(O, List[DFMember])] =
//    if (localStack.isEmpty) this.sanityCheck
    val ((localOwner, localMembers), updatedStack0) =
      (localStack.head, localStack.drop(1))
    globalMembers match
      // DFDesignBlock is a Top-like definition — open its own scope without
      // calling getOwnerFunc on it (a DFDesignBlock with ownerRef = Empty
      // would throw). The DFS pre-order layout guarantees the current
      // stack-top is the intended parent at this point, and Step 1 of
      // "Drop DFDesignBlock as Instance Member" has it not appear in any
      // parent's member list.
      case (d: DFDesignBlock) :: mList =>
        d match
          case o: O if classTag[O].runtimeClass.isInstance(o) =>
            val updatedStack2 = (o -> List()) :: localStack
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack2)
          case _ =>
            // O does not include DFDesignBlock — drop it silently.
            OMLGen[O](getOwnerFunc)(oml, mList, localStack)
      // current member indeed belongs to current owner
      case m :: mList if getOwnerFunc(m) == localOwner =>
        m match
          // Deep borrowing into block as the new owner
          case o: O if classTag[O].runtimeClass.isInstance(o) =>
            val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
            val updatedStack2 = (o -> List()) :: updatedStack1
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack2)
          // Just a member
          case _ =>
            val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack1)
      // current member does not belong to current owner
      case x :: xs =>
        val updatedOML = (localOwner -> localMembers.reverse) :: oml
        OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
      case Nil if updatedStack0.nonEmpty =>
        val updatedOML = (localOwner -> localMembers.reverse) :: oml
        OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
      case Nil =>
        (localOwner -> localMembers.reverse) :: oml
    end match
  end OMLGen

  def getMembersOf(owner: DFOwner, memberView: MemberView)(using MemberGetSet): List[DFMember] =
    memberView match
      case MemberView.Folded =>
        ownerMemberTable(owner)
      case MemberView.Flattened =>
        def recur(owner: DFOwner): List[DFMember] =
          val members = ownerMemberTable(owner)
          members.flatMap {
            case d: DFDesignBlock => Some(d)
            case o: DFOwner       => o :: recur(o)
            case m                => Some(m)
          }
        end recur
        owner match
          case d: DFDesignBlock => designMemberTable(d)
          case _                => recur(owner)

  // Members to feed into OMLGen as "locals excluding top". Both old-style
  // and new-style (option-a) DBs have the top designBlock at
  // `membersNoGlobals.head`, so dropping it yields the locals uniformly.
  private lazy val localsForOML: List[DFMember] = membersNoGlobals.drop(1)

  // holds the topological order of owner owner dependency
  lazy val ownerMemberList: List[(DFOwner, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFOwner](_.getOwner)(List(), localsForOML, List(top -> List())).reverse

  // holds a hash table that lists members of each owner. The member list order is maintained.
  lazy val ownerMemberTable: Map[DFOwner, List[DFMember]] =
    Map(ownerMemberList*)

  // holds the topological order of named owner block dependency
  lazy val namedOwnerMemberList: List[(DFOwnerNamed, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFOwnerNamed](_.getOwnerNamed)(
      List(),
      localsForOML,
      List(top -> List())
    ).reverse

  // holds a hash table that lists members of each named owner. The member list order is maintained.
  lazy val namedOwnerMemberTable: Map[DFOwnerNamed, List[DFMember]] =
    Map(namedOwnerMemberList*)

  // holds the topological order of domain owner dependency
  lazy val domainOwnerMemberList: List[(DFDomainOwner, List[DFMember])] =
    if (isRoot)
      subDBs.view.values.flatMap(_.domainOwnerMemberList).toList
    else
      // head will always be the TOP owner
      OMLGen[DFDomainOwner](_.getOwnerDomain)(
        List(),
        localsForOML,
        List(top -> List())
      ).reverse

  // holds a hash table that lists members of each named owner. The member list order is maintained.
  lazy val domainOwnerMemberTable: Map[DFDomainOwner, List[DFMember]] =
    Map(domainOwnerMemberList*)

  // holds the topological order of owner block dependency
  lazy val blockMemberList: List[(DFBlock, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFBlock](_.getOwnerBlock)(List(), localsForOML, List(top -> List())).reverse

  // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val blockMemberTable: Map[DFBlock, List[DFMember]] =
    Map(blockMemberList*)

  // holds the topological order of design block dependency
  lazy val designMemberList: List[(DFDesignBlock, List[DFMember])] =
    if (isRoot) subDBs.view.values.map(db => db.top -> db.membersNoGlobals).toList
    else
      // head will always be the TOP block
      OMLGen[DFDesignBlock](_.getOwnerDesign)(
        List(),
        localsForOML,
        List(top -> List())
      ).reverse

  // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val designMemberTable: Map[DFDesignBlock, List[DFMember]] =
    Map(designMemberList*)

  // design block to its instances map
  lazy val designBlockInstMap: Map[DFDesignBlock, List[DFDesignInst]] =
    if (isRoot) subDBs.view.values.flatMap { subDB =>
      // need to use the context of each subDB to be able to call `.getDesignBlock`
      // TODO: in the future, once all stages are updated to use the new-style root DB,
      // we can use the DFDesignBlock's ownerRef as a specialized ref instead.
      subDB.atGetSet {
        subDB.membersNoGlobals.view.collect {
          case inst: DFDesignInst => inst -> inst.getDesignBlock
        }
      }
    }.groupBy(_._2).view.mapValues(_.map(_._1).toList).toMap + (top -> Nil)
    else
      members.view.collect { case inst: DFDesignInst => inst }
        .groupBy(_.getDesignBlock).view.mapValues(_.toList).toMap + (top -> Nil)

  // design block to its owner design blocks map (multiple owners in case of multiple
  // instantiations). A method (def design) is owned through its CALLS (`Func` with
  // `Op.Def`), since it has no design instance.
  lazy val designBlockOwnershipMap: Map[DFDesignBlock, Set[DFDesignBlock]] =
    if (isOldStyleFlatDB)
      members.view.collect {
        case inst: DFDesignInst => inst.getDesignBlock -> inst.getOwnerDesign
        // a call at GLOBAL scope (a static function computing a global value) has no owning
        // design; it is a global method, recognized separately by the printer's global-method
        // emission
        case DFVal.Func.Call(call, key) if !call.isGlobal =>
          key.getDesignBlock -> call.getOwnerDesign
      }.toList.groupBy(_._1).view.mapValues(_.view.map(_._2).toSet).toMap + (top -> Set.empty)
    else if (isRoot)
      subDBs.view.values.flatMap { subDB =>
        subDB.atGetSet {
          subDB.membersNoGlobals.view.collect {
            case inst: DFDesignInst      => inst.getDesignBlock -> subDB.top
            case DFVal.Func.Call(_, key) => key.getDesignBlock -> subDB.top
          }
        }
      }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap + (top -> Set.empty)
    else rootDB.designBlockOwnershipMap

  lazy val parentSubDBOpt: Option[DB] =
    if (isOldStyleFlatDB) None
    else if (isRoot) None
    else designBlockOwnershipMap.get(top).flatMap(_.headOption).flatMap { parentBlock =>
      rootDB.subDBs.get(parentBlock.ownerRef)
    }

  lazy val designBlockDomainOwnershipMap: Map[DFDesignBlock, Set[DFDomainOwner]] =
    if (isOldStyleFlatDB)
      members.view.collect {
        case inst: DFDesignInst => inst.getDesignBlock -> inst.getOwnerDomain
        case DFVal.Func.Call(call, key) if !call.isGlobal =>
          key.getDesignBlock -> call.getOwnerDomain
      }.toList.groupBy(_._1).view.mapValues(_.view.map(_._2).toSet).toMap + (top -> Set.empty)
    else if (isRoot)
      subDBs.view.values.flatMap { subDB =>
        subDB.atGetSet {
          subDB.membersNoGlobals.view.collect {
            case inst: DFDesignInst         => inst.getDesignBlock -> inst.getOwnerDomain
            case DFVal.Func.Call(call, key) => key.getDesignBlock -> call.getOwnerDomain
          }
        }
      }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap + (top -> Set.empty)
    else rootDB.designBlockDomainOwnershipMap

  lazy val designInstPBNS: Map[DFDesignInst, List[DFVal.PortByNameSelect]] =
    members.collect { case pbns: DFVal.PortByNameSelect => pbns }.groupBy(_.getDesignInst)

  private def conditionalChainGen: Map[DFConditional.Header, List[DFConditional.Block]] =
    val handled = mutable.Set.empty[DFConditional.Block]
    members.foldRight(
      Map.empty[DFConditional.Header, List[DFConditional.Block]]
    ) {
      case (m: DFConditional.Block, chainMap) if !handled.contains(m) =>
        @tailrec def getChain(
            block: DFConditional.Block,
            chain: List[DFConditional.Block]
        ): (DFConditional.Header, List[DFConditional.Block]) =
          handled += block
          block.prevBlockOrHeaderRef.get match
            case header: DFConditional.Header   => (header, block :: chain)
            case prevBlock: DFConditional.Block =>
              getChain(prevBlock, block :: chain)
        chainMap + getChain(m, Nil)
      case (_, chainMap) => chainMap
    }
  end conditionalChainGen
  // Maps the conditional construct header with the entire case/ifelse block chain
  lazy val conditionalChainTable: Map[DFConditional.Header, List[DFConditional.Block]] =
    conditionalChainGen

  private enum Access derives CanEqual:
    case Read, Write, ReadWrite, Unknown, Error
  import Access.*
  import DFVal.Modifier.*
  import DFNet.Op.*
  private def getValAccess(dfVal: DFVal, slice: Slice, net: DFNet)(
      connToMap: ConnectToMap
  ): Access =
    def isExternalConn =
      if (dfVal.isGlobal) true
      else if (dfVal.isInstanceOf[DFVal.PortByNameSelect]) true
      else dfVal.getOwnerDesign isSameOwnerDesignAs net
    def isInternalConn =
      if (dfVal.isGlobal) false
      else if (dfVal.isInstanceOf[DFVal.PortByNameSelect]) false
      else dfVal isSameOwnerDesignAs net
    dfVal match
      case dfVal: (DFVal.Dcl | DFVal.PortByNameSelect) =>
        val dir = dfVal match
          case dcl: DFVal.Dcl               => dcl.modifier.dir
          case pbns: DFVal.PortByNameSelect => pbns.dir
        dir match
          // external connection to an input port
          case IN if isExternalConn => Write
          // internal connection to an output port
          case OUT if isInternalConn => Write
          // external connection to an output port
          case OUT if isExternalConn => Read
          // internal connection to an input port
          case IN if isInternalConn => Read
          // internal or external connections to input/output port
          case INOUT if isExternalConn || isInternalConn => ReadWrite
          // internal connection to a var
          case VAR if isInternalConn =>
            // if already was connected as write, then it must be read
            if (connToMap.contains(dfVal, slice)) Read
            // otherwise it is unknown
            else Unknown
          // illegal connection
          case _ => Error
        end match
      case open if open.isOpen => Unknown
      case _                   => Read
    end match
  end getValAccess
  private def getValAccess(dfVal: DFVal, net: DFNet)(
      connToMap: ConnectToMap
  ): Access =
    val dpart = dfVal.departial
    getValAccess(dpart._1, dpart._2, net)(connToMap)
  private case class FlatNet(lhsVal: DFVal, rhsVal: DFVal, net: DFNet) derives CanEqual
  private object FlatNet:
    def apply(net: DFNet): List[FlatNet] =
      (net.lhsRef.get, net.rhsRef.get) match
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          List(FlatNet(lhsVal, rhsVal, net))
  end FlatNet
  given printer: Printer = DefaultPrinter
  @tailrec private def getConnToMap(
      analyzeNets: List[FlatNet],
      pendingNets: List[FlatNet],
      connToMap: ConnectToMap,
      errors: List[String]
  ): ConnectToMap =
    analyzeNets match
      case flatNet :: otherNets =>
        var newErrors = errors
        extension (dfVal: DFVal)
          def relValString: String =
            printer.csDFValRef(dfVal, flatNet.net.getOwnerDesign)
        def newError(errMsg: String): Unit =
          val errMsgComplete =
            s"""|DFiant HDL connectivity error!
                |Position:  ${flatNet.net.meta.position}
                |Hierarchy: ${flatNet.net.getOwnerDesign.getFullName}
                |LHS:       ${flatNet.lhsVal.relValString}
                |RHS:       ${flatNet.rhsVal.relValString}
                |Message:   ${errMsg}""".stripMargin
          newErrors = errMsgComplete :: newErrors
        import flatNet.{lhsVal, rhsVal, net}
        // `OPEN` marks an entire output port of a design instance as deliberately unconnected.
        // That is the only partner the backends can render (`OPEN` is printed as the actual of
        // a port in an instantiation port map) and the only one with a derivable direction,
        // since `OPEN` carries no value and so can never drive. `ViaConnection` relies on the
        // same shape: it moves such a net into the port map instead of bridging the port
        // through a via variable, and any other partner is left as an undirected
        // `variable <> OPEN` net there, which no longer resolves below.
        def openCheck(other: DFVal): Unit =
          val legal = other match
            case pbns: DFVal.PortByNameSelect => pbns.isOut
            // the flat-DB form of the same shape: the port declaration itself, referenced from
            // the design that instantiates its owner (mirrors `getValAccess.isExternalConn`)
            case dcl: DFVal.Dcl =>
              dcl.modifier.dir == OUT && (dcl.getOwnerDesign isSameOwnerDesignAs net)
            case _ => false
          if (!legal)
            newError(
              s"""|Cannot connect `OPEN` to ${other.relValString}.
                  |`OPEN` marks an entire output port of a design instance as deliberately
                  |unconnected, so it can never drive a value nor cover just part of a port.
                  |To Fix:
                  |* An input port of a design instance must be driven: connect a value to it.
                  |* To leave only some bits of an output port unused, just do not connect them.""".stripMargin
            )
        end openCheck
        // A design's own output port is writable from inside the design, and readable there too
        // (`DropOutportRead` shadows the read through a signal for the dialects that forbid it).
        // So a connection between two of them (`y <> x`) is the one shape whose direction cannot
        // be derived: either side could be the sink. `<>` is commutative everywhere else, and
        // this ambiguity is its single exception: the LHS is taken as the driven side, so the
        // connection reads like an assignment.
        def isOwnOutPort(dfVal: DFVal): Boolean =
          dfVal.departial._1 match
            case dcl: DFVal.Dcl => dcl.modifier.dir == OUT && (dcl isSameOwnerDesignAs net)
            case _              => false
        val (lhsAccess, rhsAccess) = net.op match
          // assignment is always from right to left
          case Assignment | NBAssignment =>
            lhsVal.dealias match
              case Some(dfVal) if dfVal.isPortInPBNS =>
                newError("Cannot assign to an input port.")
                (Unknown, Unknown)
              case Some(_: DFVal.PortByNameSelect) =>
                newError("Ports can only be assigned at their own design scope.")
                (Unknown, Unknown)
              case _ =>
                (Write, Read)
          // connections are analyzed according to the context of the net
          case _ =>
            if (lhsVal.isOpen) openCheck(rhsVal)
            else if (rhsVal.isOpen) openCheck(lhsVal)
            (getValAccess(lhsVal, net)(connToMap), getValAccess(rhsVal, net)(connToMap))
        val toValOption = (lhsAccess, rhsAccess) match
          case (Write, Read | ReadWrite | Unknown) => Some(lhsVal)
          case (Read | ReadWrite | Unknown, Write) => Some(rhsVal)
          case (Read, Read)                        =>
            newError("Unsupported read-to-read connection.")
            None
          // the LHS-favouring exception, see `isOwnOutPort`
          case (Write, Write) if isOwnOutPort(lhsVal) && isOwnOutPort(rhsVal) => Some(lhsVal)
          case (Write, Write)                                                 =>
            newError("Unsupported write-to-write connection.")
            None
          case (_, Read)  => Some(lhsVal)
          case (Read, _)  => Some(rhsVal)
          case (Error, _) =>
            newError(s"Unknown access pattern with ${lhsVal.relValString}.")
            None
          case (_, Error) =>
            newError(s"Unknown access pattern with ${rhsVal.relValString}.")
            None
          case _ => None
        val toValAndSliceOption: Option[(ConnectToVal, Slice)] = toValOption.flatMap(v =>
          v.departialPBNS match
            case None =>
              newError(s"Unexpected write access to the immutable value ${v.relValString}.")
              None
            case Some(connectToVal, Slice.Concrete(range))
                if connectToVal.dfType.widthIntOpt.exists(_ < range.length) =>
              newError(s"Unexpected write access to the immutable value ${v.relValString}.")
              None
            case x => x
        )
        toValAndSliceOption match
          // found target variable or port declaration for the given connection/assignment
          case Some(connectToVal, slice) =>
            val prevNets = connToMap.getNets(connectToVal, slice)
            // checking multiple assignments from different domains, except for a condition
            // where the declaration is a shared variable.
            // this is used to define a shared variable which is against the RT model,
            // but is useful to described inferred memories like True Dual Port RAM.
            val isSharedVar = connectToVal match
              case dcl: DFVal.Dcl if dcl.modifier.isShared => true
              case _                                       => false
            if (!isSharedVar)
              prevNets.headOption.foreach: prevNet =>
                if (prevNet.getOwnerDomain != net.getOwnerDomain)
                  newError(
                    s"""|Found multiple domain assignments to the same variable/port `${connectToVal.getFullName}`.
                        |Only variables declared as `VAR.SHARED` under ED domain allow this.
                        |The previous write occurred at ${prevNet.meta.position}""".stripMargin
                  )
            // go through all previous nets and check for collisions
            prevNets.foreach: prevNet =>
              // multiple assignments are allowed in the same range, but not multiple
              // connections or a combination of an assignment and a connection
              if (prevNet.isConnection || prevNet.isAssignment && !net.isAssignment)
                newError(
                  s"""Found multiple connections write to the same variable/port `${connectToVal.getFullName}`.
                     |The previous write occurred at ${prevNet.meta.position}""".stripMargin
                )
            // if no previous connection in this range, we add it to the range map
            if (prevNets.isEmpty)
              getConnToMap(
                otherNets,
                pendingNets,
                connToMap.addNet(connectToVal, slice, net),
                newErrors
              )
            // if there are previous connections, it's either assignments or already reported as
            // errors, so no need to further modify the range map (the range map is not intended
            // to save all the previous assignment nets).
            else
              getConnToMap(otherNets, pendingNets, connToMap, newErrors)
          // unable to determine net directionality, so move net to pending
          case None =>
            getConnToMap(otherNets, flatNet :: pendingNets, connToMap, newErrors)
        end match
      case Nil if errors.nonEmpty =>
        throw new IllegalArgumentException(
          errors.view.reverse.mkString("\n\n")
        )
      case Nil if pendingNets.nonEmpty =>
        val reexamine = pendingNets.exists { n =>
          connToMap.contains(n.lhsVal) | connToMap.contains(n.rhsVal)
        }
        if (reexamine) getConnToMap(pendingNets, Nil, connToMap, errors)
        else
          throw new IllegalArgumentException(
            s"""DFiant HDL connectivity errors!
               |Unable to determine directionality for the following nets:
               |${pendingNets.map(_.net.meta.position).mkString("\n")}""".stripMargin
          )
      case Nil =>
        connToMap
    end match
  end getConnToMap

  // Magnet connection map + per-point (owner design, name) info, computed on the
  // root DB. Magnet matching is intrinsically cross-design; `MagnetMap.get`
  // precomputes each magnet point's design context per sub-DB, then matches on
  // the root-aware design tree (no flattening). The point info lets consumers
  // avoid re-resolving a cross-design ConnectPoint (which would need a flat
  // member index).
  private lazy val magnetData
      : (Map[ConnectPoint, ConnectPoint], Map[ConnectPoint, (DFDesignBlock, String)]) =
    if (!isRoot) rootDB.magnetData
    else MagnetMap.get(this)
  lazy val magnetConnectionMap: Map[ConnectPoint, ConnectPoint] = magnetData._1
  lazy val magnetPointInfo: Map[ConnectPoint, (DFDesignBlock, String)] = magnetData._2

  // Dangling-port check, run on the root DB. The assignment coverage and the
  // connected-point set are aggregated across all sub-DBs (each design's
  // assignments/connections live in its own sub-DB) plus the cross-design
  // magnet connections. Input ports are checked from each parent sub-DB (which
  // owns the instance and its connections), reading the child design's port
  // list via the root-aware designMemberTable; output ports are checked per
  // instantiated design under its own sub-DB getSet. Only instantiated designs
  // (designBlockInstMap keys) are checked — the toptop's own ports are device
  // IO, not dangling.
  def checkDanglingPorts(): Unit =
    val assignmentsDclTable: Map[DFVal.Dcl, Coverage] =
      subDBs.view.values.flatMap { sub =>
        // resolve the dcl width inside the sub-DB's getSet (a DFBits width-param
        // ref can't be resolved against the root getSet)
        sub.atGetSet {
          sub.assignmentsTable.keys.flatMap(_.departialDcl).map { case (dcl, slice) =>
            (dcl, slice, dcl.dfType.widthIntOpt)
          }.toList
        }
      }.foldLeft(Map.empty[DFVal.Dcl, Coverage]) { case (acc, (dcl, slice, widthOpt)) =>
        acc.updated(dcl, acc.getOrElse(dcl, Coverage.empty).assign(slice, widthOpt))
      }
    // A procedural call's `<> OUT` argument actual is written by the call, exactly like a
    // connection to it, so it counts toward the actual's connectivity coverage. The direction
    // lives in the called def's formal port: the actuals resolve in the CALLER's sub-DB, the
    // formal directions in the DEF's own sub-DB (elaboration runs over root+subDBs, so the def's
    // members are not in the caller's table). Formal ports are in declaration order, aligned with
    // the call's args; a trailing return output port (a function's result) has no actual and is
    // dropped by the zip.
    val outArgActualPoints: Set[ConnectPoint] =
      subDBs.view.values.flatMap { sub =>
        sub.atGetSet {
          sub.members.view.collect { case DFVal.Func.Call(call, staticRef) =>
            val formalDirs = domainOwnerToSubDB(staticRef.getDesignBlock).atGetSet {
              designMemberTable(staticRef.getDesignBlock).collect {
                case dcl: DFVal.Dcl if dcl.modifier.isPort => dcl.modifier.dir
              }
            }
            call.args.view.map(_.get).zip(formalDirs).collect {
              case (actual, DFVal.Modifier.OUT | DFVal.Modifier.INOUT) => actual
            }.toList
          }.flatten.collect {
            case dcl: DFVal.Dcl               => ConnectPoint.Direct(dcl)
            case pbns: DFVal.PortByNameSelect => ConnectPoint.Via(pbns)
          }.toList
        }
      }.toSet
    val alreadyConnectedPoints: Set[ConnectPoint] =
      subDBs.view.values.flatMap { sub =>
        sub.atGetSet {
          sub.connectionTable.connectToVals.view.collect {
            case dcl: DFVal.Dcl               => ConnectPoint.Direct(dcl)
            case pbns: DFVal.PortByNameSelect => ConnectPoint.Via(pbns)
          }.toList
        }
      }.toSet ++ magnetConnectionMap.keySet ++ outArgActualPoints
    // input ports: checked from the parent sub-DB that owns the instance
    val danglingInputs = subDBs.view.values.flatMap { parentSub =>
      parentSub.atGetSet {
        parentSub.membersNoGlobals.view.collect { case inst: DFDesignInst => inst }.flatMap {
          designInst =>
            val childDesign = designInst.getDesignBlock
            val instFullName = designInst.getFullName
            val instPos = designInst.meta.position
            domainOwnerToSubDB(childDesign).atGetSet {
              designMemberTable(childDesign).view.collect {
                case port: DFVal.Dcl if port.isPortIn && !port.isClkDcl && !port.isRstDcl =>
                  (ConnectPoint.Via(designInst, port), port.getName)
              }.toList
            }.flatMap { case (via, portName) =>
              if (alreadyConnectedPoints.contains(via)) None
              else
                Some(
                  s"""|DFiant HDL connectivity error!
                      |Position:  ${instPos}
                      |Hierarchy: ${instFullName}
                      |Message:   Found a dangling (unconnected) input port `${portName}`.""".stripMargin
                )
            }
        }.toList
      }
    }
    // output ports: checked per instantiated design under its own sub-DB getSet
    val danglingOutputs = designBlockInstMap.keys.view.flatMap { design =>
      domainOwnerToSubDB(design).atGetSet {
        designMemberTable(design).view.collect {
          case port: DFVal.Dcl
              if port.isPortOut && !design.isBlackBox && !port.hasNonBubbleInit &&
                !assignmentsDclTable.contains(port) &&
                !alreadyConnectedPoints.contains(ConnectPoint.Direct(port)) =>
            s"""|DFiant HDL connectivity error!
                |Position:  ${port.meta.position}
                |Hierarchy: ${design.getFullName}
                |Message:   Found a dangling (unconnected/unassigned and uninitialized) output port `${port.getName}`.""".stripMargin
        }.toList
      }
    }
    val danglingPorts = (danglingInputs ++ danglingOutputs).toList
    if (danglingPorts.nonEmpty)
      throw new IllegalArgumentException(danglingPorts.mkString("\n"))
  end checkDanglingPorts

  extension (domainOwner: DFDomainOwner)
    // Aggregates `@hw.constraints.IO` / `@timing.clock` annotations applied at
    // the domain owner and on its (single) clk-port-in declaration. Used by the
    // resolver and by the device-top location-collision check.
    private def getDomainClkConstraintsView: collection.View[constraints.Constraint] =
      domainOwner.getConstraints.view ++
        domainOwnerMemberTable(domainOwner).view.collectFirst {
          case dcl: DFVal.Dcl if dcl.isPortIn && dcl.isClkDcl => dcl.getConstraints
        }.getOrElse(Nil)
    private def getTimingConstraintClkRateOpt: Option[RateNumber] =
      getDomainClkConstraintsView.collectFirst {
        case constraints.Timing.Clock(rate = rate: RateNumber @unchecked) => rate
      }
    // Plan-mandated "forcing" semantics: an explicit `@timing.clock` / `@timing.reset`
    // annotation on the owner counts as "this slot is required" even when no member
    // generates the usage (covers bare-annotation forcing on blackbox / combinational
    // owners and partial-override forms).
    private def hasClkAnnot: Boolean = domainOwner.meta.annotations.exists {
      case _: constraints.Timing.Clock => true
      case _                           => false
    }
    private def hasRstAnnot: Boolean = domainOwner.meta.annotations.exists {
      case _: constraints.Timing.Reset => true
      case _                           => false
    }
  end extension

  // ===========================================================================
  // RT clk/rst domain analyses, computed on the hierarchical ROOT DB. Each
  // routes ref resolution / per-design table lookups to the OWNING sub-DB's
  // getSet (the root getSet throws); a sub-DB delegates up to its root.
  // ===========================================================================

  // Cross-design reaches navigate the DESIGN TREE (no global member index):
  // `subDBs.get(d.ownerRef)` goes DOWN to a child design's sub-DB;
  // `subDB.parentSubDBOpt` goes UP to the parent sub-DB where that design is
  // instantiated. Each analysis processes one sub-DB's own members at a time,
  // under that sub-DB's getSet, hopping between neighbors via the tree.

  lazy val relatedAnnotMap: Map[DFDomainOwner, DFDomainOwner] =
    if (!isRoot) rootDB.relatedAnnotMap
    else
      subDBs.view.values.flatMap { sub =>
        sub.atGetSet {
          sub.domainOwnerMemberList.view.flatMap { (owner, _) =>
            owner.meta.annotations.collectFirst {
              case rel: constraints.Timing.Related => rel.ref.get
            }.map(owner -> _)
          }
        }
      }.toMap

  // Resolves a PortByNameSelect (living in `ctxSub`) to its underlying Dcl by
  // navigating DOWN to the targeted child design's sub-DB and walking its
  // namedOwnerMemberTable there. Returns the port with the child sub-DB that
  // owns it, so callers can resolve the port's domain in the right getSet.
  private def pbnsToPort(
      pbns: DFVal.PortByNameSelect,
      ctxSub: DB
  ): Option[(DFVal.Dcl, DB)] =
    val childDesign = ctxSub.atGetSet(pbns.designInstRef.get.getDesignBlock)
    subDBs.get(childDesign.ownerRef).flatMap { childSub =>
      childSub.atGetSet {
        val pathParts = pbns.portNamePath.split('.').toList
        @tailrec def walk(owner: DFOwnerNamed, parts: List[String]): Option[DFMember] =
          parts match
            case Nil          => None
            case head :: rest =>
              childSub.namedOwnerMemberTable.getOrElse(owner, Nil).collectFirst {
                case n: DFMember.Named if !n.isAnonymous && n.getName == head => n
              } match
                case Some(m) if rest.isEmpty => Some(m)
                case Some(o: DFOwnerNamed)   => walk(o, rest)
                case _                       => None
        walk(childDesign, pathParts) match
          case Some(dcl: DFVal.Dcl) => Some(dcl -> childSub)
          case _                    => None
      }
    }
  end pbnsToPort

  // Resolve `design`'s own body members from its instantiation site: on a flat DB the
  // design's body members are directly in this DB's ownerMemberTable, while under a
  // hierarchical root the instantiation scope holds only the design's header, so we
  // navigate DOWN to the design's own sub-DB. The collector runs under the resolved
  // DB's getSet.
  private def fromDesignMembers[T](design: DFDesignBlock)(
      collect: List[DFMember] => MemberGetSet ?=> T
  ): T =
    def run(sub: DB): T = sub.atGetSet(collect(sub.ownerMemberTable.getOrElse(design, Nil)))
    if (rootDB.isRoot)
      rootDB.subDBs.get(design.ownerRef) match
        case Some(sub) => run(sub)
        case None      => atGetSet(collect(Nil))
    else run(this)

  // The names of `design`'s phantom-tagged design parameters (see PhantomTag). Used by
  // the DFHDL printer to drop phantom parameter applications at a method
  // instantiation site (only the def view form hides phantoms).
  def phantomParamNamesOf(design: DFDesignBlock): Set[String] =
    fromDesignMembers(design) { members =>
      members.view.collect {
        case param: DFVal.DesignParam if param.isPhantom => param.getName
      }.toSet
    }

  // Whether `design` has any phantom-tagged ports or parameters (see PhantomTag). A
  // method with phantoms references its host's values by name, so the DFHDL printer
  // prints its declaration locally in the host design's body (just before the def's
  // first instance) instead of at file level.
  def designHasPhantoms(design: DFDesignBlock): Boolean =
    fromDesignMembers(design) { members =>
      members.exists {
        case dfVal @ (_: DFVal.Dcl | _: DFVal.DesignParam) => dfVal.isPhantom
        case _                                             => false
      }
    }

  // The domain that encloses `design`'s instantiation: navigate UP to `design`'s
  // parent sub-DB and read the owning domain of `design`'s inst there. Returns
  // the domain owner with the parent sub-DB that owns it.
  private def designEnclosingDomain(design: DFDesignBlock): Option[(DFDomainOwner, DB)] =
    subDBs.get(design.ownerRef).flatMap(_.parentSubDBOpt).flatMap { parentSub =>
      parentSub.atGetSet {
        parentSub.membersNoGlobals.view.collectFirst {
          case inst: DFDesignInst if inst.getDesignBlock eq design => inst.getOwnerDomain
        }
      }.map(_ -> parentSub)
    }

  // `member` lives in `ctxSub`; returns its RT domain owner together with the
  // sub-DB that owns that domain (None if the resolved domain is not RT).
  private def getRTOwnerWithSub(
      member: DFMember,
      ctxSub: DB
  ): Option[(DFDomainOwner, DB)] =
    val ownerAndSub: Option[(DFDomainOwner, DB)] = member match
      case design: DFDesignBlock        => designEnclosingDomain(design)
      case pbns: DFVal.PortByNameSelect =>
        pbnsToPort(pbns, ctxSub) match
          case Some((port, portSub)) => Some(portSub.atGetSet(port.getOwnerDomain) -> portSub)
          case None                  => Some(ctxSub.atGetSet(pbns.getOwnerDomain) -> ctxSub)
      case _ => Some(ctxSub.atGetSet(member.getOwnerDomain) -> ctxSub)
    ownerAndSub.flatMap { case (o, sub) =>
      o.domainType match
        case DomainType.RT => Some(o -> sub)
        case _             => None
    }
  end getRTOwnerWithSub

  // Best-effort full instance path for `owner` (error messages only). `owner`
  // lives in `ownerSub`; instance paths are recovered by navigating UP.
  private def fullNameViaInst(owner: DFDomainOwner, ownerSub: DB): String =
    val design = ownerSub.atGetSet(owner.getThisOrOwnerDesign)
    if (design eq topDB.top) ownerSub.atGetSet(owner.getFullName)
    else
      // The enclosing design's `inst.getFullName` yields the design CLASS name
      // (e.g. `Internal2`), not the instance path: resolve the
      // enclosing design by its own full name in its sub-DB (where it is the
      // top), then append the owner's design-relative name.
      owner match
        case d: DFDesignBlock => ownerSub.atGetSet(d.getFullName)
        case _                =>
          val designName = ownerSub.atGetSet(design.getFullName)
          s"$designName.${ownerSub.atGetSet(owner.getRelativeName(design))}"
  end fullNameViaInst

  lazy val dependentRTDomainOwners: Map[DFDomainOwner, DFDomainOwner] =
    if (!isRoot) rootDB.dependentRTDomainOwners
    else
      subDBs.view.values.flatMap { subDB =>
        subDB.domainOwnerMemberList.view.flatMap { case (domainOwner, domainMembers) =>
          subDB.atGetSet {
            domainOwner.domainType match
              case DomainType.RT =>
                relatedAnnotMap.get(domainOwner) match
                  case Some(relatedOwner) => Some(domainOwner -> relatedOwner)
                  case None               =>
                    val hasClkOrRst = domainOwner.meta.annotations.exists {
                      case _: constraints.Timing.Clock => true
                      case _: constraints.Timing.Reset => true
                      case _                           => false
                    }
                    if (hasClkOrRst) None
                    else
                      domainOwner match
                        case design: DFDesignBlock =>
                          // The absolute top is the root's top-top design; a
                          // sub-DB's own `isTopTop`/`isTop` can't tell because
                          // every design block has an Empty ownerRef.
                          if (design eq topDB.top) None
                          else getRTOwnerWithSub(design, subDB).map { case (o, _) =>
                            design -> o
                          }
                        case domain: DomainBlock =>
                          val ed = domain.getOwnerDesign
                          val portRelNames = domainMembers.collect {
                            case dcl: DFVal.Dcl
                                if dcl.isPortIn && !dcl.isClkDcl && !dcl.isRstDcl =>
                              dcl -> dcl.getRelativeName(ed)
                          }
                          // External drivers come from `ed`'s instantiations in
                          // each PARENT sub-DB — navigate the design tree up.
                          val parentDesigns = designBlockOwnershipMap.getOrElse(ed, Set.empty)
                          val inSources: Set[(DFDomainOwner, DB)] =
                            portRelNames.view.flatMap { case (port, portRelName) =>
                              val externalNets: List[(DFNet, DB)] =
                                parentDesigns.toList.flatMap { parentDesign =>
                                  subDBs.get(parentDesign.ownerRef).toList.flatMap { parentSub =>
                                    parentSub.atGetSet {
                                      parentSub.membersNoGlobals.collect {
                                        case inst: DFDesignInst
                                            if inst.getDesignBlock eq ed => inst
                                      }.flatMap { inst =>
                                        parentSub.designInstPBNS.getOrElse(inst, Nil)
                                          .filter(_.portNamePath == portRelName)
                                          .flatMap(parentSub.connectionTable.getNets(_))
                                      }
                                    }.map(_ -> parentSub)
                                  }
                                }
                              val localNets: List[(DFNet, DB)] =
                                subDB.connectionTable.getNets(port).toList.map(_ -> subDB)
                              (localNets ++ externalNets).headOption.flatMap { case (net, netSub) =>
                                netSub.atGetSet {
                                  net match
                                    case DFNet.Connection(_, from, _) =>
                                      getRTOwnerWithSub(from, netSub)
                                    case _ => None
                                }
                              }
                            }.toSet
                          val inSourceDomains = inSources.map { case (o, _) => o }
                          if (inSourceDomains.isEmpty)
                            getRTOwnerWithSub(domain, subDB).map { case (o, _) =>
                              domain -> o
                            }
                          else if (inSourceDomains.size > 1)
                            throw new IllegalArgumentException(
                              s"""|Found ambiguous source RT configurations for the domain:
                                  |${fullNameViaInst(domain, subDB)}
                                  |Sources:
                                  |${inSources.map { case (o, s) => fullNameViaInst(o, s) }
                                   .mkString("\n")}
                                  |Possible solution:
                                  |Either explicitly define a configuration for the domain or drive it from a single source domain.
                                  |""".stripMargin
                            )
                          else Some(domain -> inSourceDomains.head)
              case _ => None
          }
        }
      }.toMap

  // Hierarchical equivalent of the `isDependentOn` analysis: does `domainOwner`
  // transitively depend on `thatDomainOwner` per `dependentRTDomainOwners`?
  // Invoked on the root DB.
  @tailrec final def isDependentOn(
      domainOwner: DFDomainOwner,
      thatDomainOwner: DFDomainOwner
  ): Boolean =
    dependentRTDomainOwners.get(domainOwner) match
      case Some(dependency) =>
        if (dependency == thatDomainOwner) true
        else isDependentOn(dependency, thatDomainOwner)
      case None => false

  // Structural map: every domain owner -> its sub-DB. Keys are domain owners
  // only (designs + domain blocks), built from each sub-DB's own
  // domainOwnerMemberList — the same category as designBlockOwnershipMap, NOT a
  // member index. Routes a domain owner's getResolvedClkRst / usesClkRst to its
  // own getSet, including reverse-dependent domains reached by usesClk/usesRst.
  private lazy val domainOwnerToSubDB: Map[DFDomainOwner, DB] =
    if (isRoot)
      subDBs.view.values.flatMap { sub =>
        sub.domainOwnerMemberList.view.map { case (owner, _) => owner -> sub }
      }.toMap
    else rootDB.domainOwnerToSubDB

  // Resolved @timing.clock / @timing.reset per RT domain owner. Computed on the
  // hierarchical root; a sub-DB delegates to its root. This is the entry point
  // the CLK_FREQ const-folder (DFMember.Special) hits.
  lazy val resolvedClkRstMap: Map[DFDomainOwner, ClkRstTiming] =
    if (!isRoot) rootDB.resolvedClkRstMap
    else
      val reversedDependents: Map[DFDomainOwner, Set[DFDomainOwner]] =
        dependentRTDomainOwners.invert
      val designUsesClkRst =
        mutable.Map.empty[String, (usesClk: Boolean, usesRst: Boolean)]
      val domainOwnerUsesClkRst =
        mutable.Map.empty[DFDomainOwner, (usesClk: Boolean, usesRst: Boolean)]
      // run `f` under the getSet of `owner`'s sub-DB (looked up structurally)
      def atOwner[T](owner: DFDomainOwner)(f: MemberGetSet ?=> T): T =
        domainOwnerToSubDB(owner).atGetSet(f)
      // getSet-threaded clones of the clk/rst resolver extensions: the originals
      // bind to the class's `this.getSet` (which throws on the root), so they
      // can't be reused here — these take an explicit `using MemberGetSet` that
      // `atOwner` supplies from the owner's sub-DB.
      def domainClkConstraints(owner: DFDomainOwner)(using
          MemberGetSet
      ): collection.View[constraints.Constraint] =
        owner.getConstraints.view ++
          domainOwnerMemberTable(owner).view.collectFirst {
            case dcl: DFVal.Dcl if dcl.isPortIn && dcl.isClkDcl => dcl.getConstraints
          }.getOrElse(Nil)
      def timingClkRateOpt(owner: DFDomainOwner)(using MemberGetSet): Option[RateNumber] =
        domainClkConstraints(owner).collectFirst {
          case constraints.Timing.Clock(rate = rate: RateNumber @unchecked) => rate
        }
      def resolvedClkRst(owner: DFDomainOwner)(using MemberGetSet): ClkRstTiming =
        val defaultTag = globalTags.getTagOf[DefaultRTDomainCfgTag].get
        val userClkOpt = owner.meta.annotations.collectFirst {
          case c: constraints.Timing.Clock => c
        }
        val userRstOpt = owner.meta.annotations.collectFirst {
          case r: constraints.Timing.Reset => r
        }
        val isDeviceTop = owner.getThisOrOwnerDesign.isDeviceTop
        val explicitNoRst = userClkOpt.isDefined && userRstOpt.isEmpty
        val (baseClkOpt, baseRstOpt) =
          if (isDeviceTop)
            val clk = timingClkRateOpt(owner) match
              case Some(rate) => defaultTag.clk.copy(rate = rate)
              case None       => defaultTag.clk
            (Some(clk), None)
          else if (explicitNoRst) (Some(defaultTag.clk), None)
          else (Some(defaultTag.clk), Some(defaultTag.rst))
        def mergeClk(
            base: Option[constraints.Timing.Clock],
            user: Option[constraints.Timing.Clock]
        ): Option[constraints.Timing.Clock] = (base, user) match
          case (Some(b), Some(u)) =>
            Some(b.merge(u, withPriority = true).get.asInstanceOf[constraints.Timing.Clock])
          case (Some(b), None) => Some(b)
          case (None, Some(u)) => Some(u)
          case (None, None)    => None
        def mergeRst(
            base: Option[constraints.Timing.Reset],
            user: Option[constraints.Timing.Reset]
        ): Option[constraints.Timing.Reset] = (base, user) match
          case (Some(b), Some(u)) =>
            Some(b.merge(u, withPriority = true).get.asInstanceOf[constraints.Timing.Reset])
          case (Some(b), None) => Some(b)
          case (None, Some(u)) => Some(u)
          case (None, None)    => None
        (mergeClk(baseClkOpt, userClkOpt), mergeRst(baseRstOpt, userRstOpt))
      end resolvedClkRst
      def isAlwaysAtTopClk(owner: DFDomainOwner)(using MemberGetSet): Boolean =
        resolvedClkRst(owner)._1 match
          case Some(clk) =>
            clk.inclusionPolicy match
              case ClkRstInclusionPolicy.AlwaysAtTop => true
              case _                                 => false
          case None => false
      def isAlwaysAtTopRst(owner: DFDomainOwner)(using MemberGetSet): Boolean =
        resolvedClkRst(owner)._2 match
          case Some(rst) =>
            rst.inclusionPolicy match
              case ClkRstInclusionPolicy.AlwaysAtTop => true
              case _                                 => false
          case None => false
      def usesClkRst(owner: DFDomainOwner): (usesClk: Boolean, usesRst: Boolean) =
        owner match
          case design: DFDesignBlock =>
            designUsesClkRst.getOrElseUpdate(design.dclName, (usesClk(design), usesRst(design)))
          case _ =>
            domainOwnerUsesClkRst.getOrElseUpdate(owner, (usesClk(owner), usesRst(owner)))
      def isInitialPB(pb: ProcessBlock): Boolean = pb.sensitivity match
        case ProcessBlock.Sensitivity.Initial => true
        case _                                => false
      // An RT initial block counts as using reset only when it assigns a REG — semantically
      // identical to that REG having a declaration init (its content is lowered into the
      // reset branch / decl init by SplitInitialBlocks and ToED).
      def initialAssignsREG(pb: ProcessBlock)(using gs: MemberGetSet): Boolean =
        gs.designDB.getMembersOf(pb, MemberView.Flattened).exists {
          case DFNet.BAssignment(toVal, _) => toVal.departialDcl.exists(_._1.isReg)
          case _                           => false
        }
      def usesClk(owner: DFDomainOwner): Boolean =
        atOwner(owner) {
          domainOwnerMemberTable(owner).exists {
            // an initial block on its own does not imply a clock (a REG it may assign
            // already implies one via its Dcl)
            case pb: ProcessBlock if pb.isInRTDomain => !isInitialPB(pb)
            case dcl: DFVal.Dcl                      => dcl.isReg || dcl.isClkDcl
            case reg: DFVal.Alias.History            => true
            // a shared-variable write commits at the clock step's end, so it requires a clock
            // even in a domain with no registers of its own (the shared Dcl itself is
            // declared outside the domain)
            case net @ DFNet.Assignment(toVal, _) if net.isInRTDomain =>
              toVal.departialDcl.exists(_._1.modifier.isShared)
            case inst: DFDesignInst      => usesClkRst(inst.getDesignBlock).usesClk
            case DFVal.Func.Call(_, key) => usesClkRst(key.getDesignBlock).usesClk
            case _                       => false
          }
        } || reversedDependents.getOrElse(owner, Set()).exists(d => usesClkRst(d).usesClk) ||
          (owner eq topDB.top) && atOwner(owner)(isAlwaysAtTopClk(owner)) ||
          owner.hasClkAnnot
      def usesRst(owner: DFDomainOwner): Boolean =
        atOwner(owner) {
          domainOwnerMemberTable(owner).exists {
            case pb: ProcessBlock if pb.isInRTDomain =>
              if (isInitialPB(pb)) initialAssignsREG(pb) else true
            case dcl: DFVal.Dcl =>
              (dcl.isReg && dcl.hasNonBubbleInit) || dcl.isRstDcl
            case reg: DFVal.Alias.History => reg.hasNonBubbleInit
            case inst: DFDesignInst       => usesClkRst(inst.getDesignBlock).usesRst
            case DFVal.Func.Call(_, key)  => usesClkRst(key.getDesignBlock).usesRst
            case _                        => false
          }
        } || reversedDependents.getOrElse(owner, Set()).exists(d => usesClkRst(d).usesRst) ||
          (owner eq topDB.top) && atOwner(owner)(isAlwaysAtTopRst(owner)) ||
          owner.hasRstAnnot
      def relaxed(resolved: ClkRstTiming, atDomain: DFDomainOwner): ClkRstTiming =
        val (uClk, uRst) = usesClkRst(atDomain)
        val (clk, rst) = resolved
        (if (uClk) clk else None, if (uRst) rst else None)
      @tailrec def fillDomainMap(
          domains: List[DFDomainOwner],
          stack: List[DFDomainOwner],
          domainMap: mutable.Map[DFDomainOwner, ClkRstTiming]
      ): Unit =
        domains match
          case domain :: rest if domainMap.contains(domain) =>
            fillDomainMap(rest, stack, domainMap)
          case domain :: rest =>
            dependentRTDomainOwners.get(domain) match
              case Some(dependencyDomain) =>
                domainMap.get(dependencyDomain) match
                  case Some(dependencyResolved) =>
                    domainMap += domain -> relaxed(dependencyResolved, domain)
                    fillDomainMap(rest, stack, domainMap)
                  case None => fillDomainMap(rest, domain :: stack, domainMap)
              case _ =>
                val resolved = atOwner(domain)(resolvedClkRst(domain))
                domainMap += domain -> relaxed(resolved, domain)
                fillDomainMap(rest, stack, domainMap)
          case Nil if stack.nonEmpty => fillDomainMap(stack, Nil, domainMap)
          case _                     =>
      val domainMap = mutable.Map.empty[DFDomainOwner, ClkRstTiming]
      val rtDomainOwners: List[DFDomainOwner] =
        subDBs.view.values.flatMap { sub =>
          sub.domainOwnerMemberList.view.map { case (owner, _) => owner }
        }.filter { owner =>
          owner.domainType match
            case DomainType.RT => true
            case _             => false
        }.toList
      fillDomainMap(rtDomainOwners, Nil, domainMap)
      domainMap.toMap

  // Device-top clock-rate check, run on the root DB. A device-top RT domain
  // "uses a clock" exactly when the resolver produced a clock
  // (resolvedClkRstMap(owner)._1.isDefined), so the resolved-clock map drives
  // both the filter and the explicit rate. Per-owner
  // local reads (isDeviceTop, isTop position, getFullName, the clk timing
  // constraint) are routed through the owner's sub-DB getSet via `atOwner`.
  def domainClkRateCheck(): Unit =
    def atOwner[T](owner: DFDomainOwner)(f: MemberGetSet ?=> T): T =
      domainOwnerToSubDB(owner).atGetSet(f)
    val errors = collection.mutable.ArrayBuffer[String]()
    domainOwnerMemberList.view.map(_._1).foreach { domainOwner =>
      val resolvedClkOpt = resolvedClkRstMap.get(domainOwner).flatMap(_._1)
      if (
        resolvedClkOpt.isDefined &&
        atOwner(domainOwner)(domainOwner.getThisOrOwnerDesign.isDeviceTop)
      )
        def waitError(msg: String): Unit =
          val pos = atOwner(domainOwner) {
            if (domainOwner.isTop) domainOwner.asInstanceOf[DFDesignBlock].dclMeta.position
            else domainOwner.meta.position
          }
          errors += s"""|DFiant HDL domain clock rate error!
                        |Position:  ${pos}
                        |Hierarchy: ${atOwner(domainOwner)(domainOwner.getFullName)}
                        |Message:   $msg""".stripMargin
        val explicitRateOpt = resolvedClkOpt.flatMap(_.rate.toOption)
        val timingConstraintRateOpt =
          atOwner(domainOwner)(domainOwner.getTimingConstraintClkRateOpt)
        (explicitRateOpt, timingConstraintRateOpt) match
          case (Some(explicitRate), Some(timingConstraintRate)) =>
            if (explicitRate.to_freq.to_hz != timingConstraintRate.to_freq.to_hz)
              waitError(
                s"""|Mismatch between domain clock rate configuration ($explicitRate) and timing constraint rate ($timingConstraintRate).
                    |To fix, do one of the following:
                    |* Connect a different clock resource to the domain to match your configuration.
                    |* Explicitly set the clock rate configuration to $timingConstraintRate.
                    |* Remove the domain clock rate configuration and let it be derived from the timing constraint.""".stripMargin
              )
          case (Some(explicitRate), None) =>
            waitError(
              s"""|Missing clock rate timing constraint.
                  |To Fix:
                  |Connect a $explicitRate clock resource to the domain to match your configuration.""".stripMargin
            )
          case (None, None) =>
            waitError(
              s"""|Missing clock rate timing constraint.
                  |To Fix:
                  |Connect the wanted clock resource to the domain.
                  |(the domain will automatically derive the clock rate from the resource).""".stripMargin
            )
          case _ =>
        end match
      end if
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end domainClkRateCheck

  // Timed-wait divisibility check, run on the root DB. Iterates each
  // sub-DB's RT waits under that sub-DB's getSet (the root has no members of its
  // own); the clock rate comes from resolvedClkRstMap. The error hierarchy
  // uses fullNameViaInst so a wait in a nested design reports its full
  // instance path (plain getFullName on a sub-DB would be relative).
  def waitCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    subDBs.view.values.foreach { sub =>
      sub.atGetSet {
        for
          wait <- sub.members.collect { case w: Wait if w.isInRTDomain => w }
          trigger = wait.triggerRef.get
          if trigger.dfType == DFTime
        do
          def waitError(msg: String): Unit =
            errors += s"""|DFiant HDL wait error!
                          |Position:  ${wait.meta.position}
                          |Hierarchy: ${fullNameViaInst(wait.getOwnerDesign, sub)}
                          |Message:   $msg""".stripMargin
          val ownerDomain = wait.getOwnerDomain
          trigger.getConstData[TimeNumber].toOption match
            case Some(waitTime) =>
              resolvedClkRstMap.get(ownerDomain).flatMap(_._1).flatMap(_.rate.toOption) match
                case Some(rate) =>
                  val clockPeriodPs = rate.to_ps.value
                  val desc = rate match
                    case time: TimeNumber => s"period ${time}"
                    case freq: FreqNumber => s"frequency ${freq}"
                  val waitDurationPs = waitTime.to_ps.value
                  if (!(waitDurationPs / clockPeriodPs).isWhole)
                    waitError(
                      s"Wait duration ${waitTime} is not exactly divisible by the clock $desc."
                    )
                case _ =>
                  waitError(
                    s"Wait statement is missing an explicit clock configuration in its domain."
                  )
              end match
            case _ =>
              waitError(s"Wait duration is not constant.")
          end match
        end for
      }
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end waitCheck

  // ED procedural method call-site checks (see devdocs/methods.md), run once on the root.
  // A method that (transitively) contains a wait statement suspends its calling process,
  // which is only legal for processes without a sensitivity list (`process.forever`) or
  // `initial` blocks — VHDL hard-errors on a wait inside a procedure called from a process
  // with a sensitivity list, and the equivalent Verilog is semantically broken.
  // Nested calls (a waiting method called from another method body) are covered by the
  // outermost call site.
  def edMethodCallSiteCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    // memoized transitive wait detection per method design (keyed by its hierarchy key)
    val waitingCache = collection.mutable.Map.empty[DFOwner.Ref, Boolean]
    def methodSubDBOpt(designKey: StaticRef): Option[DB] = subDBs.get(designKey)
    def isWaiting(methodDB: DB): Boolean =
      val key = methodDB.top.ownerRef
      waitingCache.get(key) match
        case Some(cached) => cached
        case None         =>
          val waiting = methodDB.members.exists {
            case _: Wait                  => true
            case DFVal.Func.Call(_, dKey) =>
              methodSubDBOpt(dKey).exists(sub => sub.top.isEDMethod && isWaiting(sub))
            case _ => false
          }
          waitingCache(key) = waiting
          waiting
    subDBs.view.values.foreach { sub =>
      // a call inside another method body is checked at its outermost call site
      if (!sub.top.isEDMethod)
        sub.atGetSet {
          sub.members.foreach {
            case DFVal.Func.Call(call, dKey) =>
              methodSubDBOpt(dKey) match
                case Some(methodDB) if methodDB.top.isEDMethod && isWaiting(methodDB) =>
                  def callError(msg: String): Unit =
                    errors += s"""|DFiant HDL ED method error!
                                  |Position:  ${call.meta.position}
                                  |Hierarchy: ${fullNameViaInst(call.getOwnerDesign, sub)}
                                  |Message:   $msg""".stripMargin
                  @tailrec def owningProcess(m: DFMember): Option[ProcessBlock] =
                    m.getOwner match
                      case pb: ProcessBlock => Some(pb)
                      case _: DFDesignBlock => None
                      case o                => owningProcess(o)
                  owningProcess(call) match
                    case Some(pb) =>
                      pb.sensitivity match
                        case ProcessBlock.Sensitivity.List(Nil) |
                            ProcessBlock.Sensitivity.Initial => // ok — suspension-capable
                        case _ =>
                          callError(
                            "A wait-containing ED method can only be called inside a process without a sensitivity list (`process.forever`) or an `initial` block."
                          )
                    case None =>
                      callError(
                        "A wait-containing ED method can only be called inside a process without a sensitivity list (`process.forever`) or an `initial` block."
                      )
                case _ => // not a waiting ED method call
            case _ =>
          }
        }
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end edMethodCallSiteCheck

  // `initial` block content and conflict checks, run per design (initial blocks may
  // only reference values of their own design, so no cross-design pass is needed):
  //   * Both domains: only blocking assignments — no non-blocking assignments,
  //     connections, waits, gotos, or nested owners (processes/steps/forks/domains/designs).
  //   * RT domain: assignments must have a constant RHS; for-loops and conditionals with
  //     constant guards/selectors as control flow; no while loops, text output, or
  //     declarations (except loop iterators).
  //   * ED domain: also allows non-constant conditionals, while loops, text output, and
  //     local declarations.
  //   * Conflict rules: a declaration may be assigned by at most one `initial` block, and a
  //     declaration `init` is mutually exclusive with an `initial` block assignment.
  def initialCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    def memberError(member: DFMember, msg: String): Unit =
      errors += s"""|DFiant HDL initial block error!
                    |Position:  ${member.meta.position}
                    |Hierarchy: ${member.getOwnerDesign.getFullName}
                    |Message:   $msg""".stripMargin
    val initialPBs = members.collect {
      case pb @ ProcessBlock(sensitivity = ProcessBlock.Sensitivity.Initial) => pb
    }
    // per-block content checks, while collecting the externally-declared assigned dcls
    // (ordered and deduped per block) for the cross-block conflict checks
    val assignedDclsPerPB: List[(ProcessBlock, List[(DFVal.Dcl, DFNet)])] = initialPBs.map { pb =>
      val isRT = pb.isInRTDomain
      if (pb.isInDFDomain)
        memberError(pb, "An `initial` block is not supported under dataflow (DF) domains.")
      val assigned = mutable.LinkedHashMap.empty[DFVal.Dcl, DFNet]
      getMembersOf(pb, MemberView.Flattened).foreach {
        case net @ DFNet.BAssignment(toVal, fromVal) =>
          if (isRT && !fromVal.isConst)
            memberError(
              net,
              "An `initial` block under a register-transfer (RT) domain may only assign constant values."
            )
          toVal.departialDcl.foreach { (dcl, _) =>
            // local declarations (inside the initial block itself) are exempt from
            // the cross-block conflict rules
            if (!dcl.isInsideOwner(pb) && !assigned.contains(dcl)) assigned += dcl -> net
          }
        case net @ DFNet.NBAssignment(_, _) =>
          memberError(net, "Non-blocking assignments are not allowed inside an `initial` block.")
        case net: DFNet =>
          memberError(net, "Connections are not allowed inside an `initial` block.")
        case wait: Wait =>
          memberError(wait, "Wait statements are not allowed inside an `initial` block.")
        case goto: Goto =>
          memberError(goto, "Step transitions are not allowed inside an `initial` block.")
        case textOut: TextOut if isRT =>
          memberError(
            textOut,
            "Text output statements are not allowed inside an `initial` block under a register-transfer (RT) domain."
          )
        case mh: DFConditional.DFMatchHeader if isRT && !mh.selectorRef.get.isConst =>
          memberError(
            mh,
            "A `match` selector inside an `initial` block under a register-transfer (RT) domain must be a constant."
          )
        case cb: DFConditional.Block if isRT =>
          cb.guardRef.get match
            case guard: DFVal if !guard.isConst =>
              memberError(
                cb,
                "A conditional guard inside an `initial` block under a register-transfer (RT) domain must be a constant."
              )
            case _ => // no guard (else branch / catch-all case) or a constant guard — ok
        case wb: DFLoop.DFWhileBlock if isRT =>
          memberError(
            wb,
            "While loops are not allowed inside an `initial` block under a register-transfer (RT) domain (only for-loops are)."
          )
        case dcl: DFVal.Dcl if isRT && !dcl.isIterator =>
          memberError(
            dcl,
            "Declarations are not allowed inside an `initial` block under a register-transfer (RT) domain."
          )
        case owner @ (_: ProcessBlock | _: StepBlock | _: ForkBlock | _: DomainBlock |
            _: DFDesignBlock | _: DFDesignInst) =>
          memberError(owner, "This construct is not allowed inside an `initial` block.")
        case _ => // ok
      }
      pb -> assigned.toList
    }
    // cross-block conflict checks
    val seenDcls = mutable.Set.empty[DFVal.Dcl]
    assignedDclsPerPB.foreach { (pb, assigned) =>
      assigned.foreach { (dcl, net) =>
        if (dcl.hasNonBubbleInit)
          memberError(
            net,
            s"The declaration `${dcl.getName}` has an `init` value and is also assigned inside an `initial` block. These are mutually exclusive."
          )
        if (seenDcls.contains(dcl))
          memberError(
            net,
            s"The declaration `${dcl.getName}` is assigned inside more than one `initial` block."
          )
        else seenDcls += dcl
      }
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end initialCheck

  // A conditional expression (a header with a non-Unit `dfType`) yields a value, so each of its
  // branches contributes only that value. Whether a branch can additionally hold a *named* value
  // depends on the scope the expression sits in:
  //
  //   * In a sequential scope (an RT or DF domain, or an ED process), the branch lowers to a
  //     procedural block, so the named value becomes a plain blocking assignment inside it. Legal,
  //     and relied upon by existing designs.
  //   * In a concurrent scope (directly in an ED domain body), the branch is not a block at all.
  //     The expression is later wrapped into a `process(all)`, but the named value's drive was
  //     already fixed as a concurrent connection, which then prints as an `assign` inside an
  //     `always_comb`. There is no scope for it to land in, so it is rejected here.
  //
  // The concurrent test is `isInEDDomain && !isInProcess`, the same predicate `ExplicitNamedVars`
  // uses to choose a connection over an assignment. The two must stay in agreement: whatever that
  // stage would drive by connection is exactly what has nowhere to live inside a branch. A named
  // conditional header is exempt for the same reason: `ExplicitNamedVars` drives it through
  // `patchChains`, which assigns per branch, so it never becomes a connection and nests fine.
  def condExprNamedValCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    // The nearest enclosing conditional block, if that block belongs to a conditional
    // expression chain. A conditional chain never crosses a domain boundary, so the walk
    // stops there (which also keeps it clear of the top's empty owner ref).
    @tailrec def condExprBlockOf(member: DFMember): Option[DFConditional.Block] =
      member.getOwner match
        case cb: DFConditional.Block =>
          if (cb.getHeaderCB.dfType == DFUnit) condExprBlockOf(cb) else Some(cb)
        case _: DFDomainOwner => None
        case owner            => condExprBlockOf(owner)
    members.foreach {
      case _: DFConditional.Header => // driven per branch, never by connection
      case dfVal: DFVal
          if !dfVal.isAnonymous && !dfVal.isGlobal &&
            dfVal.isInEDDomain && !dfVal.isInProcess =>
        condExprBlockOf(dfVal).foreach { _ =>
          errors +=
            s"""|DFiant HDL conditional expression error!
                |Position:  ${dfVal.meta.position}
                |Hierarchy: ${dfVal.getOwnerDesign.getFullName}
                |Message:   Found the named value `${dfVal.getName}` inside a conditional expression branch.
                |An event-driven (ED) domain body is a concurrent scope, so a conditional expression
                |branch in it is not a block and cannot hold a named value declaration.
                |To Fix:
                |Move the declaration before the conditional expression, place the conditional
                |expression inside a `process`, or turn it into a conditional statement that assigns
                |or connects its result.""".stripMargin
        }
      case _ =>
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end condExprNamedValCheck

  // A DECLARATION made inside a statement block exists only inside it, so nothing outside may
  // read it. Elaboration breaks this whenever a Scala `var` is reassigned inside a DFHDL loop or
  // conditional block: the reassignment binds the Scala name to a value built under that block,
  // and the next read of the `var`, after the block, reaches through to what that value reads,
  // which includes the block's own iterator. Nothing downstream notices, because the expression
  // is anonymous and therefore printed at its reader, so the backend emits it outside the loop,
  // where the iterator is not in scope (issue #433).
  //
  // Which is why an anonymous value is TRANSPARENT here: it has no place of its own, so the
  // question is not where it sits but what it reads, asked from the position of whoever reads it.
  // A declaration or a named value does have a place, and that place has to enclose the reader.
  //
  // Only statement blocks bind: a `DFDomainOwner` (a design, a domain, an interface) declares
  // members that are public by construction, since a domain's port is read by the design that
  // owns it.
  def blockScopeCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    // `ownerRef.get` rather than `getOwner`, which throws on a member with no owner at all: a
    // global, the top, and the `Goto` step placeholders all have an empty owner ref
    @tailrec def isInsideOrIs(member: DFMember, block: DFBlock): Boolean =
      if (member == block) true
      else
        member.ownerRef.get match
          case owner if owner == block => true
          // stop at the enclosing design/domain, and at anything with no owning block
          case _: DFDomainOwner => false
          case owner: DFBlock   => isInsideOrIs(owner, block)
          case _                => false
    // A `for` iterator is declared in the loop header, so the loop is its scope. Elaboration owns
    // the declaration from the ENCLOSING block instead (it is created just before the loop), so
    // the loop has to be looked up rather than read off the declaration's owner. Once
    // `SimplifyRTOps` rewrites the loop into a `while`, the iterator becomes an ordinary variable
    // of the enclosing scope and this map no longer holds it, which is correct.
    val iteratorScope: Map[DFMember, DFBlock] = members.view.collect {
      case forBlock: DFLoop.DFForBlock => forBlock.iteratorRef.get -> forBlock
    }.toMap
    def blockKind(block: DFBlock): String = block match
      case _: DFLoop.DFForBlock   => "`for` loop"
      case _: DFLoop.DFWhileBlock => "`while` loop"
      case _: DFConditional.Block => "conditional block"
      case _: ProcessBlock        => "process"
      case _: StepBlock           => "step"
      case _: ForkBlock           => "fork block"
      case _                      => "block"
    // `visited` also keeps one reader from reporting the same declaration once per path to it:
    // `x(k * 11 + 10, k * 11)` reaches the iterator through two independent index expressions
    def checkReads(reader: DFMember, target: DFMember, visited: mutable.Set[DFMember]): Unit =
      if (visited.add(target)) target match
        case dfVal: DFVal if dfVal.isAnonymous =>
          dfVal.getRefs.foreach(r => checkReads(reader, r.get, visited))
        case named: DFMember.Named =>
          iteratorScope.getOrElse(target, target.ownerRef.get) match
            case block: DFBlock
                if !block.isInstanceOf[DFDomainOwner] && !isInsideOrIs(reader, block) =>
              errors +=
                s"""|DFiant HDL scope error!
                    |Position:  ${reader.meta.position}
                    |Hierarchy: ${reader.getOwnerDesign.getFullName}
                    |Message:   Found a read of `${named.getName}`, declared inside the ${blockKind(
                     block
                   )} at
                    |${block.meta.position}, from outside that block.
                    |A declaration made inside a block exists only within it. This usually comes from
                    |a Scala `var` reassigned inside the block: the reassignment binds the Scala name
                    |to a value built under the block, so reading the `var` afterwards reaches the
                    |block's own declarations from outside.
                    |To Fix:
                    |Declare a DFHDL variable before the block, assign it with `:=` inside the block,
                    |and read the variable afterwards.""".stripMargin
            case _ =>
        case _ =>
    members.foreach {
      // an anonymous value is checked from whoever reads it, not from where it sits
      case dfVal: DFVal if dfVal.isAnonymous =>
      case member                            =>
        val visited = mutable.Set.empty[DFMember]
        member.getRefs.foreach(ref => checkReads(member, ref.get, visited))
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end blockScopeCheck

  // Shared-variable access rules that only a clocked context can honor (`VAR.SHARED` models a
  // multi-ported RAM; the write-operator discipline lives in `DFVarOps.SharedNBAssignOnly` and
  // `SanityCheck.sharedAssignCheck`):
  //   * Rule 1: no writes inside a combinational `process(all)`. Such a write has no faithful
  //     rendering (Verilog executes a non-blocking assignment in `always_comb` as blocking, and
  //     a level-sensitive RAM write contradicts the end-of-step commit semantics that
  //     DFacsimile and the clocked-process backends implement).
  //   * Rule 2: under an ED domain, a shared variable may only be accessed inside a process. A
  //     concurrent access (connection, expression, or sensitivity entry) has no faithful VHDL
  //     rendering: a shared variable is not a signal, so its change never re-triggers a
  //     concurrent statement, which silently freezes at its time-zero value. RT/DF-domain
  //     accesses are concurrent by nature and are exempt; the compilation stages lower them
  //     into clocked processes.
  //   * Rule 3: an RT-domain shared-variable write must be able to lower into the clocked
  //     process. A plain read of a later-reassigned wire is fine (`NameVarVersions` captures
  //     it into a version variable), but no capture can fix a guard-path hazard (a chain
  //     guard reading a later-reassigned wire) or a `.din` read (its shadow variable only
  //     exists in the combinational process).
  //   * Rule 4: a loop containing an RT-domain shared-variable write moves whole into the
  //     clocked process, so all its content must be sequential-sink writes with settled
  //     reads; otherwise the loop must be split.
  def sharedVarCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    def memberError(member: DFMember, msg: String): Unit =
      errors += s"""|DFiant HDL shared variable error!
                    |Position:  ${member.meta.position}
                    |Hierarchy: ${member.getOwnerDesign.getFullName}
                    |Message:   $msg""".stripMargin
    @tailrec def ownerProcessOpt(member: DFMember): Option[ProcessBlock] =
      member.ownerRef.get match
        case pb: ProcessBlock => Some(pb)
        case _: DFDomainOwner => None
        case owner: DFBlock   => ownerProcessOpt(owner)
        case _                => None
    members.foreach { m =>
      // Rule 1: a write to a shared variable inside a `process(all)`
      m match
        case net @ DFNet.Assignment(toVal, _) =>
          toVal.departialDcl match
            case Some((dcl, _)) if dcl.modifier.isShared =>
              ownerProcessOpt(net) match
                case Some(ProcessBlock(sensitivity = ProcessBlock.Sensitivity.All)) =>
                  memberError(
                    net,
                    """|A shared variable cannot be written inside a combinational process (`process(all)`).
                       |A shared-variable write commits at the end of a clock step, so it must reside inside a clocked process.""".stripMargin
                  )
                case _ =>
            case _ =>
        case _ =>
      // Rule 2: a concurrent (outside-process) access under an ED domain. The shared-ref test
      // comes first: it is the only part safe to evaluate on global members (the domain walk
      // throws on a member with no owner), and a global can never reference a design-local
      // declaration.
      val refsShared = m.getRefs.exists { ref =>
        ref.get match
          case dcl: DFVal.Dcl => dcl.modifier.isShared
          case _              => false
      }
      if (refsShared && m.isInEDDomain && !m.isInProcess)
        memberError(
          m,
          """|A shared variable can only be accessed inside a process under an event-driven (ED) domain.
             |A concurrent access has no faithful VHDL rendering: a shared variable is not a signal, so its change never re-triggers a concurrent statement.
             |To Fix: move the access into a process, or use a regular variable instead.""".stripMargin
        )
    }
    // Rules 3 and 4: RT-domain shared-variable writes must be movable into the clocked process
    domainOwnerMemberList.foreach { (owner, members) =>
      def sharedWriteTarget(net: DFNet): Option[DFVal.Dcl] =
        net match
          case DFNet.Assignment(toVal, _) if !net.isInProcess =>
            toVal.departialDcl.collect { case (dcl, _) if dcl.modifier.isShared => dcl }
          case _ => None
      val rtBlockOwner = owner match
        case block: (DFDomainOwner & DFBlock) =>
          block.domainType match
            case DomainType.RT => Some(block)
            case _             => None
      val analysisOpt = rtBlockOwner
        .filter(_ =>
          members.exists {
            case net: DFNet => sharedWriteTarget(net).nonEmpty
            case _          => false
          }
        )
        .map(new dfhdl.compiler.analysis.RTDomainAnalysis(_, members))
      analysisOpt.foreach { analysis =>
        members.foreach {
          case net: DFNet =>
            sharedWriteTarget(net).foreach { dcl =>
              analysis.loopRootOf(net) match
                case Some(loop) =>
                  if (!analysis.loopSeqMovable(loop, analysis.dinReadREGs))
                    memberError(
                      net,
                      """|A shared-variable write inside a loop requires the whole loop to lower into the clocked process, but the loop mixes combinational content or reads values that are reassigned later in the domain body.
                         |To Fix: split the loop so that the shared-variable write is in a purely-sequential loop.""".stripMargin
                    )
                case None =>
                  if (analysis.stmtUncapturable(net, Some(dcl)))
                    memberError(
                      net,
                      """|A shared-variable write must lower into the clocked process, but its guard path reads a value that is reassigned later in the domain body, or it reads a `.din` value.
                         |To Fix: restructure so that nothing the write's guards depend on is reassigned after the write, or hoist the guard condition computation after its operands' final assignments.""".stripMargin
                    )
            }
          case _ =>
        }
      }
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end sharedVarCheck

  // Circular-derived-domain check, run on the root DB. DFS over
  // `dependentRTDomainOwners`; the cycle error names each
  // owner via `fullNameViaInst` routed through that owner's sub-DB.
  def circularDerivedDomainsCheck(): Unit =
    @tailrec def dfs(
        node: DFDomainOwner,
        visited: Set[DFDomainOwner],
        stack: Set[DFDomainOwner]
    ): Unit =
      if (stack.contains(node))
        throw new IllegalArgumentException(
          s"""|Circular derived RT configuration detected. Involved in the cycle:
              |${stack.map(o => fullNameViaInst(o, domainOwnerToSubDB(o))).mkString("\n")}
              |""".stripMargin
        )
      if (!visited.contains(node))
        dependentRTDomainOwners.get(node) match
          case Some(dependentNode) => dfs(dependentNode, visited + node, stack + node)
          case None                =>
    end dfs
    for (node <- dependentRTDomainOwners.keys)
      dfs(node, Set.empty, Set.empty)
  end circularDerivedDomainsCheck

  def nameCheck(): Unit =
    // We use a Set since meta programming is usually the cause and can result in
    // multiple anonymous members with the same position. The top can be anonymous.
    // DFDesignBlocks are always anonymous post-Phase 2 (instance name lives on
    // the associated DFDesignInst) so they're not checked here; DFDesignInsts
    // are checked instead.
    val anonErrorMemberPositions: Set[Position] = localsForOML.view.collect {
      case dcl: DFVal.Dcl if dcl.isAnonymous => dcl
      // design instantiations must carry an instance name (def-mode insts are
      // allowed to be anonymous)
      case inst: DFDesignInst if inst.isAnonymous && inst.getDesignBlock.instMode != InstMode.Def =>
        inst
      // domains cannot be anonymous
      case domain: DomainBlock if domain.isAnonymous => domain
    }.map(_.meta.position).toSet
    if (anonErrorMemberPositions.nonEmpty)
      throw new IllegalArgumentException(
        s"""DFiant HDL name errors!
           |Unable to determine names for the members declared at the following positions:
           |${anonErrorMemberPositions.mkString("\n")}
           |
           |Explanation:
           |This can happen when utilizing the meta programming power of Scala in a way that
           |DFHDL cannot infer the actual name of the member.
           |
           |Resolution:
           |To resolve this issue use `setName` when declaring the member.
           |
           |Example 1:
           |```
           |  // Scala Vector holding 4 DFHDL ports
           |  val x_vec = Vector.fill(4)(UInt(8) <> IN setName "x_vec")
           |```
           |In this example all the ports will be named "x_vec", and DFHDL will enumerate
           |them automatically to "x_vec_0", "x_vec_1", etc.
           |
           |Example 2:
           |If you wish to give the ports an explicit unique name, you can just use the power
           |of Scala, as in the following example:
           |```
           |  val x_vec = Vector.tabulate(4)(i => UInt(8) <> IN setName s"x_vec_{i + 10}")
           |```
           |This would yield the same ports, but named "x_vec_10", "x_vec_11", etc.
           |""".stripMargin
      )
  end nameCheck

  // checks for direct references across designs
  def directRefCheck(): Unit =
    import DFVal.PortByNameSelect
    val problemReferences: List[(DFMember, DFMember)] =
      membersNoGlobals.view.drop(1).flatMap {
        case _: PortByNameSelect => None
        case m                   =>
          m.getRefs.view.map(_.get).flatMap {
            // global values are ok to be referenced
            case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => None
            // skip empty
            case empty: DFMember.Empty => None
            // port referencing is done by name and validated via the
            // PortByNameSelect's design instance, which lives in m's design
            // scope.
            case pbns: PortByNameSelect =>
              val designInst = pbns.getDesignInst
              if (designInst.isOutsideOwner(m.getOwnerDesign)) Some(pbns)
              else None
            // design referenced by its member (e.g. via @timing.related)
            case refMember: DFDesignBlock =>
              if (m.isMemberOf(refMember)) None
              else Some(refMember)
            // the rest must be in the same design
            case refMember if !refMember.isSameOwnerDesignAs(m) => Some(refMember)
            case _                                              => None
          }.map(m -> _)
      }.toList
    val errorMessages = problemReferences.map { (from, to) =>
      val toName = to match
        case named: DFMember.Named => s"`${named.getName}` "
        case _                     => ""
      s"""|The DFHDL code at:
          |    Position:  ${from.meta.position}
          |    Hierarchy: ${from.getOwnerDesign.getFullName}
          |    Structure: ${from}
          |is directly referencing the member ${toName}at:
          |    Position:  ${to.meta.position}
          |    Hierarchy: ${to.getOwnerDesign.getFullName}""".stripMargin
    }
    if (errorMessages.nonEmpty)
      throw new IllegalArgumentException(
        s"""DFiant HDL direct reference errors!
           |${errorMessages.mkString("\n")}
           |To Fix:
           |Use ports and connections to transfer values across design hierarchies.
           |""".stripMargin
      )
  end directRefCheck

  // Device-top port location-constraint check, run on the root DB. Only the
  // device-top design is examined; its members are resolved under that design's
  // sub-DB getSet (device top == toptop, so getFullName is the full path).
  def portLocationCheck(): Unit =
    val errors = mutable.ListBuffer.empty[String]
    val locationCollisions = mutable.ListBuffer.empty[String]
    designMemberList.foreach {
      case (design, members) if design.isDeviceTop =>
        domainOwnerToSubDB(design).atGetSet {
          val locationMap = mutable.Map.empty[String, String] // loc -> portName(idx)
          // the root-aware designMemberList already includes the design block as
          // the head of its member list, so iterate `members` directly (the flat
          // version prepended `design ::` to a children-only list).
          members.foreach {
            case designInstance: DFDesignBlock if designInstance != design =>
            case domainOwner: DFDomainOwner                                =>
              domainOwner.domainType match
                case DomainType.RT =>
                  var foundLoc = false
                  domainOwner.getDomainClkConstraintsView.foreach {
                    case constraints.IO(loc = loc: String) =>
                      locationMap.get(loc).foreach { prevPort =>
                        locationCollisions +=
                          s"${prevPort} and ${domainOwner.getFullName} are both assigned to location `${loc}`"
                      }
                      locationMap += loc -> domainOwner.getFullName
                      foundLoc = true
                    case _ =>
                  }
                  val clkIsVar = domainOwnerMemberTable(domainOwner).view.collectFirst {
                    case dcl: DFVal.Dcl if dcl.isClkDcl => dcl.isVar
                  }.getOrElse(false)
                  if (!foundLoc && !clkIsVar)
                    errors += s"${domainOwner.getFullName} is missing a clock location constraint"
                case _ =>
              end match
            case clkPort: DFVal.Dcl if clkPort.isPortIn && clkPort.isClkDcl =>
            case port: DFVal.Dcl if port.isPort                             =>
              val bitSet = port.widthIntOpt match
                case Some(width) => mutable.BitSet((0 until width)*)
                case None        => mutable.BitSet.empty
              port.meta.annotations.foreach {
                case constraints.IO(bitIdx = None, loc = loc: String) =>
                  bitSet.clear()
                  locationMap.get(loc).foreach { prevPort =>
                    locationCollisions +=
                      s"${prevPort} and ${port.getFullName} are both assigned to location `${loc}`"
                  }
                  locationMap += loc -> port.getFullName
                  if (port.widthIntOpt.get != 1)
                    locationCollisions +=
                      s"${port.getFullName} has mutliple bits assigned to location `${loc}`"
                case constraints.IO(bitIdx = bitIdx: Int, loc = loc: String) =>
                  locationMap.get(loc).foreach { prevPort =>
                    locationCollisions +=
                      s"${prevPort} and ${port.getFullName}(${bitIdx}) are both assigned to location `${loc}`"
                  }
                  locationMap += loc -> s"${port.getFullName}(${bitIdx})"
                  bitSet -= bitIdx
                case _ =>
              }
              if (bitSet.nonEmpty)
                if (port.widthIntOpt.get == 1)
                  errors += s"${port.getFullName}"
                else
                  errors += s"${port.getFullName} with bits ${bitSet.mkString(", ")}"
            case _ =>
          }
        }
      case _ =>
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(
        s"""|The following top device design ports or domains are missing location constraints:
            |  ${errors.mkString("\n  ")}
            |To Fix:
            |Add a location constraint to the ports by connecting them to a located resource or
            |by using the `@io` constraint.
            |""".stripMargin
      )
    if (locationCollisions.nonEmpty)
      throw new IllegalArgumentException(
        s"""|The following location constraints have collisions:
            |  ${locationCollisions.mkString("\n  ")}
            |To Fix:
            |Ensure each location is used by a single port bit.
            |""".stripMargin
      )
  end portLocationCheck

  // Device-top port resource-direction check, run on the root DB. Members
  // resolved under the device-top sub-DB getSet.
  def portResourceDirCheck(): Unit =
    import DFVal.Modifier.Dir
    val errors = mutable.ListBuffer.empty[String]
    designMemberList.foreach {
      case (design, members) if design.isDeviceTop =>
        domainOwnerToSubDB(design).atGetSet {
          members.foreach {
            case port: DFVal.Dcl if port.isPort =>
              port.meta.annotations.foreach {
                case constraints.IO(dir = dir: Dir) =>
                  (dir, port.modifier.dir) match
                    case (Dir.IN, Dir.OUT) | (Dir.OUT, Dir.IN) =>
                      errors +=
                        s"${port.getFullName} direction (${port.modifier.dir}) has a resource direction ($dir) mismatch."
                    case _ =>
                case _ =>
              }
            case _ =>
          }
        }
      case _ =>
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(
        s"""|The following top device design ports have resource direction mismatches:
            |  ${errors.mkString("\n  ")}
            |To Fix:
            |Make sure you connect the resource to the port with the correct direction.
            |""".stripMargin
      )
  end portResourceDirCheck

  // Uniform entry point, representation-aware:
  //   - hierarchical root: run each sub-DB's per-design checks, then the
  //     cross-design root checks once on the root;
  //   - in-hierarchy sub-DB: run only its per-design checks.
  // Callers always invoke this on the root.
  lazy val check: Unit =
    if (isRoot)
      subDBs.view.values.foreach(_.subDBCheck)
      rootDBCheck
    else subDBCheck

  // Per-design structural checks: each validates a single design's own members
  // and references in isolation. Run on each sub-DB (and on the flat DB, on the
  // whole design).
  lazy val subDBCheck: Unit =
    nameCheck()
    connectionTable // causes connectivity checks
    directRefCheck()
    initialCheck()
    condExprNamedValCheck()
    blockScopeCheck()
    sharedVarCheck()

  // Whole-tree checks, run once on the root: the cross-design connectivity /
  // RT-domain / device-top checks, via the `*` clones that navigate the
  // sub-DB tree.
  private lazy val rootDBCheck: Unit =
    magnetConnectionMap // causes magnet connectivity checks
    checkDanglingPorts()
    circularDerivedDomainsCheck()
    domainClkRateCheck()
    waitCheck()
    edMethodCallSiteCheck()
    portLocationCheck()
    portResourceDirCheck()

  // There can only be a single connection to a value in a given range
  // (multiple assignments are possible)
  lazy val connectionTable: ConnectToMap =
    val flatNets = members.flatMap {
      case net: DFNet => FlatNet(net)
      case _          => Nil
    }
    getConnToMap(flatNets, Nil, ConnectToMap.empty, Nil).removeAssignments

  //                                    From       Via
  lazy val connectionTableInverted: Map[DFVal, Set[DFNet]] =
    members.view
      .collect { case n @ DFNet.Connection(_: DFVal, fromVal: DFVal, _) => (fromVal, n) }
      .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  //                              To       From
  lazy val assignmentsTable: Map[DFVal, Set[DFVal]] =
    members.foldLeft(Map.empty[DFVal, Set[DFVal]]) {
      case (at, DFNet.Assignment(toVal, fromVal)) =>
        at + (toVal -> (at.getOrElse(toVal, Set()) + fromVal))
      case (at, _) => at
    }

  //                                     From       To
  lazy val assignmentsTableInverted: Map[DFVal, Set[DFVal]] =
    members.foldLeft(Map.empty[DFVal, Set[DFVal]]) {
      case (at, DFNet.Assignment(toVal, fromVal)) =>
        at + (fromVal -> (at.getOrElse(fromVal, Set()) + toVal))
      case (at, _) => at
    }

  // ~~~ sub-design cache support ~~~

  // The return DFType of a loaded method sub-DB: the type of its RETURN output port, or
  // DFUnit when the def has a Unit return (no return port). The method harness needs this
  // on a cache hit, before any body exists. The return port is the OUT port driven by a
  // CONNECTION (the result wiring, mirroring `methodReturnPort`), NOT just any output-class
  // port: an ED procedure's `<> OUT`/`<> OUT.NB` ARGUMENT is an output-class port too, but the
  // body drives it by ASSIGNMENT, so it must not be mistaken for the return of a Unit procedure.
  def subDesignRetDFType: DFType =
    import topDB.getSet
    topDB.members.collectFirst {
      case DFNet.Connection(toVal = p: DFVal.Dcl) if p.isPortOut && !p.isPhantom => p.dfType
    }.getOrElse(DFUnit)

  // Collapses a new-style (option-a) DB back into a flat old-style DB.
  // Non-DFDesignBlock members dedup by OBJECT identity (globals are shared by
  // identity across every DB that references them in Phase 1 / early Phase 2).
  // DFDesignBlocks dedup by ownerRef because a stage that patches a sub-DB's
  // own designBlock produces a new instance in that sub-DB only — and any
  // ref pointing at the stale parent copy must retarget to the sub-DB's
  // patched version.
  // Nested DFDesignBlocks no longer live in their parent sub-DB's `members`,
  // so descent into a child sub-DB is triggered by the FIRST DFDesignInst
  // pointing at it (mirroring the original elaboration order, in which the
  // nested block is added during the first instance's elaboration and so
  // appears just before that inst in the flat DB).

  lazy val newToOld: DB =
    if (subDBs.isEmpty) this
    else
      // Under B-pure, root has empty `members` and empty `refTable`; all design
      // content lives in sub-DBs. `allDBs` only needs the sub-DBs.
      val allDBs: List[DB] = subDBs.values.toList
      // canonical DFDesignBlock per ownerRef: the one in its own sub-DB's
      // `members` (the sub-DB owns patches against its header).
      val canonicalDesign = mutable.Map.empty[DFOwner.Ref, DFDesignBlock]
      subDBs.foreach { (key, subDB) =>
        subDB.members.collectFirst {
          case d: DFDesignBlock if StaticRef(d.ownerRef) == key => d
        }.foreach(canonicalDesign(key.asRef) = _)
      }
      def canonicalize(m: DFMember): DFMember = m match
        case d: DFDesignBlock => canonicalDesign.getOrElse(d.ownerRef, d)
        case _                => m
      // inst → canonical target DFDesignBlock. Resolved through each sub-DB's
      // own refTable because the root DB's refTable does not necessarily cover
      // refs that originate in sub-DB members.
      val instToDesign = mutable.Map.empty[DFDesignInst, DFDesignBlock]
      // method call -> canonical target DFDesignBlock (keyed at the member level,
      // matching `emit`'s scrutinee)
      val callToDesign = mutable.Map.empty[DFMember, DFDesignBlock]
      allDBs.foreach { db =>
        db.members.foreach {
          case inst: DFDesignInst =>
            // Resolve inst -> child block structurally: `designRef` is unified with
            // the child block's `ownerRef` (the `subDBs` key) and is not present in
            // the parent sub-DB's refTable. `getDesignBlock` handles both the
            // unified form and the pre-unification distinct form.
            instToDesign(inst) =
              canonicalize(inst.getDesignBlock(using db.getSet)).asInstanceOf[DFDesignBlock]
          case DFVal.Func.Call(call, key) =>
            callToDesign(call) =
              canonicalize(key.getDesignBlock(using db.getSet)).asInstanceOf[DFDesignBlock]
          case _ =>
        }
      }
      val flat = mutable.ListBuffer.empty[DFMember]
      val seen = mutable.Set.empty[DFMember]
      // The real top design, and whether it is still waiting to be emitted. A global-scope
      // call (a static function computing a global value) precedes the top design in the
      // member list, and emitting it pulls its target def block in with it; see the deferral
      // in the call case below.
      val realTopBlock: Option[DFMember] = topDB.members.collectFirst {
        case d: DFDesignBlock if d.instMode != DFDesignBlock.InstMode.Def => canonicalize(d)
      }
      def topPending: Boolean = realTopBlock.exists(rt => !seen.contains(rt))
      val deferredDefs = mutable.ListBuffer.empty[DFDesignBlock]
      def emit(ms: List[DFMember]): Unit =
        ms.foreach { m =>
          val c = canonicalize(m)
          c match
            case inst: DFDesignInst =>
              // Emit the target design block (and its sub-DB body) before the
              // inst, on the first inst that references it. Subsequent insts
              // for the same block just emit themselves.
              instToDesign.get(inst).foreach { targetBlock =>
                if (!seen.contains(targetBlock))
                  seen += targetBlock
                  flat += targetBlock
                  subDBs.get(targetBlock.ownerRef).foreach(sub => emit(sub.members))
              }
              if (!seen.contains(c))
                seen += c
                flat += c
            // a method call triggers the same first-reference emission of its
            // target def design's body
            case call if callToDesign.contains(call) =>
              callToDesign.get(call).foreach { targetBlock =>
                if (!seen.contains(targetBlock))
                  // A GLOBAL-scope call sits before the top design in the member list, so
                  // emitting its target def block here would place that block AHEAD of the
                  // top. The flat form's `top` is the first non-global member, and a design
                  // block is NOT filtered out as a global (only `DFVal.CanBeGlobal` is), so
                  // the def block would masquerade as the top design, taking over the top
                  // name, the emitted defs-header name and the output folder. Defer it until
                  // the top design is out; being ownerless it is valid anywhere in the list.
                  if (topPending) deferredDefs += targetBlock
                  else
                    seen += targetBlock
                    flat += targetBlock
                    subDBs.get(targetBlock.ownerRef).foreach(sub => emit(sub.members))
              }
              if (!seen.contains(c))
                seen += c
                flat += c
            case _ =>
              if (!seen.contains(c))
                seen += c
                flat += c
                c match
                  case d: DFDesignBlock if subDBs.contains(d.ownerRef) =>
                    emit(subDBs(d.ownerRef).members)
                  case _ =>
          end match
        }
      // B-pure: root has empty `members`. Start emission from the topDB's
      // members. The DFDesignBlock-descent guard inside `emit` prevents the
      // re-entry from looping when topDsn (a member of topDB.members) triggers
      // a recursive `emit(topDB.members)` on its own sub-DB.
      emit(topDB.members)
      // the def blocks held back while the top design was still pending (see the call case in
      // `emit`), emitted now that the top is in place
      deferredDefs.toList.foreach { targetBlock =>
        if (!seen.contains(targetBlock))
          seen += targetBlock
          flat += targetBlock
          subDBs.get(targetBlock.ownerRef).foreach(sub => emit(sub.members))
      }
      // Merge refTables from root + every sub-DB. A shared ref key can live in
      // multiple sub-DB refTables (e.g. a nested DFDesignBlock's ownerRef appears
      // in both parent's refTable — where it points to the parent's own members —
      // and child's refTable). When a sub-DB patches its member in-place, only
      // that sub-DB's refTable is rewired; the peer sub-DB's entry for the same
      // key still points at the pre-patch (now removed) instance. Prefer the
      // entry whose target is in `flat` so stale targets lose when a fresh one
      // exists for the same key. DFDesignBlock targets still go through
      // `canonicalize` so refs pointing at the parent's stale copy retarget to
      // the sub-DB's patched version.
      val flatSet = mutable.Set.empty[DFMember]
      flat.foreach(flatSet += _)
      val mergedRefTable = mutable.Map.empty[DFRefAny, DFMember]
      allDBs.foreach { db =>
        db.refTable.foreach { (k, target) =>
          val canonicalTarget = canonicalize(target)
          val isFresh = flatSet.contains(canonicalTarget)
          mergedRefTable.get(k) match
            case None => mergedRefTable(k) = canonicalTarget
            case Some(existing) if !flatSet.contains(existing) && isFresh =>
              mergedRefTable(k) = canonicalTarget
            case _ => // keep existing
        }
      }
      DB(
        members = flat.toList,
        refTable = mergedRefTable.toMap,
        globalTags = this.globalTags,
        srcFiles = this.srcFiles
      )
  end newToOld

  // Lightweight repair of the per-sub-DB global closure: adds any global member
  // (and its missing ref bindings) that a sub-DB's members reference but that is
  // absent from that sub-DB. Recomputes each sub-DB's global closure and
  // refTable IN PLACE on the existing hierarchy, with no flatten and no full
  // rebuild. A stage that mints globals shared across sub-DBs (e.g.
  // GlobalizePortVectorParams creates the globalized port-vector params in the
  // top's MetaDesign, and the vec-type `FullReplacement`s purge their `TypeRef`
  // bindings) leaves exactly these gaps; Only meaningful on a root DB; returns `this`
  // unchanged (by reference) when nothing is missing. Sub-DBs needing no repair
  // are likewise carried over by reference.
  def repairGlobalClosures: DB =
    if (subDBs.isEmpty) return this
    // Merged ref pool: a member's ref whose binding is missing from its own
    // sub-DB is recovered from whichever sub-DB does carry it (globals and their
    // bindings are shared across sub-DBs by identity). First-wins is correct for
    // the global/Empty targets we redistribute (their target is consistent).
    val pool = mutable.Map.empty[DFRefAny, DFMember]
    subDBs.valuesIterator.foreach(_.refTable.foreach((r, t) => pool.getOrElseUpdate(r, t)))
    // Global members + a deterministic (first-occurrence) order across sub-DBs;
    // `topDB` carries them first in canonical order, so this is topological (a
    // global never references a later global).
    val globalOrder = mutable.LinkedHashSet.empty[DFMember]
    subDBs.valuesIterator.foreach { sub =>
      sub.atGetSet {
        sub.members.foreach {
          case g: DFVal.CanBeGlobal if g.isGlobal => globalOrder += g
          case _                                  =>
        }
      }
    }
    val globalSet: Set[DFMember] = globalOrder.toSet
    var anyChanged = false
    val updatedSubDBs = subDBs.map { (k, sub) =>
      // Resolve a ref preferring the sub-DB's own binding (the correct local
      // target), falling back to the shared pool for a binding the sub-DB is
      // missing.
      def resolve(r: DFRefAny): Option[DFMember] = sub.refTable.get(r).orElse(pool.get(r))
      // Global closure: globals transitively reachable from the sub-DB's
      // non-global members' refs.
      val reachable = mutable.HashSet.empty[DFMember]
      def pull(t: DFMember): Unit = t match
        case g: DFVal.CanBeGlobal if globalSet.contains(g) && reachable.add(g) =>
          g.getRefs.foreach(r => resolve(r).foreach(pull))
        case _ =>
      val nonGlobals = sub.members.filterNot(globalSet.contains)
      nonGlobals.foreach(_.getRefs.foreach(r => resolve(r).foreach(pull)))
      // globals first, in canonical order, then the rest — `closure ::: d :: locals`.
      val newMembers = globalOrder.iterator.filter(reachable.contains).toList ::: nonGlobals
      // Rebuild the refTable from the new members' refs (drops orphan entries,
      // fills the missing global bindings).
      val newRefTable = mutable.Map.empty[DFRefAny, DFMember]
      newMembers.foreach { m =>
        m.getAllRefs.foreach(r => resolve(r).foreach(t => newRefTable(r) = t))
      }
      val rebuilt = newRefTable.toMap
      if (newMembers == sub.members && rebuilt == sub.refTable) k -> sub
      else
        anyChanged = true
        k -> sub.update(members = newMembers, refTable = rebuilt)
    }
    if (anyChanged) update(subDBs = updatedSubDBs) else this
  end repairGlobalClosures

  // Normalizes an old-style flat DB so:
  //   1. globals appear BEFORE `top` in `members` (their mutual order
  //      preserved).
  //   2. each non-top DFDesignBlock (and its contiguous content — its own
  //      locals plus any further nested blocks recursively) is positioned
  //      immediately before its FIRST DFDesignInst in the parent's locals.
  //      Matches the form `newToOld` emits, so that round-trip equality holds
  //      against the original elaboration order (which places a nested block
  //      where it was first elaborated, not where the corresponding inst
  //      settles after later stages).
  // Idempotent on already-canonical DBs; a no-op on new-style DBs (those
  // are already canonical).
  def canonicalForm: DB =
    if (subDBs.nonEmpty) return this
    given MemberGetSet = self.getSet
    val globals = mutable.ListBuffer.empty[DFMember]
    val nonGlobals = mutable.ListBuffer.empty[DFMember]
    members.foreach {
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => globals += dfVal
      case m                                          => nonGlobals += m
    }
    val topD = top
    // Per-design ordered locals (excluding nested DFDesignBlocks): members
    // owned by design D land in designLocals(D) in their original elaboration
    // order.
    val designLocals = mutable.LinkedHashMap.empty[DFDesignBlock, mutable.ListBuffer[DFMember]]
    designLocals(topD) = mutable.ListBuffer.empty
    nonGlobals.foreach {
      case d: DFDesignBlock => designLocals.getOrElseUpdate(d, mutable.ListBuffer.empty)
      case _                =>
    }
    nonGlobals.foreach {
      case _: DFDesignBlock => // nested blocks reordered separately
      case m                => designLocals(m.getOwnerDesign) += m
    }
    val emittedDesigns = mutable.Set.empty[DFDesignBlock]
    val out = mutable.ListBuffer.empty[DFMember]
    def emitDesign(d: DFDesignBlock): Unit =
      out += d
      designLocals.getOrElse(d, mutable.ListBuffer.empty).foreach {
        case inst: DFDesignInst =>
          val target = inst.getDesignBlock
          if (!emittedDesigns.contains(target))
            emittedDesigns += target
            emitDesign(target)
          out += inst
        case DFVal.Func.Call(call, key) =>
          val target = key.getDesignBlock
          if (!emittedDesigns.contains(target))
            emittedDesigns += target
            emitDesign(target)
          out += call
        case m => out += m
      }
    end emitDesign
    emittedDesigns += topD
    emitDesign(topD)
    val newMembers = globals.toList ++ out.toList
    if (newMembers == members) this else this.update(members = newMembers)
  end canonicalForm

end DB

object DB:
  def apply(
      members: List[DFMember],
      refTable: Map[DFRefAny, DFMember],
      globalTags: DFTags,
      srcFiles: List[SourceFile]
  ): DB =
    lazy val db: DB = new DB(members, refTable, globalTags, srcFiles, ListMap.empty)(db)
    db.rootDB
    db
  def apply(
      members: List[DFMember],
      refTable: Map[DFRefAny, DFMember],
      globalTags: DFTags,
      srcFiles: List[SourceFile],
      subDBs: ListMap[StaticRef, DB]
  ): DB =
    lazy val updatedSubDBs = subDBs.map((r, subDB) => r -> subDB.copy()(db))
    lazy val db: DB = new DB(members, refTable, globalTags, srcFiles, updatedSubDBs)(db)
    updatedSubDBs.foreach(_._2.rootDB)
    db.rootDB
    db

  // Custom ReadWriter for DB: recursively serializes `subDBs` so a hierarchical
  // (root) DB round-trips losslessly through JSON. The disk cache now persists
  // root DBs because the pipeline runs natively on the hierarchical form. Sub-DBs
  // and old-style flat DBs carry an empty `subDBs`, so the recursion terminates
  // immediately (one level: root -> its sub-DBs). `subDBs` is serialized as an
  // ordered list of (key, sub-DB) pairs to preserve the elaboration-order ListMap.
  // Each sub-DB is serialized as its own JSON string rather than a nested `DB`.
  // This keeps `DBSerialized` free of any reference to `DB`, so deriving
  // `ReadWriter[DBSerialized]` does not recursively summon `ReadWriter[DB]` (which
  // would trigger a spurious "Infinite loop in function body" warning). The
  // recursion now happens only at runtime inside the mapping functions below.
  private type DBSerialized =
    (
        List[DFMember],
        Map[DFRefAny, DFMember],
        DFTags,
        List[SourceFile],
        List[(StaticRef, String)]
    )
  given ReadWriter[DB] =
    readwriter[DBSerialized].bimap[DB](
      db =>
        (
          db.members,
          db.refTable,
          db.globalTags,
          db.srcFiles,
          db.subDBs.toList.map((ref, subDB) => ref -> write(subDB))
        ),
      { case (members, refTable, globalTags, srcFiles, subDBs) =>
        if (subDBs.isEmpty) DB(members, refTable, globalTags, srcFiles)
        else
          DB(
            members,
            refTable,
            globalTags,
            srcFiles,
            ListMap.from(subDBs.map((ref, json) => ref -> read[DB](json)))
          )
      }
    )
  extension (db: DB)
    def toJsonString: String = write(db)
  def fromJsonString(json: String): DB = read[DB](json)
end DB

/** Controls how `owner.members(view)` traverses the ownership tree.
  *   - `Folded`: returns only the direct children of the owner (members whose `getOwner == owner`).
  *   - `Flattened`: returns all descendants recursively. For a `DFDesignBlock` owner this returns
  *     every member in the design (via `designMemberTable`). For other owners it recurses into
  *     nested blocks but does NOT cross `DFDesignBlock` boundaries — sub-designs appear as a single
  *     opaque entry.
  *
  * Use `Folded` when you only need to process immediate children (e.g. steps at one nesting level).
  * Use `Flattened` when you need to inspect or collect all descendants (e.g. all `Goto` members
  * anywhere inside a `ProcessBlock`, or gathering every member to move together with a block).
  */
enum MemberView derives CanEqual:
  case Folded, Flattened

trait MemberGetSet:
  val isMutable: Boolean
  def designDB: DB
  // Structural resolution of a design-block hierarchy key (`StaticRef`). A unified key is
  // a design's `ownerRef` token, so it must NOT be resolved through the refTable (where
  // that token maps to the design's owner); each context resolves it against its own
  // design registry, falling back to the refTable only for the pre-unification
  // `DFDesignInst.designRef` form (a distinct parent-side entry). A design block is
  // always accessible from a key; failure to resolve is a bug.
  def getDesignBlockByKey(key: StaticRef): DFDesignBlock
  def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0
  def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0]
  def getOrigin(ref: DFRef.TwoWayAny): DFMember
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member: M): M
  def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit
  def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT]
  final lazy val topName: String = designDB.top.dclName
end MemberGetSet

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]
