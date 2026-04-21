package dfhdl.compiler.ir

import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{ListMap, ListSet, BitSet}
import dfhdl.internals.*
import dfhdl.compiler.printing.{Printer, DefaultPrinter}
import DFDesignBlock.InstMode
import upickle.default.*

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRefAny, DFMember],
    globalTags: DFTags,
    srcFiles: List[SourceFile],
    // Keyed by the sub-DB's `designBlock.ownerRef` — a lightweight, stable
    // identity for the design instance that isn't invalidated when a stage
    // replaces the DFDesignBlock object via a patch (the patch preserves
    // ownerRef). Each sub-DB carries its own `designBlock: Some(d)` so the
    // actual block is always accessible without a map lookup.
    internalDBs: ListMap[DFOwner.Ref, DB] = ListMap.empty,
    // On new-style sub-DBs this is `Some(d)` where `d` is the design block
    // this sub-DB represents. The design block itself is NOT in `members` —
    // it is a member of its parent's design context. On root / old-style DBs
    // this is `None` and `top` is resolved from `membersNoGlobals.head`.
    designBlock: Option[DFDesignBlock] = None
) derives CanEqual:
  private val self = this
  given getSet: MemberGetSet with
    val isMutable: Boolean = false
    val designDB: DB = self
    // Read from the flat (merged) refTable so new-style DBs with a partitioned
    // `refTable` still resolve refs across sub-DBs. Identical to `refTable`
    // when `internalDBs.isEmpty` (old-style).
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 =
      refTable(ref).asInstanceOf[M0]
    def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] =
      refTable.get(ref).asInstanceOf[Option[M0]]
    def getOrigin(ref: DFRef.TwoWayAny): DFMember = originRefTable(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      newMemberFunc(originalMember)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M = newMember
    def remove[M <: DFMember](member: M): M = member
    def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit = throw new Exception(
      "Cannot set global tag on immutable DB"
    )
    def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT] = globalTags.getTagOf[CT]
    def findDesignInst(design: DFDesignBlock): Option[DFDesignInst] =
      designInstMap.get(design)
  end getSet

  // considered to be in simulation if the top design has no ports
  lazy val inSimulation: Boolean = membersNoGlobals.forall {
    case dcl: DFVal.Dcl if dcl.isPort => dcl.getOwnerDesign != top
    case _                            => true
  }

  // considered to be in build if not in simulation and has a device constraint
  lazy val inBuild: Boolean = !inSimulation && top.isDeviceTop

  lazy val topIOs: List[DFVal.Dcl] = designMemberTable(top).collect {
    case dcl: DFVal.Dcl if dcl.isPort => dcl
  }

  // map of all ports and their by-name selectors
  lazy val portsByNameSelectors: Map[DFVal.Dcl, List[DFVal.PortByNameSelect]] =
    members.view
      .collect { case m: DFVal.PortByNameSelect => m }
      .groupBy(_.getPortDcl)
      .view
      .mapValues(_.toList)
      .toMap

  lazy val membersNoGlobals: List[DFMember] = members.filter {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => false
    case _                                          => true
  }

  lazy val membersGlobals: List[DFVal.CanBeGlobal] = members.collect {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => dfVal
  }

  lazy val top: DFDesignBlock = designBlock.getOrElse(
    membersNoGlobals.head match
      case m: DFDesignBlock => m
      case invalidTop       =>
        throw new IllegalArgumentException(s"Unexpected member as Top:\n$invalidTop")
  )

  lazy val memberTable: Map[DFMember, Set[DFRefAny]] = refTable.invert

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
      // current member indeed belongs to current owner
      case m :: mList if getOwnerFunc(m) == localOwner =>
        val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
        m match
          // Deep borrowing into block as the new owner
          case o: O if classTag[O].runtimeClass.isInstance(o) =>
            val updatedStack2 = (o -> List()) :: updatedStack1
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack2)
          // Just a member
          case _ =>
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

  // Members to feed into OMLGen as "locals excluding top". Both old-style
  // and new-style (option-a) DBs have the top designBlock at
  // `membersNoGlobals.head`, so dropping it yields the locals uniformly.
  private lazy val localsForOML: List[DFMember] = membersNoGlobals.drop(1)

  // holds the topological order of owner owner dependency
  lazy val ownerMemberList: List[(DFOwner, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFOwner](_.getOwner)(List(), localsForOML, List(top -> List())).reverse
//  def printOwnerMemberList(implicit printConfig: CSPrinter.Config): DB = {
//    implicit val printer: CSPrinter = new CSPrinter {
//      val getSet: MemberGetSet = __getset
//      val config: CSPrinter.Config = printConfig
//    }
//    println(
//      ownerMemberList
//        .map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})"))
//        .mkString("\n")
//    )
//    this
//  }
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
    // head will always be the TOP block
    OMLGen[DFDesignBlock](_.getOwnerDesign)(
      List(),
      localsForOML,
      List(top -> List())
    ).reverse

  // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val designMemberTable: Map[DFDesignBlock, List[DFMember]] =
    Map(designMemberList*)

  // Reverse map from each DFDesignBlock to its DFDesignInst. Every non-top
  // design has exactly one DFDesignInst introduced during elaboration; the
  // top-level DFDesignBlock has none (no instantiation site).
  lazy val designInstMap: Map[DFDesignBlock, DFDesignInst] =
    members.view.collect { case inst: DFDesignInst => inst.getDesignBlock -> inst }.toMap

  // holds the topological order of unique design block dependency
  lazy val uniqueDesignMemberList: List[(DFDesignBlock, List[DFMember])] =
    designMemberList.filterNot(_._1.isDuplicate)

  // DBs to iterate for `dup*` analyses. In new-style (internalDBs.nonEmpty),
  // each sub-DB holds only its own slice, so a single DB's `members` isn't
  // enough to find every port / domain block / PBNS. We include this DB and
  // every sub-DB — iteration must use each DB's own `getSet` because ownerRef
  // chains are partitioned per sub-DB. In old-style, just this DB.
  private lazy val dupAnalysisDBs: List[DB] =
    if (internalDBs.nonEmpty) this :: internalDBs.values.toList
    else List(this)

  // All DFDesignBlocks in scope (origins and duplicates). In new-style,
  // duplicates are NOT `internalDBs` keys but they do appear as members in
  // their parent sub-DB's locals, so we walk each DB's members and dedup
  // by object identity. In old-style, walk flat `members`.
  private lazy val dupAnalysisDesigns: List[DFDesignBlock] =
    if (internalDBs.nonEmpty)
      val seen = mutable.Set.empty[DFDesignBlock]
      dupAnalysisDBs.flatMap(_.members).collect {
        case d: DFDesignBlock if seen.add(d) => d
      }
    else members.collect { case d: DFDesignBlock => d }

  // maps each duplicated design to its origin (first non-duplicate design with the same dclName)
  lazy val dupDesignToOrigMap: Map[DFDesignBlock, DFDesignBlock] =
    if (internalDBs.nonEmpty)
      val origByName = dupAnalysisDesigns.filterNot(_.isDuplicate)
        .groupBy(_.dclName).view.mapValues(_.head).toMap
      dupAnalysisDesigns.collect {
        case d if d.isDuplicate => d -> origByName(d.dclName)
      }.toMap
    else
      val origByName =
        uniqueDesignMemberList.map(_._1).groupBy(_.dclName).view.mapValues(_.head).toMap
      designMemberList.collect {
        case (design, _) if design.isDuplicate => design -> origByName(design.dclName)
      }.toMap

  // Single pass: for each duplicate design, compute dup domain blocks AND dup ports together.
  // dupDesignDomainBlockMap: maps (dupDesign, origDomainBlock) -> dupDomainBlock
  // dupPortsByName: maps design -> port name -> Dcl (including dup entries with DuplicationRef)
  lazy val (dupDesignDomainBlockMap, dupPortsByName) =
    // Collect per-DB using each DB's own getSet — `getOwnerDesign` /
    // `getRelativeName` walk ownerRef chains that only resolve within the
    // owning DB's partitioned refTable. Each port / domain block / PBNS lives
    // in exactly one DB, so per-DB maps union cleanly.
    def collectFrom(db: DB): (
        Map[DFDesignBlock, List[DomainBlock]],
        Map[DFDesignBlock, ListMap[String, DFVal.Dcl]],
        Map[DFDesignInstOld, Map[String, DFType]]
    ) =
      given MemberGetSet = db.getSet
      val doms = db.members.view
        .collect { case d: DomainBlock => d }
        .groupBy(_.getOwnerDesign)
        .map { (design, blocks) => design -> blocks.toList }
        .toMap
      val ports = db.members.view
        .collect { case m: DFVal.Dcl if m.isPort => m }
        .groupBy(_.getOwnerDesign)
        .map { case (design, dcls) =>
          design -> ListMap.from(dcls.view.map(m => m.getRelativeName(design) -> m))
        }.toMap
      // PBNS members carry the correct dfType for each duplicate's port
      // (reflecting the actual instantiation parameters), so we use it to
      // override the origin Dcl's dfType.
      val pbns = db.members.view
        .collect { case m: DFVal.PortByNameSelect => m }
        .groupBy(m => m.designInstRef.get)
        .view.mapValues(_.map(m => m.portNamePath -> m.dfType).toMap).toMap
      (doms, ports, pbns)
    end collectFrom
    val (origDomainBlocks, origPortMap, pbnsByDesign) =
      dupAnalysisDBs.map(collectFrom).foldLeft((
        Map.empty[DFDesignBlock, List[DomainBlock]],
        Map.empty[DFDesignBlock, ListMap[String, DFVal.Dcl]],
        Map.empty[DFDesignInstOld, Map[String, DFType]]
      )) { case ((d1, p1, b1), (d2, p2, b2)) =>
        (d1 ++ d2, p1 ++ p2, b1 ++ b2)
      }
    val domainBlockMap = mutable.Map.empty[(DFDesignBlock, DomainBlock), DomainBlock]
    val portEntries = mutable.Map.empty[DFDesignInstOld, ListMap[String, DFVal.Dcl]]
    dupDesignToOrigMap.foreach { (dupDesign, origDesign) =>
      // Resolve origin's ownerRef chain using origin's own sub-DB getSet;
      // in old-style, this DB's getSet suffices.
      given MemberGetSet =
        if (internalDBs.nonEmpty)
          internalDBs.get(origDesign.ownerRef).map(_.getSet).getOrElse(self.getSet)
        else self.getSet
      // 1. Build domain block copies
      val origToDupMap = mutable.Map.empty[DFDomainOwner, DFDomainOwner]
      origToDupMap += origDesign -> dupDesign
      origDomainBlocks.getOrElse(origDesign, Nil).foreach { origBlock =>
        val dupOwner = origToDupMap(origBlock.getOwnerDomain)
        val dupBlock = origBlock.copy(ownerRef = DFRef.DuplicationRef(dupOwner))
        origToDupMap += origBlock -> dupBlock
        domainBlockMap += (dupDesign, origBlock) -> dupBlock
      }
      // 2. Build port copies (reusing origToDupMap from step 1)
      val pbnsTypes = pbnsByDesign.getOrElse(dupDesign, Map.empty)
      portEntries += dupDesign ->
        ListMap.from(origPortMap.getOrElse(origDesign, ListMap.empty).view.map { (name, dcl) =>
          val dfType = pbnsTypes.getOrElse(name, dcl.dfType)
          val dupOwnerDomain = origToDupMap(dcl.getOwnerDomain)
          name -> dcl.copy(ownerRef = DFRef.DuplicationRef(dupOwnerDomain), dfType = dfType)
        })
    }
    // dupEntries only fills in missing entries (designs without real port members)
    (domainBlockMap.toMap, portEntries.toMap ++ origPortMap)
  end val

  lazy val dupDomainOwnerPublicMemberList: List[(DFDomainOwner, List[DFMember])] =
    def publicMemberFilter(member: DFMember): Boolean =
      member match
        case dcl: DFVal.Dcl if dcl.isPort => true
        case design: DFDesignBlock        => true
        case domainBlock: DomainBlock     => true
        case _                            => false
    // For duplicate designs, generate entries for their dup-copy domain blocks
    // by mirroring the origin's domain owner structure.
    def dupEntriesFor(
        origOwner: DFDomainOwner,
        dupDesign: DFDesignBlock
    ): List[(DFDomainOwner, List[DFMember])] =
      val dupOwner: DFDomainOwner = origOwner match
        case _: DFDesignBlock    => dupDesign
        case db: DomainBlock     => dupDesignDomainBlockMap((dupDesign, db))
        case _: DFInterfaceOwner => ??? // TODO
      val origMembers = domainOwnerMemberTable(origOwner)
        .view.filter(publicMemberFilter).toList
      val dupDesignPorts = dupPortsByName.getOrElse(dupDesign, ListMap.empty)
      val dupMembers = origMembers.map {
        case dcl: DFVal.Dcl =>
          val relName = dcl.getRelativeName(origOwner.getThisOrOwnerDesign)
          dupDesignPorts.getOrElse(relName, dcl)
        case design: DFDesignBlock => design // nested designs stay as-is
        case db: DomainBlock       => dupDesignDomainBlockMap((dupDesign, db))
        case m                     => m
      }
      (dupOwner -> dupMembers) :: origMembers.collect { case db: DomainBlock =>
        dupEntriesFor(db, dupDesign)
      }.flatten
    end dupEntriesFor
    domainOwnerMemberList.flatMap { case (owner, members) =>
      owner match
        case dupDesign: DFDesignBlock if dupDesign.isDuplicate =>
          val origDesign = dupDesignToOrigMap(dupDesign)
          dupEntriesFor(origDesign, dupDesign)
        case origDesign: DFDesignBlock =>
          List(origDesign -> members.filter(publicMemberFilter))
        case origDomainBlock: DomainBlock =>
          List(origDomainBlock -> members.filter(publicMemberFilter))
        // TODO: missing interface handling
        case interface: DFInterfaceOwner => ???
    }
  end dupDomainOwnerPublicMemberList

  lazy val dupDomainOwnerPublicMemberTable: Map[DFDomainOwner, List[DFMember]] =
    Map(dupDomainOwnerPublicMemberList*)

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
      connToDcls: ConnectToMap
  ): Access =
    def isExternalConn =
      if (dfVal.isGlobal) true
      else dfVal.getOwnerDesign isSameOwnerDesignAs net
    def isInternalConn =
      if (dfVal.isGlobal) false
      else dfVal isSameOwnerDesignAs net
    dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
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
            if (connToDcls.contains(dcl, slice)) Read
            // otherwise it is unknown
            else Unknown
          // illegal connection
          case _ => Error
      case open if open.isOpen => Unknown
      case _                   => Read
    end match
  end getValAccess
  private def getValAccess(dfVal: DFVal, net: DFNet)(
      connToDcls: ConnectToMap
  ): Access =
    val dpart = dfVal.departial
    getValAccess(dpart._1, dpart._2, net)(connToDcls)
  private case class FlatNet(lhsVal: DFVal, rhsVal: DFVal, net: DFNet) derives CanEqual
  private object FlatNet:
    def apply(net: DFNet): List[FlatNet] =
      (net.lhsRef.get, net.rhsRef.get) match
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          List(FlatNet(lhsVal, rhsVal, net))
        case (lhsIfc: DFInterfaceOwner, rhsIfc: DFInterfaceOwner) =>
          FlatNet(lhsIfc, rhsIfc, net)
        case _ => ???
    def apply(lhsIfc: DFInterfaceOwner, rhsIfc: DFInterfaceOwner, net: DFNet): List[FlatNet] =
      val lhsMembers = getMembersOf(lhsIfc, MemberView.Folded)
      val rhsMembers = getMembersOf(rhsIfc, MemberView.Folded)
      assert(lhsMembers.length == rhsMembers.length)
      lhsMembers.lazyZip(rhsMembers).flatMap {
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          List(FlatNet(lhsVal, rhsVal, net))
        case (lhsIfc: DFInterfaceOwner, rhsIfc: DFInterfaceOwner) =>
          FlatNet(lhsIfc, rhsIfc, net)
        case _ => ???
      }
  end FlatNet
  given printer: Printer = DefaultPrinter
  @tailrec private def getConnToDcls(
      analyzeNets: List[FlatNet],
      pendingNets: List[FlatNet],
      connToDcls: ConnectToMap,
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
        val (lhsAccess, rhsAccess) = net.op match
          // assignment is always from right to left
          case Assignment | NBAssignment =>
            lhsVal.dealias match
              case Some(dcl: DFVal.Dcl) if dcl.modifier.dir == IN =>
                newError("Cannot assign to an input port.")
                (Unknown, Unknown)
              case Some(dcl: DFVal.Dcl) if !(dcl.isSameOwnerDesignAs(net)) =>
                newError("Ports and variables can only be assigned at their own design scope.")
                (Unknown, Unknown)
              case _ =>
                (Write, Read)
          // connections are analyzed according to the context of the net
          case _ => (getValAccess(lhsVal, net)(connToDcls), getValAccess(rhsVal, net)(connToDcls))
        val toValOption = (lhsAccess, rhsAccess) match
          case (Write, Read | ReadWrite | Unknown) => Some(lhsVal)
          case (Read | ReadWrite | Unknown, Write) => Some(rhsVal)
          case (Read, Read)                        =>
            newError("Unsupported read-to-read connection.")
            None
          case (Write, Write) =>
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
        var hasOpenConn = false
        val toDclAndSliceOption: Option[(DFVal.Dcl, Slice)] = toValOption.flatMap(v =>
          if (v.isOpen)
            hasOpenConn = true
            None
          else
            v.departialDcl match
              case None =>
                newError(s"Unexpected write access to the immutable value ${v.relValString}.")
                None
              case Some(dcl, Slice.Concrete(range))
                  if dcl.dfType.widthIntOpt.exists(_ < range.length) =>
                newError(s"Unexpected write access to the immutable value ${v.relValString}.")
                None
              case x => x
        )
        toDclAndSliceOption match
          // found target variable or port declaration for the given connection/assignment
          case Some((toDcl, slice)) =>
            val prevNets = connToDcls.getNets(toDcl, slice)
            // checking multiple assignments from different domains, except for a condition
            // where the declaration is a shared variable.
            // this is used to define a shared variable which is against the RT model,
            // but is useful to described inferred memories like True Dual Port RAM.
            if (!toDcl.modifier.isShared)
              prevNets.headOption.foreach: prevNet =>
                if (prevNet.getOwnerDomain != net.getOwnerDomain)
                  newError(
                    s"""|Found multiple domain assignments to the same variable/port `${toDcl
                         .getFullName}`.
                        |Only variables declared as `VAR.SHARED` under ED domain allow this.
                        |The previous write occurred at ${prevNet.meta.position}""".stripMargin
                  )
            // go through all previous nets and check for collisions
            prevNets.foreach: prevNet =>
              // multiple assignments are allowed in the same range, but not multiple
              // connections or a combination of an assignment and a connection
              if (prevNet.isConnection || prevNet.isAssignment && !net.isAssignment)
                newError(
                  s"""Found multiple connections write to the same variable/port `${toDcl
                      .getFullName}`.
                     |The previous write occurred at ${prevNet.meta.position}""".stripMargin
                )
            // if no previous connection in this range, we add it to the range map
            if (prevNets.isEmpty)
              getConnToDcls(otherNets, pendingNets, connToDcls.addNet(toDcl, slice, net), newErrors)
            // if there are previous connections, it's either assignments or already reported as
            // errors, so no need to further modify the range map (the range map is not intended
            // to save all the previous assignment nets).
            else
              getConnToDcls(otherNets, pendingNets, connToDcls, newErrors)
          // has open connection, so we can skip to the next net as no further checks are needed
          case None if hasOpenConn =>
            getConnToDcls(otherNets, pendingNets, connToDcls, newErrors)
          // unable to determine net directionality, so move net to pending
          case None =>
            getConnToDcls(otherNets, flatNet :: pendingNets, connToDcls, newErrors)
        end match
      case Nil if errors.nonEmpty =>
        throw new IllegalArgumentException(
          errors.view.reverse.mkString("\n\n")
        )
      case Nil if pendingNets.nonEmpty =>
        val reexamine = pendingNets.exists { n =>
          connToDcls.contains(n.lhsVal) | connToDcls.contains(n.rhsVal)
        }
        if (reexamine) getConnToDcls(pendingNets, Nil, connToDcls, errors)
        else
          throw new IllegalArgumentException(
            s"""DFiant HDL connectivity errors!
               |Unable to determine directionality for the following nets:
               |${pendingNets.map(_.net.meta.position).mkString("\n")}""".stripMargin
          )
      case Nil =>
        connToDcls
    end match
  end getConnToDcls

  //                                     To        From
  lazy val magnetConnectionTable: Map[DFVal.Dcl, DFVal.Dcl] =
    var errors = List.empty[String]
    def newError(errMsg: String): Option[DFVal.Dcl] =
      errors = errMsg :: errors
      None
    // TODO: what to do with missing clk/rst definitions in RTDomains when they are not declared?
    // Option 1: create a dedicated check for clk/rst
    // Option 2: always add clk/rst in RTDomains in elaboration using injection, if the user did not construct them.
    //           this will remove the need for AddClkRst stage.
    // Option 3: apply AddClkRst stage after elaboration and before elaboration checks. this is interesting since we could
    //           use this mechanism to apply various design fixes from elaboration meta-programming.
    def missingSourceError(targetPort: DFVal.Dcl) =
      // newError(
      //   s"""|Missing magnet source for target port ${targetPort.getName}
      //       |Position:  ${targetPort.meta.position}
      //       |Hierarchy: ${targetPort.getOwnerNamed.getFullName}""".stripMargin
      // )
      None
    // group magnet ports/vars according to the magnet type
    val magnetDclGroups =
      members.view
        .collect {
          case dcl @ DFVal.Dcl(dfType = dfType: DFOpaque) if dfType.isMagnet =>
            (dcl, dfType)
        }
        .groupMap(_._2)(_._1).values.map(_.toSet).toList
    // println(magnetDclGroups.map(_.mkString("\n").hindent).mkString("G\n", "G\n", ""))
    // set of magnet ports that are explicitly connected/assigned
    val alreadyConnectedOrAssigned =
      assignmentsTable.keys.flatMap(_.dealias).collect {
        case dcl @ DFVal.Dcl(dfType = dfType: DFOpaque) if dcl.isPort && dfType.isMagnet =>
          dcl
      }.toSet ++ connectionTable.dcls.collect {
        case dcl @ DFVal.Dcl(dfType = dfType: DFOpaque) if dcl.isPort && dfType.isMagnet =>
          dcl
      }
    // flatten connection map for all magnet groups
    val ret = magnetDclGroups.flatMap { dclGrp =>
      dclGrp.view
        // first rejecting inviable magnet targets
        .filterNot { dcl =>
          val dclOwnerDesign = dcl.getOwnerDesign
          // rejecting top level inputs
          (dcl.isPortIn && dclOwnerDesign.isTop) ||
          // rejecting blackbox and duplicated design outputs
          (dcl.isPortOut && (dclOwnerDesign.isBlackBox || dclOwnerDesign.isDuplicate)) ||
          // rejecting explicitly connected/assigned ports
          alreadyConnectedOrAssigned.contains(dcl)
        }
        // finding the magnet source port for each target port
        .flatMap { targetPort =>
          val targetDsn = targetPort.getOwnerDesign
          val sourcePort: Option[DFVal.Dcl] =
            if (targetPort.isPortIn)
              // sorted source in port candidates according to the distance
              val sourceInCandidates = dclGrp.filter { dcl =>
                (dcl.isPortIn || dcl.isVar) && !dcl.isSameOwnerDesignAs(targetPort) &&
                targetPort.isInsideOwner(dcl.getOwnerDesign)
              }.map { dcl =>
                (dcl, targetDsn.getDistanceFromOwnerDesign(dcl.getOwnerDesign))
              }.toList.sortBy(_._2)
              // sorted source out port candidates according to the distance
              val sourceOutCandidates = dclGrp.filter(_.isPortOut)
                .map { port =>
                  val portDsn = port.getOwnerDesign
                  val commonDesign = targetDsn.getCommonDesignWith(portDsn)
                  (
                    port,
                    targetDsn.getDistanceFromOwnerDesign(commonDesign),
                    portDsn.getDistanceFromOwnerDesign(commonDesign)
                  )
                }.toList.sortBy(_._3).sortBy(_._2)
              (sourceInCandidates, sourceOutCandidates) match
                case (Nil, Nil) =>
                  missingSourceError(targetPort)
                case (Nil, (src, _, _) :: _) =>
                  Some(src)
                case ((src, _) :: _, Nil) =>
                  Some(src)
                case ((srcIn, distIn) :: _, (srcOut, distOut, _) :: _) =>
                  if (distIn < distOut) Some(srcIn)
                  else
                    newError(
                      s"""|Found two possible magnet sources for a target magnet.
                          |Target Position:  ${targetPort.meta.position}
                          |Target Path:      ${targetPort.getFullName}
                          |Source1 Position: ${srcIn.meta.position} 
                          |Source1 Path:     ${srcIn.getFullName}
                          |Source2 Position: ${srcOut.meta.position} 
                          |Source2 Path:     ${srcOut.getFullName}""".stripMargin
                    )
              end match
            // target is an output
            else
              // sorted source candidates according to the distance
              val sourceOutCandidates = dclGrp.filter { dcl =>
                (dcl.isPortOut || dcl.isVar) && !dcl.isSameOwnerDesignAs(targetPort) &&
                dcl.isInsideOwner(targetDsn) ||
                dcl.isPortIn && dcl.isSameOwnerDesignAs(targetPort)
              }.map { dcl =>
                (dcl, dcl.getDistanceFromOwnerDesign(targetDsn))
              }.toList.sortBy(_._2)
              sourceOutCandidates match
                case Nil =>
                  missingSourceError(targetPort)
                case (src, ld) :: otherCandidates =>
                  var lastDistance: Int = ld
                  var lastSrc: DFVal.Dcl = src
                  otherCandidates.foreach { case (src, distance) =>
                    if (distance == lastDistance)
                      newError(
                        s"""|Found two possible magnet sources for a target magnet.
                            |Target Position:  ${targetPort.meta.position}
                            |Target Path:      ${targetPort.getFullName}
                            |Source1 Position: ${lastSrc.meta.position} 
                            |Source1 Path:     ${lastSrc.getFullName}
                            |Source2 Position: ${src.meta.position} 
                            |Source2 Path:     ${src.getFullName}""".stripMargin
                      )
                    lastDistance = distance
                    lastSrc = src
                  }
                  Some(src)
              end match
          sourcePort.map(targetPort -> _)
        }
    }.toMap
    if (errors.nonEmpty)
      throw new IllegalArgumentException(
        errors.view.reverse.mkString("\n\n")
      )
    ret
  end magnetConnectionTable

  def checkDanglingPorts(): Unit =
    val assignmentsDclTable =
      assignmentsTable.keys
        .flatMap(_.departialDcl)
        .foldLeft(Map.empty[DFVal.Dcl, Coverage]) { case (acc, (dcl, slice)) =>
          acc.updated(
            dcl,
            acc.getOrElse(dcl, Coverage.empty).assign(slice, dcl.dfType.widthIntOpt)
          )
        }
    // collect all ports that are not connected directly or implicitly as magnets
    val danglingPorts = dupPortsByName.view.flatMap { (ownerDesign, ports) =>
      ports.collect {
        case (_, p: DFVal.Dcl)
            if p.isPortIn && !p.isClkDcl && !p.isRstDcl && !connectionTable.contains(p) &&
              !ownerDesign.isTop && !magnetConnectionTable.contains(p) =>
          s"""|DFiant HDL connectivity error!
              |Position:  ${ownerDesign.meta.position}
              |Hierarchy: ${ownerDesign.getFullName}
              |Message:   Found a dangling (unconnected) input port `${p.getName}`.""".stripMargin
        case (_, p: DFVal.Dcl)
            if p.isPortOut && !ownerDesign.isDuplicate && !ownerDesign.isBlackBox &&
              !connectionTable.contains(p) && !assignmentsDclTable.contains(p) &&
              !magnetConnectionTable.contains(p) && !p.hasNonBubbleInit =>
          val ownerDesign = p.getOwnerDesign
          s"""|DFiant HDL connectivity error!
              |Position:  ${p.meta.position}
              |Hierarchy: ${ownerDesign.getFullName}
              |Message:   Found a dangling (unconnected/unassigned and uninitialized) output port `${p.getName}`.""".stripMargin
      }
    }
    if (danglingPorts.nonEmpty)
      throw new IllegalArgumentException(
        danglingPorts.mkString("\n")
      )
  end checkDanglingPorts

  // holds for each RTDomain/RTDesign/RTInterface that its configuration on another domain,
  // the domain it is dependent on
  lazy val dependentRTDomainOwners: Map[DFDomainOwner, DFDomainOwner] =
    extension (member: DFMember)
      def getRTOwnerOption: Option[DFDomainOwner] =
        val owner = member.getOwnerDomain
        owner.domainType match
          case _: DomainType.RT => Some(owner)
          case _                => None
    // Use dupDomainOwnerPublicMemberList to include domain owners from duplicate designs
    dupDomainOwnerPublicMemberList.view.flatMap { (domainOwner, domainMembers) =>
      domainOwner.domainType match
        // only RT domain owners are saved
        case DomainType.RT(cfg) =>
          cfg match
            // derived configuration dependency is set according to various factors:
            case RTDomainCfg.Derived =>
              domainOwner match
                // for designs, the derived configuration is defined by the owner RT design, if such exists.
                // if not, then there is no domain configuration dependency
                case design: DFDesignBlock =>
                  if (design.isTop) None
                  else design.getRTOwnerOption.map(design -> _)
                // for domains, the derived configuration is defined according to the input ports source,
                // if such ports exist (ignoring Clk/Rst ports).
                // otherwise, the derived configuration is defined by the domain's owner.
                case domain: DomainBlock =>
                  val inPorts = domainMembers.collect {
                    case dcl: DFVal.Dcl if dcl.isPortIn && !dcl.isClkDcl && !dcl.isRstDcl => dcl
                  }
                  val inSourceDomains = inPorts.view.flatMap { port =>
                    connectionTable.getNets(port).headOption match
                      case Some(DFNet.Connection(_, from, _)) => from.getRTOwnerOption
                      case _                                  => None
                  }.toSet
                  if (inSourceDomains.isEmpty) domain.getRTOwnerOption.map(domain -> _)
                  else if (inSourceDomains.size > 1)
                    throw new IllegalArgumentException(
                      s"""|Found ambiguous source RT configurations for the domain:
                          |${domain.getFullName}
                          |Sources:
                          |${inSourceDomains.map(_.getFullName).mkString("\n")}
                          |Possible solution:
                          |Either explicitly define a configuration for the domain or drive it from a single source domain.
                          |""".stripMargin
                    )
                  else Some(domain -> inSourceDomains.head)
                case ifc: DFInterfaceOwner =>
                  ??? // TODO: decide what are the rules are for interfaces
            // related configuration is just dependent on the its related domain
            case RTDomainCfg.Related(relatedDomainRef) =>
              Some(domainOwner -> relatedDomainRef.get)
            case _ => None
        case _ => None
    }
      .toMap
  end dependentRTDomainOwners

  // mapping designs and their uses of Clk/Rst
  // we use the design declaration name (after enforcing uniqueness) because designs
  // can be empty duplicates and the indications need to come from the full designs
  private lazy val designUsesClkRst =
    mutable.Map.empty[String, (usesClk: Boolean, usesRst: Boolean)]
  private lazy val domainOwnerUsesClkRst =
    mutable.Map.empty[DFDomainOwner, (usesClk: Boolean, usesRst: Boolean)]
  private lazy val reversedDependents = dependentRTDomainOwners.invert

  extension (domainOwner: DFDomainOwner)
    private def getDomainClkConstraintsView: collection.View[constraints.Constraint] =
      domainOwner.getConstraints.view ++
        domainOwnerMemberTable(domainOwner).view.collectFirst {
          case dcl: DFVal.Dcl if dcl.isPortIn && dcl.isClkDcl => dcl.getConstraints
        }.getOrElse(Nil)
    private def getTimingConstraintClkRateOpt: Option[RateNumber] =
      getDomainClkConstraintsView.collectFirst {
        case c: constraints.Timing.Clock => c.rate
      }
    private def getExplicitCfg: RTDomainCfg.Explicit =
      domainOwner.domainType match
        case DomainType.RT(explicitCfg: RTDomainCfg.Explicit) => explicitCfg
        case _                                                =>
          val defaultCfg = globalTags.getTagOf[DefaultRTDomainCfgTag].get.cfg
          val isDeviceTop = domainOwner.getThisOrOwnerDesign.isDeviceTop
          // device top designs override the default clock and reset configuration
          if (isDeviceTop)
            val cfgName = ""
            val clkCfg: ConfigN[ClkCfg.Explicit] = getTimingConstraintClkRateOpt match
              case Some(rate) => defaultCfg.clkCfg.asInstanceOf[ClkCfg.Explicit].copy(rate = rate)
              case None       => defaultCfg.clkCfg
            // disabling default rst configuration for device top designs
            // because it's wrong to directly propagate external rst throughout the design
            // without proper synchronization and possibly debouncing.
            val rstCfg = None
            defaultCfg.copy(clkCfg = clkCfg, rstCfg = rstCfg)
          else defaultCfg
          end if

    private def usesClkRst: (usesClk: Boolean, usesRst: Boolean) = domainOwner match
      case design: DFDesignBlock =>
        designUsesClkRst.getOrElseUpdate(
          design.dclName,
          design.domainType match
            case DomainType.RT(RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
              (clkCfg != None, rstCfg != None)
            case _ => (design.usesClk, design.usesRst)
        )
      case _ =>
        domainOwnerUsesClkRst.getOrElseUpdate(
          domainOwner,
          (domainOwner.usesClk, domainOwner.usesRst)
        )
    private def usesClk: Boolean = domainOwnerMemberTable(domainOwner).exists {
      case dcl: DFVal.Dcl                      => dcl.isReg || dcl.isClkDcl
      case reg: DFVal.Alias.History            => true
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst.usesClk
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst.usesClk) ||
      domainOwner.isTop &&
      (domainOwner.getExplicitCfg.clkCfg match
        case ClkCfg.Explicit(inclusionPolicy = ClkRstInclusionPolicy.AlwaysAtTop) => true
        case _                                                                    => false)

    private def usesRst: Boolean = domainOwnerMemberTable(domainOwner).exists {
      case dcl: DFVal.Dcl =>
        (dcl.isReg && dcl.hasNonBubbleInit) || dcl.isRstDcl
      case reg: DFVal.Alias.History            => reg.hasNonBubbleInit
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst.usesRst
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst.usesRst) ||
      domainOwner.isTop &&
      (domainOwner.getExplicitCfg.rstCfg match
        case RstCfg.Explicit(inclusionPolicy = ClkRstInclusionPolicy.AlwaysAtTop) => true
        case _                                                                    => false)
  end extension

  extension (cfg: RTDomainCfg.Explicit)
    // derived design configuration can be relaxed to no-Clk/Rst according to its
    // internal usage, as determined by `usesClkRst`
    private def relaxed(atDomain: DFDomainOwner): RTDomainCfg.Explicit =
      val (usesClk, usesRst) = atDomain.usesClkRst
      val updatedClkCfg: ClkCfg = if (usesClk) cfg.clkCfg else None
      val updatedRstCfg: RstCfg = if (usesRst) cfg.rstCfg else None
      val updatedName =
        if (!usesClk) s"RTDomainCfg.Comb"
        else if (cfg.clkCfg != None && !usesRst)
          s"${cfg.name}.norst"
        else cfg.name
      RTDomainCfg.Explicit(updatedName, updatedClkCfg, updatedRstCfg)
        .asInstanceOf[RTDomainCfg.Explicit]
  end extension

  @tailrec private def fillDomainMap(
      domains: List[DFDomainOwner],
      stack: List[DFDomainOwner],
      domainMap: mutable.Map[DFDomainOwner, RTDomainCfg.Explicit]
  ): Unit =
    domains match
      // already has configuration for this domain -> skip it
      case domain :: rest if domainMap.contains(domain) => fillDomainMap(rest, stack, domainMap)
      // no configuration for this domain
      case domain :: rest =>
        // check if the domain is dependent
        dependentRTDomainOwners.get(domain) match
          // the domain is dependent -> its configuration is set by the dependency
          case Some(dependencyDomain) =>
            domainMap.get(dependencyDomain) match
              // found dependency configuration -> save it to the current domain as well
              case Some(dependencyConfig) =>
                domainMap += domain -> dependencyConfig.relaxed(domain)
                fillDomainMap(rest, stack, domainMap)
              // missing dependency -> put this domain in the stack for now
              case None => fillDomainMap(rest, domain :: stack, domainMap)
            end match
          // the domain is independent -> explicit configuration is set according to other factors
          case _ =>
            val explicitCfg = domain.getExplicitCfg
            domainMap += domain -> explicitCfg.relaxed(domain)
            fillDomainMap(rest, stack, domainMap)
        end match
      // no more domains, but there are left in the stack
      case Nil if stack.nonEmpty => fillDomainMap(domains = stack, Nil, domainMap)
      // we're done!
      case _ =>
    end match
  end fillDomainMap

  lazy val explicitRTDomainCfgMap: Map[DFDomainOwner, RTDomainCfg.Explicit] =
    val domainMap = mutable.Map.empty[DFDomainOwner, RTDomainCfg.Explicit]
    val derivedDomainOwners = domainOwnerMemberList.map(_._1)
    fillDomainMap(derivedDomainOwners, Nil, domainMap)
    domainMap.toMap

  /** Checks that device top design domains all have timing clock rate constraints. Additionally, if
    * there is an explicit clock rate configuration, it must match the timing constraint rate.
    */
  def domainClkRateCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    domainOwnerMemberList.view.map(_._1).foreach {
      case domainOwner if domainOwner.getThisOrOwnerDesign.isDeviceTop && domainOwner.usesClk =>
        def waitError(msg: String): Unit =
          val pos =
            if (domainOwner.isTop) domainOwner.asInstanceOf[DFDesignBlock].dclMeta.position
            else domainOwner.meta.position
          errors += s"""|DFiant HDL domain clock rate error!
                        |Position:  ${pos}
                        |Hierarchy: ${domainOwner.getFullName}
                        |Message:   $msg""".stripMargin
        val explicitRateOpt = domainOwner.domainType match
          case DomainType.RT(RTDomainCfg.Explicit(clkCfg = ClkCfg.Explicit(rate = rate))) =>
            Some(rate)
          case _ => None
        val timingConstraintRateOpt = domainOwner.getTimingConstraintClkRateOpt

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
      case _ =>
    }
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end domainClkRateCheck

  def waitCheck(): Unit =
    val errors = collection.mutable.ArrayBuffer[String]()
    for
      wait <- members.collect { case w: Wait if w.isInRTDomain => w }
      trigger = wait.triggerRef.get
      if trigger.dfType == DFTime
    do
      def waitError(msg: String): Unit =
        errors += s"""|DFiant HDL wait error!
                      |Position:  ${wait.meta.position}
                      |Hierarchy: ${wait.getOwnerDesign.getFullName}
                      |Message:   $msg""".stripMargin
      val ownerDomain = wait.getOwnerDomain
      trigger.getConstData[TimeNumber].toOption match
        case Some(waitTime) =>
          // Check if the wait statement is in a domain with a clock rate configuration
          explicitRTDomainCfgMap.get(ownerDomain) match
            case Some(RTDomainCfg.Explicit(_, clkCfg: ClkCfg.Explicit, _)) =>

              // Get the clock period in picoseconds
              val clockPeriodPs = clkCfg.rate.to_ps.value
              val desc = clkCfg.rate match
                case time: TimeNumber => s"period ${time}"
                case freq: FreqNumber => s"frequency ${freq}"

              // Get wait duration in picoseconds
              val waitDurationPs = waitTime.to_ps.value

              // Check if wait duration is exactly divisible by clock period
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

    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.mkString("\n"))
  end waitCheck

  def circularDerivedDomainsCheck(): Unit =
    // Helper function to perform DFS and detect cycles
    @tailrec def dfs(
        node: DFDomainOwner,
        visited: Set[DFDomainOwner],
        stack: Set[DFDomainOwner]
    ): Unit =
      if (stack.contains(node))
        throw new IllegalArgumentException(
          s"""|Circular derived RT configuration detected. Involved in the cycle:
              |${stack.map(_.getFullName).mkString("\n")}
              |""".stripMargin
        )
      if (!visited.contains(node))
        val newVisited = visited + node
        val newStack = stack + node
        dependentRTDomainOwners.get(node) match
          case Some(dependentNode) => dfs(dependentNode, newVisited, newStack)
          case None                => // No dependency, end of this path
    end dfs
    // Iterate over all nodes in the map and perform DFS
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
            // port referencing is done by name, and supported for
            // internal designs, but not external ones
            case PortByNameSelect.Of(refMember) =>
              if (refMember.isOutsideOwner(m.getOwnerDesign))
                Some(refMember)
              else None
            // design referenced by its member (like in RelatedDomain)
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

  def portLocationCheck(): Unit =
    val errors = mutable.ListBuffer.empty[String]
    val locationCollisions = mutable.ListBuffer.empty[String]

    designMemberList.foreach {
      case (design, members) if design.isDeviceTop =>
        // Collect all location constraints to check for collisions
        val locationMap = mutable.Map.empty[String, String] // loc -> portName(idx)
        (design :: members).foreach {
          case designInstance: DFDesignBlock if designInstance != design => // no need to check for location constraints in nested designs
          case domainOwner: DFDomainOwner =>
            domainOwner.domainType match
              case DomainType.RT(_) =>
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

                // for internal domains (indicated by a clock variable) we don't need to check for location constraints
                if (!foundLoc && !clkIsVar)
                  errors += s"${domainOwner.getFullName} is missing a clock location constraint"
              case _ =>
            end match
          case clkPort: DFVal.Dcl if clkPort.isPortIn && clkPort.isClkDcl => // do nothing (checked in the domain itself)
          case port: DFVal.Dcl if port.isPort =>
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

  def portResourceDirCheck(): Unit =
    import DFVal.Modifier.Dir
    val errors = mutable.ListBuffer.empty[String]

    designMemberList.foreach {
      case (design, members) if design.isDeviceTop =>
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

  def check(): Unit =
    nameCheck()
    connectionTable // causes connectivity checks
    magnetConnectionTable // causes magnet connectivity checks
    checkDanglingPorts()
    directRefCheck()
    circularDerivedDomainsCheck()
    domainClkRateCheck()
    waitCheck()
    portLocationCheck()
    portResourceDirCheck()
  end check

  // There can only be a single connection to a value in a given range
  // (multiple assignments are possible)
  lazy val connectionTable: ConnectToMap =
    val flatNets = members.flatMap {
      case net: DFNet => FlatNet(net)
      case _          => Nil
    }
    getConnToDcls(flatNets, Nil, ConnectToMap.empty, Nil).removeAssignments

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

  // Converts an old-style flat DB (internalDBs.empty) to a canonical new-style DB
  // under the symmetric "option (a)" shape:
  //   - Every DB (root AND each sub-DB) has:
  //         members = [globalMembers, designBlock, localMembers]
  //     where `globalMembers` = the closure of globals reachable by the DB's
  //     local refs, `designBlock` = the DB's own top-level design header, and
  //     `localMembers` = the DB's direct locals (excluding the designBlock).
  //   - Root's `designBlock` is the top design; root has no locals of its
  //     own (top's locals live in topSubDB). Root's globals closure keeps
  //     ALL globals in their original elaboration order so round-tripping
  //     through `newToOld` + `canonicalForm` preserves global order.
  //   - Globals and DFDesignBlocks are shared across DBs by OBJECT identity.
  //   - Each sub-DB's `refTable` is self-contained for its own refs (refs
  //     emitted by any of its members — including the shared globals and
  //     designBlock).
  //   - Each DB carries its OWN subtree in `internalDBs` (flat ListMap of
  //     descendants); root's `internalDBs` spans the entire hierarchy.
  def oldToNew: DB =
    if (internalDBs.nonEmpty) return this
    given MemberGetSet = self.getSet
    val topDsn = this.top
    // All globals in original elaboration order — used verbatim as root's
    // globalMembers so that `newToOld + canonicalForm` preserves ordering.
    val allGlobals: List[DFMember] = members.collect {
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => dfVal
    }
    // designOwn(d) = d's own (non-global, non-self) members in original order.
    // Nested DFDesignBlocks live in their parent's designOwn (as "locals" of
    // the parent) AND become the designBlock of their own sub-DB.
    val designOwn = mutable.LinkedHashMap.empty[DFDesignBlock, mutable.ListBuffer[DFMember]]
    designOwn(topDsn) = mutable.ListBuffer.empty
    members.foreach {
      case d: DFDesignBlock => designOwn.getOrElseUpdate(d, mutable.ListBuffer.empty)
      case _                =>
    }
    members.foreach {
      case d: DFDesignBlock if d == topDsn            => // top has no parent
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => // globals handled separately
      case m                                          => designOwn(m.getOwnerDesign) += m
    }
    // Compute the closure of globals transitively reachable from a DB's refs.
    // Walks local members' refs; when a ref target is a global, we include it
    // and recurse through its own refs to pick up globals-referenced-by-globals.
    // Non-global intermediaries are NOT included (they belong to their own
    // design's locals), but their refs ARE walked because we iterate all of
    // the design's locals directly.
    def globalsClosure(localMembers: Iterable[DFMember]): List[DFMember] =
      val closure = mutable.LinkedHashSet.empty[DFMember]
      def pull(target: DFMember): Unit = target match
        case g: DFVal.CanBeGlobal if g.isGlobal && !closure.contains(g) =>
          closure += g
          g.getRefs.foreach(r => refTable.get(r).foreach(pull))
        case _ =>
      localMembers.foreach { m =>
        m.getRefs.foreach(r => refTable.get(r).foreach(pull))
      }
      closure.toList
    // Build the refTable partition for a DB: every ref emitted (via ownerRef
    // or getRefs) by any of the DB's members, resolved against the original
    // flat refTable.
    def refsFor(dbMembers: Iterable[DFMember]): Map[DFRefAny, DFMember] =
      val result = mutable.Map.empty[DFRefAny, DFMember]
      dbMembers.foreach { m =>
        refTable.get(m.ownerRef).foreach(t => result(m.ownerRef) = t)
        m.getRefs.foreach(r => refTable.get(r).foreach(t => result(r) = t))
        // DFDesignInst.designRef is OneWay and not reported by getRefs — pick it up explicitly.
        m match
          case inst: DFDesignInst =>
            refTable.get(inst.designRef).foreach(t => result(inst.designRef) = t)
          case _ =>
      }
      result.toMap
    // Build sub-DBs bottom-up so each one captures its full descendant tree.
    val subDBs = mutable.Map.empty[DFDesignBlock, DB]
    def buildSubDB(d: DFDesignBlock): DB =
      subDBs.getOrElseUpdate(
        d, {
          val locals = designOwn(d).toList
          val closure = globalsClosure(locals)
          val dbMembers = closure ::: d :: locals
          val dbRefTable = refsFor(dbMembers)
          val directChildren = locals.collect { case c: DFDesignBlock => c }
          val childEntries = directChildren.map(c => c -> buildSubDB(c))
          val descendants = childEntries.flatMap { case (c, cDB) =>
            (c.ownerRef -> cDB) :: cDB.internalDBs.toList
          }
          DB(
            members = dbMembers,
            refTable = dbRefTable,
            // Sub-DBs inherit globalTags from the root so per-design stage
            // helpers (e.g. explicitRTDomainCfgMap) find project-wide tags
            // like DefaultRTDomainCfgTag when dispatched against a sub-DB.
            globalTags = this.globalTags,
            srcFiles = Nil,
            internalDBs = ListMap.from(descendants),
            designBlock = Some(d)
          )
        }
      )
    val topSubDB = buildSubDB(topDsn)
    val rootInternalDBs = (topDsn.ownerRef -> topSubDB) :: topSubDB.internalDBs.toList
    // Root members: ALL globals (in original order) + topDsn. No locals at
    // root — top's locals live in topSubDB. Keeping all globals at root keeps
    // the canonical ordering trivial for the round-trip check.
    val rootMembers: List[DFMember] = allGlobals :+ topDsn
    // Root's refTable: refs emitted by its members. Include an explicit
    // fallback for any entries in the original refTable not reached by
    // member iteration (defensive — handles edge-case dangling entries).
    val rootRefTable = mutable.Map.empty[DFRefAny, DFMember]
    rootRefTable ++= refsFor(rootMembers)
    val assignedRefs = mutable.Set.empty[DFRefAny]
    assignedRefs ++= rootRefTable.keys
    subDBs.values.foreach(sub => assignedRefs ++= sub.refTable.keys)
    refTable.iterator.filterNot { case (r, _) => assignedRefs.contains(r) }.foreach {
      case (r, target) => rootRefTable += r -> target
    }
    DB(
      members = rootMembers,
      refTable = rootRefTable.toMap,
      globalTags = this.globalTags,
      srcFiles = this.srcFiles,
      internalDBs = ListMap.from(rootInternalDBs),
      designBlock = Some(topDsn)
    )
  end oldToNew

  // Collapses a new-style (option-a) DB back into a flat old-style DB.
  // Non-DFDesignBlock members dedup by OBJECT identity (globals are shared by
  // identity across every DB that references them in Phase 1 / early Phase 2).
  // DFDesignBlocks dedup by ownerRef because a stage that patches a sub-DB's
  // own designBlock produces a new instance in that sub-DB only — the parent
  // sub-DB (which also has the designBlock as a member) retains the stale
  // instance. The sub-DB's version wins: it's the one that saw the patch.
  def newToOld: DB =
    if (internalDBs.isEmpty) return this
    val allDBs: List[DB] = this :: internalDBs.values.toList
    // canonical DFDesignBlock per ownerRef: the one in its own sub-DB's
    // `members` (the sub-DB owns patches against its header).
    val canonicalDesign = mutable.Map.empty[DFOwner.Ref, DFDesignBlock]
    internalDBs.foreach { (key, subDB) =>
      subDB.members.collectFirst {
        case d: DFDesignBlock if d.ownerRef == key => d
      }.foreach(canonicalDesign(key) = _)
    }
    def canonicalize(m: DFMember): DFMember = m match
      case d: DFDesignBlock => canonicalDesign.getOrElse(d.ownerRef, d)
      case _                => m
    val flat = mutable.ListBuffer.empty[DFMember]
    val seen = mutable.Set.empty[DFMember]
    def emit(ms: List[DFMember]): Unit =
      ms.foreach { m =>
        val c = canonicalize(m)
        if (!seen.contains(c))
          seen += c
          flat += c
          c match
            case d: DFDesignBlock if internalDBs.contains(d.ownerRef) =>
              emit(internalDBs(d.ownerRef).members)
            case _ =>
      }
    emit(this.members)
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
      srcFiles = this.srcFiles,
      internalDBs = ListMap.empty,
      designBlock = None
    )
  end newToOld

  // Normalizes an old-style flat DB so globals appear BEFORE `top` in
  // `members` (their mutual order preserved). Idempotent on already-canonical
  // DBs; a no-op on new-style DBs (those are already canonical).
  def canonicalForm: DB =
    if (internalDBs.nonEmpty) return this
    given MemberGetSet = self.getSet
    val globals = mutable.ListBuffer.empty[DFMember]
    val nonGlobals = mutable.ListBuffer.empty[DFMember]
    members.foreach {
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => globals += dfVal
      case m                                          => nonGlobals += m
    }
    val newMembers = globals.toList ++ nonGlobals.toList
    if (newMembers == members) this else this.copy(members = newMembers)
  end canonicalForm

end DB

object DB:
  // Custom ReadWriter for DB: excludes `internalDBs` from serialization.
  // Phase 1 never persists a populated `internalDBs` (it's only used
  // transiently inside `sanityCheck`), so the round-trip over JSON stays
  // lossless. When Phase 5 introduces disk-cached sub-DBs this will be
  // replaced with a full recursive ReadWriter.
  private type DBSerialized =
    (List[DFMember], Map[DFRefAny, DFMember], DFTags, List[SourceFile])
  given ReadWriter[DB] =
    readwriter[DBSerialized].bimap[DB](
      db => (db.members, db.refTable, db.globalTags, db.srcFiles),
      { case (members, refTable, globalTags, srcFiles) =>
        DB(members, refTable, globalTags, srcFiles, ListMap.empty)
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
  def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0
  def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0]
  def getOrigin(ref: DFRef.TwoWayAny): DFMember
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member: M): M
  def setGlobalTag[CT <: DFTag: ClassTag](tag: CT): Unit
  def getGlobalTag[CT <: DFTag: ClassTag]: Option[CT]
  def findDesignInst(design: DFDesignBlock): Option[DFDesignInst]
  final lazy val topName: String = designDB.top.dclName
end MemberGetSet

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]
