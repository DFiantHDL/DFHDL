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
    // actual block is always accessible without a map lookup. Only the
    // new-style root DB has a populated `internalDBs`: a flat ListMap of
    // every design in elaboration order (top first, then descendants). Sub-DBs
    // and old-style flat DBs both have an empty `internalDBs`.
    internalDBs: ListMap[DFOwner.Ref, DB] = ListMap.empty,
    // On new-style sub-DBs this is `Some(d)` where `d` is the design block
    // this sub-DB represents — and `d` IS a member of this sub-DB's `members`
    // list. On the new-style root and on old-style flat DBs this is `None`;
    // on root, `top` is resolved through `topDB.designBlock`, and on
    // old-style flat DBs `top` falls back to `membersNoGlobals.head`.
    designBlock: Option[DFDesignBlock] = None
) derives CanEqual:
  private val self = this
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
  end getSet

  // True for the new-style root DB: a hierarchy container with empty members
  // and empty refTable, holding all designs (including the top) in
  // `internalDBs`. False for sub-DBs (which have `internalDBs.empty` and
  // `designBlock = Some(d)`) and for old-style flat DBs (which also have
  // `internalDBs.empty` and `designBlock = None`).
  lazy val isRoot: Boolean = internalDBs.nonEmpty

  // The sub-DB representing the top design. Only meaningful on the new-style
  // root DB; by construction in `oldToNew`, the top design's sub-DB is the
  // first entry of `internalDBs`.
  lazy val topDB: DB = internalDBs.head._2

  lazy val top: DFDesignBlock =
    if (isRoot) topDB.top
    else
      designBlock.getOrElse(
        membersNoGlobals.head match
          case m: DFDesignBlock => m
          case invalidTop       =>
            throw new IllegalArgumentException(s"Unexpected member as Top:\n$invalidTop")
      )

  lazy val topIOs: List[DFVal.Dcl] = designMemberTable(top).collect {
    case dcl: DFVal.Dcl if dcl.isPort => dcl
  }

  // considered to be in simulation if the top design has no ports
  lazy val inSimulation: Boolean = topIOs.isEmpty

  // considered to be in build if not in simulation and has a device constraint
  lazy val inBuild: Boolean = !inSimulation && top.isDeviceTop

  lazy val membersNoGlobals: List[DFMember] = members.filter {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => false
    case _                                          => true
  }

  lazy val membersGlobals: List[DFVal.CanBeGlobal] = members.collect {
    case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => dfVal
  }

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

  // design block to its instances map
  lazy val designBlockInstMap: Map[DFDesignBlock, List[DFDesignInst]] =
    members.view.collect { case inst: DFDesignInst => inst }
      .groupBy(_.getDesignBlock).view.mapValues(_.toList).toMap + (top -> Nil)

  // design block to its owner design blocks map (multiple owners in case of multiple instantiations). Note that the top-level design block has no owner and thus is not included in the map.
  lazy val designBlockOwnershipMap: Map[DFDesignBlock, Set[DFDesignBlock]] =
    designBlockInstMap.view.mapValues(_.view.map(_.getOwnerDesign).toSet).toMap

  // holds the topological order of unique design block dependency
  lazy val uniqueDesignMemberList: List[(DFDesignBlock, List[DFMember])] =
    designMemberList.filterNot(_._1.isDuplicate)

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
          case _ => (getValAccess(lhsVal, net)(connToMap), getValAccess(rhsVal, net)(connToMap))
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

  //                                     To           From
  lazy val magnetConnectionMap: Map[ConnectPoint, ConnectPoint] = MagnetMap.get

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
    val alreadyConnectedPoints = connectionTable.connectToVals.view.collect {
      case dcl: DFVal.Dcl               => ConnectPoint.Direct(dcl)
      case pbns: DFVal.PortByNameSelect => ConnectPoint.Via(pbns)
    }.toSet ++ magnetConnectionMap.keySet

    // go through all designs and their instances
    val danglingPorts = designBlockInstMap.view.flatMap { (design, insts) =>
      designMemberTable(design).flatMap {
        // all input ports that are not clock/reset and not already connected
        case port: DFVal.Dcl if port.isPortIn && !port.isClkDcl && !port.isRstDcl =>
          // all design instances
          insts.flatMap { designInst =>
            val cp = ConnectPoint.Via(designInst, port)
            if (alreadyConnectedPoints.contains(ConnectPoint.Via(designInst, port))) None
            else Some(
              s"""|DFiant HDL connectivity error!
                  |Position:  ${designInst.meta.position}
                  |Hierarchy: ${designInst.getFullName}
                  |Message:   Found a dangling (unconnected) input port `${port.getName}`.""".stripMargin
            )
          }
        // all output ports that are not blackbox and not already assigned/connected/initialized
        case port: DFVal.Dcl
            if port.isPortOut && !design.isBlackBox && !port.hasNonBubbleInit &&
              !assignmentsDclTable.contains(port) &&
              !alreadyConnectedPoints.contains(ConnectPoint.Direct(port)) =>
          Some(
            s"""|DFiant HDL connectivity error!
                |Position:  ${port.meta.position}
                |Hierarchy: ${design.getFullName}
                |Message:   Found a dangling (unconnected/unassigned and uninitialized) output port `${port.getName}`.""".stripMargin
          )
        case _ => None
      }
    }

    if (danglingPorts.nonEmpty)
      throw new IllegalArgumentException(
        danglingPorts.mkString("\n")
      )
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
  end extension

  // =========================================================================
  // Annotation-based RT domain clock/reset resolver.
  //
  // For each RT domain owner, resolvedClkRstMap holds the resolved
  // Timing.Clock / Timing.Reset (Some) or None when the corresponding slot
  // has been stripped by relaxation ("no clock needed" / "no reset needed").
  // =========================================================================

  private lazy val relatedAnnotMap: Map[DFDomainOwner, DFDomainOwner] =
    domainOwnerMemberList.view.flatMap { case (owner, _) =>
      owner.meta.annotations.collectFirst {
        case rel: constraints.Timing.Related => rel.ref.get
      }.map(owner -> _)
    }.toMap

  // Resolves a PortByNameSelect to its underlying DFVal.Dcl by walking the
  // dotted `portNamePath` from the targeted design block. Cross-design refs
  // go through PBNS (the target port lives in a different sub-DB), so this
  // is needed whenever an analysis needs the actual port (e.g. to read its
  // owning RT domain).
  private def pbnsToPort(pbns: DFVal.PortByNameSelect): Option[DFVal.Dcl] =
    val designBlock: DFDesignBlock = pbns.designInstRef.get.getDesignBlock
    val pathParts = pbns.portNamePath.split('.').toList
    @tailrec def walk(owner: DFOwnerNamed, parts: List[String]): Option[DFMember] =
      parts match
        case Nil          => None
        case head :: rest =>
          namedOwnerMemberTable.getOrElse(owner, Nil).collectFirst {
            case n: DFMember.Named if !n.isAnonymous && n.getName == head => n
          } match
            case Some(m) if rest.isEmpty => Some(m)
            case Some(o: DFOwnerNamed)   => walk(o, rest)
            case _                       => None
    walk(designBlock, pathParts) match
      case Some(dcl: DFVal.Dcl) => Some(dcl)
      case _                    => None
  end pbnsToPort

  // Full path for a domain owner that prefers the per-instance name over the
  // canonical design-block class name. With dedup, a non-top DFDesignBlock
  // is reached through one or more DFDesignInsts; we walk through the head
  // inst so that error messages show user-visible instance paths
  // ("Top.internal1.dmn1") rather than class-name paths ("Top.Internal1.dmn1").
  // For design blocks that have multiple instances, the choice picks the
  // first inst — callers that need every instance path must enumerate them
  // separately.
  private def fullNameViaInst(owner: DFDomainOwner): String =
    val design = owner.getThisOrOwnerDesign
    if (design.isTop) owner.getFullName
    else
      designBlockInstMap.get(design).flatMap(_.headOption) match
        case Some(inst) =>
          owner match
            case _: DFDesignBlock => inst.getFullName
            case _                =>
              val relPath = owner.getRelativeName(design)
              s"${inst.getFullName}.$relPath"
        case None => owner.getFullName
  end fullNameViaInst

  lazy val dependentRTDomainOwners: Map[DFDomainOwner, DFDomainOwner] =
    extension (member: DFMember)
      def getRTOwnerOption: Option[DFDomainOwner] =
        // A DFDesignBlock no longer carries a lexical parent in `ownerRef`,
        // so resolve its enclosing domain via the (first) DFDesignInst
        // instead of `getOwnerDomain` (which would throw on the empty ref).
        val owner = member match
          case design: DFDesignBlock =>
            designBlockInstMap.get(design).flatMap(_.headOption).map(_.getOwnerDomain)
          case pbns: DFVal.PortByNameSelect =>
            // Resolve through PBNS so we get the RT domain of the underlying
            // port rather than the parent design that owns the PBNS net.
            Some(pbnsToPort(pbns).map(_.getOwnerDomain).getOrElse(member.getOwnerDomain))
          case _ => Some(member.getOwnerDomain)
        owner.flatMap(_.domainType match
          case DomainType.RT => owner
          case _             => None)
    end extension
    domainOwnerMemberList.view.flatMap { (domainOwner, domainMembers) =>
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
                    if (design.isTopTop) None
                    else design.getRTOwnerOption.map(design -> _)
                  case domain: DomainBlock =>
                    val inPorts = domainMembers.collect {
                      case dcl: DFVal.Dcl if dcl.isPortIn && !dcl.isClkDcl && !dcl.isRstDcl => dcl
                    }
                    // Cross-design connections to an internal RT-domain input
                    // are keyed in the connectionTable by the PBNS at the
                    // parent scope, not by the inner port — collect both so
                    // the source-domain analysis covers external drivers.
                    val enclosingDesign = domain.getOwnerDesign
                    val designInsts = designBlockInstMap.getOrElse(enclosingDesign, Nil)
                    val inSourceDomains = inPorts.view.flatMap { port =>
                      val portRelName = port.getRelativeName(enclosingDesign)
                      val pbnsNets = designInsts.view.flatMap { inst =>
                        designInstPBNS.getOrElse(inst, Nil)
                          .filter(_.portNamePath == portRelName)
                          .flatMap(connectionTable.getNets(_))
                      }.toSet
                      val allNets = connectionTable.getNets(port) ++ pbnsNets
                      allNets.headOption match
                        case Some(DFNet.Connection(_, from, _)) => from.getRTOwnerOption
                        case _                                  => None
                    }.toSet
                    if (inSourceDomains.isEmpty) domain.getRTOwnerOption.map(domain -> _)
                    else if (inSourceDomains.size > 1)
                      throw new IllegalArgumentException(
                        s"""|Found ambiguous source RT configurations for the domain:
                            |${fullNameViaInst(domain)}
                            |Sources:
                            |${inSourceDomains.map(fullNameViaInst).mkString("\n")}
                            |Possible solution:
                            |Either explicitly define a configuration for the domain or drive it from a single source domain.
                            |""".stripMargin
                      )
                    else Some(domain -> inSourceDomains.head)
                  case ifc: DFInterfaceOwner => ???
        case _ => None
    }.toMap
  end dependentRTDomainOwners

  private lazy val designUsesClkRst =
    mutable.Map.empty[String, (usesClk: Boolean, usesRst: Boolean)]
  private lazy val domainOwnerUsesClkRst =
    mutable.Map.empty[DFDomainOwner, (usesClk: Boolean, usesRst: Boolean)]
  private lazy val reversedDependents = dependentRTDomainOwners.invert

  extension (domainOwner: DFDomainOwner)
    private def getResolvedClkRst
        : (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset]) =
      val defaultTag = globalTags.getTagOf[DefaultRTDomainCfgTag].get
      val userClkOpt = domainOwner.meta.annotations.collectFirst {
        case c: constraints.Timing.Clock => c
      }
      val userRstOpt = domainOwner.meta.annotations.collectFirst {
        case r: constraints.Timing.Reset => r
      }
      val isDeviceTop = domainOwner.getThisOrOwnerDesign.isDeviceTop
      // No-reset opt-out: if the user specifies @timing.clock without @timing.reset,
      // that explicitly declares "this group has no reset" — we suppress the default
      // reset that would otherwise be merged in.
      val explicitNoRst = userClkOpt.isDefined && userRstOpt.isEmpty
      val (baseClkOpt, baseRstOpt)
          : (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset]) =
        if (isDeviceTop)
          val clk = domainOwner.getTimingConstraintClkRateOpt match
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
    end getResolvedClkRst

    private def usesClkRst: (usesClk: Boolean, usesRst: Boolean) = domainOwner match
      case design: DFDesignBlock =>
        designUsesClkRst.getOrElseUpdate(
          design.dclName,
          (design.usesClk, design.usesRst)
        )
      case _ =>
        domainOwnerUsesClkRst.getOrElseUpdate(
          domainOwner,
          (domainOwner.usesClk, domainOwner.usesRst)
        )

    private def isAlwaysAtTopClk: Boolean =
      domainOwner.getResolvedClkRst._1 match
        case Some(clk) => clk.inclusionPolicy match
            case ClkRstInclusionPolicy.AlwaysAtTop => true
            case _                                 => false
        case None => false

    private def isAlwaysAtTopRst: Boolean =
      domainOwner.getResolvedClkRst._2 match
        case Some(rst) => rst.inclusionPolicy match
            case ClkRstInclusionPolicy.AlwaysAtTop => true
            case _                                 => false
        case None => false

    // Plan-mandated "forcing" semantics: an explicit `@timing.clock` / `@timing.reset`
    // annotation on the owner counts as "this slot is required" even when no member
    // generates the usage (covers bare-annotation forcing on blackbox / combinational owners
    // and partial-override forms).
    private def hasClkAnnot: Boolean = domainOwner.meta.annotations.exists {
      case _: constraints.Timing.Clock => true
      case _                           => false
    }
    private def hasRstAnnot: Boolean = domainOwner.meta.annotations.exists {
      case _: constraints.Timing.Reset => true
      case _                           => false
    }

    private def usesClk: Boolean = domainOwnerMemberTable(domainOwner).exists {
      case dcl: DFVal.Dcl                      => dcl.isReg || dcl.isClkDcl
      case reg: DFVal.Alias.History            => true
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst.usesClk
      case inst: DFDesignInst                  => inst.getDesignBlock.usesClkRst.usesClk
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst.usesClk) ||
      domainOwner.isTopTop && domainOwner.isAlwaysAtTopClk ||
      domainOwner.hasClkAnnot

    private def usesRst: Boolean = domainOwnerMemberTable(domainOwner).exists {
      case dcl: DFVal.Dcl =>
        (dcl.isReg && dcl.hasNonBubbleInit) || dcl.isRstDcl
      case reg: DFVal.Alias.History            => reg.hasNonBubbleInit
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst.usesRst
      case inst: DFDesignInst                  => inst.getDesignBlock.usesClkRst.usesRst
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst.usesRst) ||
      domainOwner.isTopTop && domainOwner.isAlwaysAtTopRst ||
      domainOwner.hasRstAnnot
  end extension

  extension (
      resolved: (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset])
  )
    private def relaxed(atDomain: DFDomainOwner)
        : (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset]) =
      val (usesClk, usesRst) = atDomain.usesClkRst
      val (clk, rst) = resolved
      val updatedClk = if (usesClk) clk else None
      val updatedRst = if (usesRst) rst else None
      (updatedClk, updatedRst)
  end extension

  @tailrec private def fillDomainMap(
      domains: List[DFDomainOwner],
      stack: List[DFDomainOwner],
      domainMap: mutable.Map[
        DFDomainOwner,
        (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset])
      ]
  ): Unit =
    domains match
      case domain :: rest if domainMap.contains(domain) =>
        fillDomainMap(rest, stack, domainMap)
      case domain :: rest =>
        dependentRTDomainOwners.get(domain) match
          case Some(dependencyDomain) =>
            domainMap.get(dependencyDomain) match
              case Some(dependencyResolved) =>
                domainMap += domain -> dependencyResolved.relaxed(domain)
                fillDomainMap(rest, stack, domainMap)
              case None => fillDomainMap(rest, domain :: stack, domainMap)
            end match
          case _ =>
            val resolved = domain.getResolvedClkRst
            domainMap += domain -> resolved.relaxed(domain)
            fillDomainMap(rest, stack, domainMap)
        end match
      case Nil if stack.nonEmpty => fillDomainMap(domains = stack, Nil, domainMap)
      case _                     =>
    end match
  end fillDomainMap

  lazy val resolvedClkRstMap: Map[
    DFDomainOwner,
    (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset])
  ] =
    val domainMap = mutable.Map.empty[
      DFDomainOwner,
      (Option[constraints.Timing.Clock], Option[constraints.Timing.Reset])
    ]
    val rtDomainOwners = domainOwnerMemberList.view.map(_._1).filter(_.domainType match
      case DomainType.RT => true
      case _             => false).toList
    fillDomainMap(rtDomainOwners, Nil, domainMap)
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
        val explicitRateOpt = resolvedClkRstMap.get(domainOwner)
          .flatMap(_._1)
          .flatMap(_.rate.toOption)
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
          resolvedClkRstMap.get(ownerDomain).flatMap(_._1).flatMap(_.rate.toOption) match
            case Some(rate) =>

              // Get the clock period in picoseconds
              val clockPeriodPs = rate.to_ps.value
              val desc = rate match
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
              |${stack.map(fullNameViaInst).mkString("\n")}
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
    magnetConnectionMap // causes magnet connectivity checks
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

  // Converts an old-style flat DB (internalDBs.empty) to a canonical new-style DB
  // under the "B-pure" shape:
  //   - The root DB is a pure hierarchy container:
  //         members = [], refTable = {}, designBlock = None
  //         internalDBs = ListMap of every design (top first, then descendants)
  //         globalTags and srcFiles preserved as project metadata.
  //   - Every sub-DB (including the top's) has:
  //         members = [globalMembers, designBlock, localMembers]
  //     where `globalMembers` = the closure of globals reachable by the DB's
  //     local refs, `designBlock` = the DB's own top-level design header (as
  //     `Some(d)` AND as a member of the sub-DB's `members`), and
  //     `localMembers` = the DB's direct locals (excluding the designBlock).
  //   - The TOP design lives only in `internalDBs` (as the first entry by
  //     insertion order), no longer at the root level. There is no root-level
  //     duplicate of the top.
  //   - A nested DFDesignBlock is NEVER a member of its parent's sub-DB; only
  //     the DFDesignInst that instantiates it lives in the parent's locals.
  //     The nested block lives exclusively as the `designBlock` (and a member)
  //     of its own sub-DB.
  //   - Globals and DFDesignBlocks are shared across DBs by OBJECT identity.
  //   - Each sub-DB's `refTable` is self-contained for its own refs (refs
  //     emitted by any of its members — including the shared globals and
  //     designBlock).
  //   - Only the root carries `internalDBs` — a flat ListMap of every design
  //     in elaboration order (top first). Sub-DBs themselves have an empty
  //     `internalDBs`; descendant lookups always go through the root.
  //   - Round-trip note: with all globals partitioned per sub-DB by closure,
  //     `newToOld` no longer guarantees global ordering matches the input.
  //     The round-trip check in SanityCheck compares globals as a set.
  def oldToNew: DB =
    if (internalDBs.nonEmpty) return this
    given MemberGetSet = self.getSet
    val topDsn = this.top
    // designOwn(d) = d's own (non-global, non-self, non-nested-block) members
    // in original order. Nested DFDesignBlocks are NOT included here — they
    // become the `designBlock` of their own sub-DB and are reachable from the
    // parent only through their DFDesignInst entries.
    val designOwn = mutable.LinkedHashMap.empty[DFDesignBlock, mutable.ListBuffer[DFMember]]
    designOwn(topDsn) = mutable.ListBuffer.empty
    members.foreach {
      case d: DFDesignBlock => designOwn.getOrElseUpdate(d, mutable.ListBuffer.empty)
      case _                =>
    }
    // Non-top DFDesignBlocks no longer carry their parent in `ownerRef` — it
    // resolves to DFMember.Empty under the new convention. Recover the parent
    // design via the FIRST DFDesignInst (in elaboration order) whose
    // `designRef` targets the block. That parent is the canonical owner of
    // the child's sub-DB in the design tree; any other parents that also
    // instantiate the same block reach it only through their own
    // DFDesignInst, not via a `directChildren` claim.
    val designBlockParent = mutable.LinkedHashMap.empty[DFDesignBlock, DFDesignBlock]
    members.foreach {
      case inst: DFDesignInst =>
        designBlockParent.getOrElseUpdate(inst.designRef.get, inst.getOwnerDesign)
      case _ =>
    }
    members.foreach {
      case _: DFDesignBlock => // nested blocks live in their own sub-DB only
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => // globals handled separately
      case m                                          => designOwn(m.getOwnerDesign) += m
    }
    // parent → ordered list of canonical child DFDesignBlocks. Iteration of
    // `designBlockParent` (a LinkedHashMap) preserves first-inst-encounter
    // order, which matches the elaboration order of the children.
    val parentToChildren =
      mutable.LinkedHashMap.empty[DFDesignBlock, mutable.ListBuffer[DFDesignBlock]]
    designBlockParent.foreach { (child, parent) =>
      parentToChildren.getOrElseUpdate(parent, mutable.ListBuffer.empty) += child
    }
    // All globals in their original elaboration order — used to project each
    // sub-DB's closure back into a deterministic, topological, source-faithful
    // order (elaboration order is itself topological since a global cannot
    // reference a later-defined global).
    val allGlobalsOrdered: List[DFMember] = members.collect {
      case g: DFVal.CanBeGlobal if g.isGlobal => g
    }
    // Compute the closure of globals transitively reachable from a DB's refs.
    // Walks local members' refs; when a ref target is a global, we include it
    // and recurse through its own refs to pick up globals-referenced-by-globals.
    // Non-global intermediaries are NOT included (they belong to their own
    // design's locals), but their refs ARE walked because we iterate all of
    // the design's locals directly. Returns reachable globals in original
    // elaboration order — required because `newToOld` emits a sub-DB's
    // members directly into the flat output, and both `SanityCheck.orderCheck`
    // and code generation depend on stable, topological ordering.
    def globalsClosure(localMembers: Iterable[DFMember]): List[DFMember] =
      val reachable = mutable.Set.empty[DFMember]
      def pull(target: DFMember): Unit = target match
        case g: DFVal.CanBeGlobal if g.isGlobal && !reachable.contains(g) =>
          reachable += g
          g.getRefs.foreach(r => refTable.get(r).foreach(pull))
        case _ =>
      localMembers.foreach { m =>
        m.getRefs.foreach(r => refTable.get(r).foreach(pull))
      }
      allGlobalsOrdered.filter(reachable.contains)
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
    // Build sub-DBs in top-down elaboration order. Sub-DBs themselves have
    // empty `internalDBs` — only the root collects the flat hierarchy. The
    // LinkedHashMap preserves insertion order so the resulting list runs
    // top → top's first child → grandchildren … in elaboration order.
    val subDBs = mutable.LinkedHashMap.empty[DFDesignBlock, DB]
    def buildSubDB(d: DFDesignBlock): DB =
      subDBs.get(d) match
        case Some(db) => db
        case None =>
          val locals = designOwn(d).toList
          // Walk d's own refs in addition to its locals: with nested blocks
          // no longer present in the parent's `localMembers`, globals reached
          // only through a sub-DB's `designBlock` refs would otherwise be
          // missed by every closure that needs them.
          val closure = globalsClosure(d :: locals)
          val dbMembers = closure ::: d :: locals
          val dbRefTable = refsFor(dbMembers)
          val builtForD = DB(
            members = dbMembers,
            refTable = dbRefTable,
            // Sub-DBs inherit globalTags from the root so per-design stage
            // helpers (e.g. resolvedClkRstMap) find project-wide tags
            // like DefaultRTDomainCfgTag when dispatched against a sub-DB.
            globalTags = this.globalTags,
            srcFiles = Nil,
            internalDBs = ListMap.empty,
            designBlock = Some(d)
          )
          // Insert d BEFORE recursing into children so the LinkedHashMap
          // ordering is top-down (parent before children).
          subDBs(d) = builtForD
          val directChildren = parentToChildren.getOrElse(d, Nil).toList
          directChildren.foreach(buildSubDB)
          builtForD
    buildSubDB(topDsn)
    // Orphan globals: any global in the original flat DB that is not reached
    // by any sub-DB's `globalsClosure` (e.g. a global that nothing references).
    // Without explicit handling these would vanish across `oldToNew + newToOld`,
    // because root.members is empty and only sub-DB closures carry globals.
    // Anchor them at the top design's sub-DB so they survive the round-trip.
    val coveredGlobals = mutable.Set.empty[DFMember]
    subDBs.valuesIterator.foreach { sub =>
      sub.members.foreach {
        case g: DFVal.CanBeGlobal if g.isGlobal => coveredGlobals += g
        case _                                  =>
      }
    }
    val orphanGlobals: List[DFMember] = members.collect {
      case g: DFVal.CanBeGlobal if g.isGlobal && !coveredGlobals.contains(g) => g
    }
    if (orphanGlobals.nonEmpty)
      val topSub = subDBs(topDsn)
      val newMembers = orphanGlobals ::: topSub.members
      val orphanRefs = refsFor(orphanGlobals)
      val newRefTable = topSub.refTable ++ orphanRefs
      subDBs(topDsn) = topSub.copy(members = newMembers, refTable = newRefTable)
    // Root is a pure hierarchy container: empty members, empty refTable,
    // designBlock = None. `internalDBs` lists every design — top first (so
    // `topDB` resolves correctly), then the top's descendants in elaboration
    // order.
    DB(
      members = Nil,
      refTable = Map.empty,
      globalTags = this.globalTags,
      srcFiles = this.srcFiles,
      internalDBs = ListMap.from(subDBs.iterator.map((d, sub) => d.ownerRef -> sub)),
      designBlock = None
    )
  end oldToNew

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
  def newToOld: DB =
    if (internalDBs.isEmpty) return this
    // Under B-pure, root has empty `members` and empty `refTable`; all design
    // content lives in sub-DBs. `allDBs` only needs the sub-DBs.
    val allDBs: List[DB] = internalDBs.values.toList
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
    // inst → canonical target DFDesignBlock. Resolved through each sub-DB's
    // own refTable because the root DB's refTable does not necessarily cover
    // refs that originate in sub-DB members.
    val instToDesign = mutable.Map.empty[DFDesignInst, DFDesignBlock]
    allDBs.foreach { db =>
      db.members.foreach {
        case inst: DFDesignInst =>
          db.refTable.get(inst.designRef).foreach {
            case d: DFDesignBlock =>
              instToDesign(inst) = canonicalize(d).asInstanceOf[DFDesignBlock]
            case _ =>
          }
        case _ =>
      }
    }
    val flat = mutable.ListBuffer.empty[DFMember]
    val seen = mutable.Set.empty[DFMember]
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
                internalDBs.get(targetBlock.ownerRef).foreach(sub => emit(sub.members))
            }
            if (!seen.contains(c))
              seen += c
              flat += c
          case _ =>
            if (!seen.contains(c))
              seen += c
              flat += c
              c match
                case d: DFDesignBlock if internalDBs.contains(d.ownerRef) =>
                  emit(internalDBs(d.ownerRef).members)
                case _ =>
        end match
      }
    // B-pure: root has empty `members`. Start emission from the topDB's
    // members. The DFDesignBlock-descent guard inside `emit` prevents the
    // re-entry from looping when topDsn (a member of topDB.members) triggers
    // a recursive `emit(topDB.members)` on its own sub-DB.
    emit(topDB.members)
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

  // Normalizes an old-style flat DB so:
  //   1. globals appear BEFORE `top` in `members` (their mutual order
  //      preserved).
  //   2. each non-top DFDesignBlock (and its contiguous content — its own
  //      locals plus any further nested blocks recursively) is positioned
  //      immediately before its FIRST DFDesignInst in the parent's locals.
  //      Mirrors the form produced by `oldToNew + newToOld` so that
  //      round-trip equality holds against the original elaboration order
  //      (which places a nested block where it was first elaborated, not
  //      where the corresponding inst settles after later stages).
  // Idempotent on already-canonical DBs; a no-op on new-style DBs (those
  // are already canonical).
  def canonicalForm: DB =
    if (internalDBs.nonEmpty) return this
    given MemberGetSet = self.getSet
    val globals = mutable.ListBuffer.empty[DFMember]
    val nonGlobals = mutable.ListBuffer.empty[DFMember]
    members.foreach {
      case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal => globals += dfVal
      case m                                          => nonGlobals += m
    }
    val topD = top
    // Per-design ordered locals (excluding nested DFDesignBlocks). Mirrors
    // the partitioning oldToNew uses — members owned by design D land in
    // designLocals(D) in their original elaboration order.
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
          val target = inst.designRef.get
          if (!emittedDesigns.contains(target))
            emittedDesigns += target
            emitDesign(target)
          out += inst
        case m => out += m
      }
    emittedDesigns += topD
    emitDesign(topD)
    val newMembers = globals.toList ++ out.toList
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
  final lazy val topName: String = designDB.top.dclName
end MemberGetSet

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]
