package dfhdl.compiler.ir

import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{ListMap, ListSet}
import dfhdl.internals.*
import dfhdl.compiler.printing.{Printer, DefaultPrinter}
import DFDesignBlock.InstMode

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRefAny, DFMember],
    globalTags: Map[(Any, ClassTag[?]), DFTag],
    srcFiles: List[SourceFile]
):
  private val self = this
  given getSet: MemberGetSet with
    val designDB: DB = self
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 =
      refTable(ref).asInstanceOf[M0]
    def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0] =
      refTable.get(ref).asInstanceOf[Option[M0]]
    def getOrigin(ref: DFRef.TwoWayAny): DFMember = originRefTable(ref)
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      newMemberFunc(originalMember)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M = newMember
    def remove[M <: DFMember](member: M): M = member
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = {}
    def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT] =
      globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
  end getSet

  // considered to be in simulation if the top design has no ports
  lazy val inSimulation: Boolean = membersNoGlobals.forall {
    case dcl: DFVal.Dcl if dcl.isPort => dcl.getOwnerDesign != top
    case _                            => true
  }

  lazy val portsByName: Map[DFDesignInst, Map[String, DFVal.Dcl]] =
    members.view
      .collect { case m: DFVal.Dcl if m.isPort => m }
      .groupBy(_.getOwnerDesign)
      .map { case (design, dcls) =>
        design -> dcls.map(m => m.getRelativeName(design) -> m).toMap
      }.toMap

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

  lazy val top: DFDesignBlock = membersNoGlobals.head match
    case m: DFDesignBlock => m
    case invalidTop => throw new IllegalArgumentException(s"Unexpected member as Top:\n$invalidTop")

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
        case r =>
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
                  namedDFTypeMap + (dfType -> None) // used in more than one block -> global named type
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

  // holds the topological order of owner owner dependency
  lazy val ownerMemberList: List[(DFOwner, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFOwner](_.getOwner)(List(), membersNoGlobals.drop(1), List(top -> List())).reverse
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
        def recur(owner: DFOwner, includeOwner: Boolean = true): List[DFMember] =
          val members = ownerMemberTable(owner)
          members.flatMap {
            case d: DFDesignBlock => Some(d)
            case o: DFOwner       => if (includeOwner) o :: recur(o) else recur(o)
            case m                => Some(m)
          }
        end recur
        owner match
          case d: DFDesignBlock => designMemberTable(d)
          case _                => recur(owner, includeOwner = false)

  // holds a hash table that lists members of each owner. The member list order is maintained.
  lazy val ownerMemberTable: Map[DFOwner, List[DFMember]] =
    Map(ownerMemberList*)

  // holds the topological order of named owner block dependency
  lazy val namedOwnerMemberList: List[(DFOwnerNamed, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFOwnerNamed](_.getOwnerNamed)(
      List(),
      membersNoGlobals.drop(1),
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
      membersNoGlobals.drop(1),
      List(top -> List())
    ).reverse

  // holds a hash table that lists members of each named owner. The member list order is maintained.
  lazy val domainOwnerMemberTable: Map[DFDomainOwner, List[DFMember]] =
    Map(domainOwnerMemberList*)

  // holds the topological order of owner block dependency
  lazy val blockMemberList: List[(DFBlock, List[DFMember])] =
    // head will always be the TOP owner
    OMLGen[DFBlock](_.getOwnerBlock)(List(), membersNoGlobals.drop(1), List(top -> List())).reverse

  // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val blockMemberTable: Map[DFBlock, List[DFMember]] =
    Map(blockMemberList*)

  // holds the topological order of design block dependency
  lazy val designMemberList: List[(DFDesignBlock, List[DFMember])] =
    // head will always be the TOP block
    OMLGen[DFDesignBlock](_.getOwnerDesign)(
      List(),
      membersNoGlobals.drop(1),
      List(top -> List())
    ).reverse

  // holds the topological order of unique design block dependency
  lazy val uniqueDesignMemberList: List[(DFDesignBlock, List[DFMember])] =
    designMemberList.filterNot(_._1.isDuplicate)

  // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val designMemberTable: Map[DFDesignBlock, List[DFMember]] =
    Map(designMemberList*)

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
            case header: DFConditional.Header => (header, block :: chain)
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
  private def getValAccess(dfVal: DFVal, range: Range, net: DFNet)(
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
            if (connToDcls.contains(dcl, range)) Read
            // otherwise it is unknown
            else Unknown
          // illegal connection
          case _ => Error
      case _: DFVal.OPEN => Unknown
      case _             => Read
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
          case (Read, Read) =>
            newError("Unsupported read-to-read connection.")
            None
          case (Write, Write) =>
            newError("Unsupported write-to-write connection.")
            None
          case (_, Read) => Some(lhsVal)
          case (Read, _) => Some(rhsVal)
          case (Error, _) =>
            newError(s"Unknown access pattern with ${lhsVal.relValString}.")
            None
          case (_, Error) =>
            newError(s"Unknown access pattern with ${rhsVal.relValString}.")
            None
          case _ => None
        var hasOpenConn = false
        val toDclAndRangeOption: Option[(DFVal.Dcl, Range)] = toValOption.flatMap(v =>
          if (v.isOpen)
            hasOpenConn = true
            None
          else
            v.departialDcl match
              case None =>
                newError(s"Unexpected write access to the immutable value ${v.relValString}.")
                None
              case x => x
        )
        toDclAndRangeOption match
          // found target variable or port declaration for the given connection/assignment
          case Some((toDcl, range)) =>
            val prevNets = connToDcls.getNets(toDcl, range)
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
              getConnToDcls(otherNets, pendingNets, connToDcls.addNet(toDcl, range, net), newErrors)
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
    // group magnet ports according to the magnet type
    val magnetDclGroups =
      members.view
        .collect {
          case dcl @ DFVal.Dcl(dfType = dfType: DFOpaque) if dcl.isPort && dfType.isMagnet =>
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
              val sourceInCandidates = dclGrp.filter { port =>
                port.isPortIn && !port.isSameOwnerDesignAs(targetPort) &&
                targetPort.isInsideOwner(port.getOwnerDesign)
              }.map { port =>
                (port, targetDsn.getDistanceFromOwnerDesign(port.getOwnerDesign))
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
              val sourceOutCandidates = dclGrp.filter { port =>
                port.isPortOut && !port.isSameOwnerDesignAs(targetPort) &&
                port.isInsideOwner(targetDsn) ||
                port.isPortIn && port.isSameOwnerDesignAs(targetPort)
              }.map { port =>
                (port, port.getDistanceFromOwnerDesign(targetDsn))
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

  def checkDanglingInputs(): Unit =
    // collect all input ports that are not connected directly or implicitly as magnets
    val danglingInputs = members.collect {
      case p: DFVal.Dcl
          if p.isPortIn && !connectionTable.contains(p) &&
            !p.getOwnerDesign.isTop && !magnetConnectionTable.contains(p) =>
        val ownerDesign = p.getOwnerDesign
        s"""|DFiant HDL connectivity error!
            |Position:  ${ownerDesign.meta.position}
            |Hierarchy: ${ownerDesign.getFullName}
            |Message:   Found a dangling (unconnected) input port `${p.getName}`.""".stripMargin
    }
    if (danglingInputs.nonEmpty)
      throw new IllegalArgumentException(
        danglingInputs.mkString("\n")
      )
  end checkDanglingInputs

  // holds for each RTDomain/RTDesign/RTInterface that its configuration on another domain,
  // the domain it is dependent on
  lazy val dependentRTDomainOwners: Map[DFDomainOwner, DFDomainOwner] =
    extension (member: DFMember)
      def getRTOwnerOption: Option[DFDomainOwner] =
        val owner = member.getOwnerDomain
        owner.domainType match
          case _: DomainType.RT => Some(owner)
          case _                => None
    members.view.flatMap {
      case domainOwner: DFDomainOwner =>
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
                    val domainMembers = domainOwnerMemberTable(domain)
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
      case _ => None
    }
      .toMap
  end dependentRTDomainOwners

  // mapping designs and their uses of Clk/Rst
  // we use the design declaration name (after enforcing uniqueness) because designs
  // can be empty duplicates and the indications need to come from the full designs
  //                                       dclName  usesClk  usesRst
  private lazy val designUsesClkRst = mutable.Map.empty[String, (Boolean, Boolean)]
  //                                                            usesClk  usesRst
  private lazy val domainOwnerUsesClkRst = mutable.Map.empty[DFDomainOwner, (Boolean, Boolean)]
  private lazy val reversedDependents = dependentRTDomainOwners.invert

  extension (domainOwner: DFDomainOwner)
    private def getExplicitCfg: RTDomainCfg.Explicit =
      domainOwner.domainType match
        case DomainType.RT(explicitCfg: RTDomainCfg.Explicit) => explicitCfg
        case _ => top.getTagOf[RTDomainCfg.Explicit].get
    private def usesClkRst: (Boolean, Boolean) = domainOwner match
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
      case dcl: DFVal.Dcl                      => dcl.modifier.isReg || dcl.isClkDcl
      case reg: DFVal.Alias.History            => true
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst._1
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst._1) ||
      domainOwner.isTop && (domainOwner.getExplicitCfg.clkCfg match
        case ClkCfg.Explicit(inclusionPolicy = ClkRstInclusionPolicy.AlwaysAtTop) => true
        case _                                                                    => false)

    private def usesRst: Boolean = domainOwnerMemberTable(domainOwner).exists {
      case dcl: DFVal.Dcl =>
        (dcl.modifier.isReg && dcl.hasNonBubbleInit) || dcl.isRstDcl
      case reg: DFVal.Alias.History            => reg.hasNonBubbleInit
      case pb: ProcessBlock if pb.isInRTDomain => true
      case internal: DFDesignBlock             => internal.usesClkRst._2
      case _                                   => false
    } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst._2) ||
      domainOwner.isTop && (domainOwner.getExplicitCfg.rstCfg match
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
      trigger.getConstData match
        case Some((waitValue: BigDecimal, waitUnit: DFPhysical.Unit.Time.Scale)) =>
          // Check if the wait statement is in a domain with a clock rate configuration
          explicitRTDomainCfgMap.get(ownerDomain) match
            case Some(RTDomainCfg.Explicit(_, clkCfg: ClkCfg.Explicit, _)) =>
              // Get the clock period in picoseconds
              val (clockPeriodPs: BigDecimal, desc: String) = clkCfg.rate.getConstData.get match
                case (value: BigDecimal, unit: DFPhysical.Unit.Time.Scale) =>
                  // Direct period specification
                  (unit.to_ps(value), s"period ${value}.${unit}")
                case (value: BigDecimal, unit: DFPhysical.Unit.Freq.Scale) =>
                  // Frequency specification - convert to period
                  (unit.to_ps(value), s"frequency ${value}.${unit}")
              // Get wait duration in picoseconds
              val waitDurationPs = waitUnit.to_ps(waitValue)

              // Check if wait duration is exactly divisible by clock period
              if (waitDurationPs % clockPeriodPs != 0)
                waitError(
                  s"Wait duration ${waitValue}.${waitUnit} is not exactly divisible by the clock $desc."
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
    val anonErrorMemberPositions: Set[Position] = membersNoGlobals.drop(1).view.collect {
      case dcl: DFVal.Dcl if dcl.isAnonymous => dcl
      // designs cannot be anonymous, but design definitions are allowed to be anonymous
      case dsn: DFDesignBlock if dsn.isAnonymous && dsn.instMode != InstMode.Def => dsn
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
        case m =>
          val isDesignParam = m match
            case _: DFVal.DesignParam => true
            case _                    => false
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
            case refMember =>
              m match
                // design parameters are expected to reference values from their parent design
                // or from the same design for default parameter values
                case dp: DFVal.DesignParam =>
                  if (refMember.isSameOwnerDesignAs(dp) && dp.defaultRef.get == refMember) None
                  else if (m.isOneLevelBelow(refMember)) None
                  else Some(refMember)
                // the rest must be in the same design
                case _ if !refMember.isSameOwnerDesignAs(m) => Some(refMember)
                case _                                      => None
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

  def check(): Unit =
    nameCheck()
    connectionTable // causes connectivity checks
    magnetConnectionTable // causes magnet connectivity checks
    checkDanglingInputs()
    directRefCheck()
    circularDerivedDomainsCheck()
    waitCheck()

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

end DB

//object DB:
//end DB

enum MemberView derives CanEqual:
  case Folded, Flattened

trait MemberGetSet:
  def designDB: DB
  def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0
  def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0]
  def getOrigin(ref: DFRef.TwoWayAny): DFMember
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member: M): M
  def setGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any, tag: CT): Unit
  def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT]

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]
