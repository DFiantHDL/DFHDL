package DFiant.compiler.ir

import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec
import scala.collection.mutable
import DFiant.internals.*
import DFiant.compiler.printing.{Printer, DefaultPrinter}

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRefAny, DFMember],
    globalTags: Map[(Any, ClassTag[_]), DFTag],
    srcFiles: List[SourceFile]
):
  private val self = this
  given getSet: MemberGetSet with
    def designDB: DB = self
    def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0 =
      refTable(ref).asInstanceOf[M0]
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      newMemberFunc(originalMember)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M = newMember
    def remove[M <: DFMember](member: M): M = member
    def getMembersOf(owner: DFOwner, memberView: MemberView): List[DFMember] =
      memberView match
        case MemberView.Folded =>
          ownerMemberTable(owner)
        case MemberView.Flattened =>
          owner match
            case d: DFDesignBlock => designMemberTable(d)
            case b: DFBlock       => blockMemberTable(b)
            case _                => ownerMemberTable(owner)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = {}
    def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT] =
      globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
  end getSet

  lazy val top: DFDesignBlock = members.head match
    case m: DFDesignBlock => m
    case _                => throw new IllegalArgumentException("Unexpected member as Top.")

  lazy val memberTable: Map[DFMember, Set[DFRefAny]] = refTable.invert

  // Map of all named types in the design with their design block owners.
  // If the named type is global (used in IO or more than one design block),
  // then its owner is set to None.
  private lazy val namedDFTypes: Map[NamedDFType, Option[DFDesignBlock]] =
    members.foldLeft(Map.empty[NamedDFType, Option[DFDesignBlock]]) {
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
              case None =>
                namedDFTypeMap + (dfType -> Some(
                  namedDFTypeMember.getOwnerDesign
                )) // found new named type
          }
      case (namedDFTypeMap, _) => namedDFTypeMap // not a named type member
    }

  private lazy val invertedNamedDFTypes = namedDFTypes.invert
  lazy val getGlobalNamedDFTypes: Set[NamedDFType] = invertedNamedDFTypes.getOrElse(None, Set())
  private lazy val localNamedDFTypes: Map[DFDesignBlock, Set[NamedDFType]] =
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
    OMLGen[DFOwner](_.getOwner)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse // head will always be the TOP owner
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

    // holds a hash table that lists members of each owner. The member list order is maintained.
  lazy val ownerMemberTable: Map[DFOwner, List[DFMember]] =
    Map(ownerMemberList: _*)

  // holds the topological order of named owner block dependency
  lazy val namedOwnerMemberList: List[(DFOwnerNamed, List[DFMember])] =
    OMLGen[DFOwnerNamed](_.getOwnerNamed)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse // head will always be the TOP owner

      // holds a hash table that lists members of each named owner. The member list order is maintained.
  lazy val namedOwnerMemberTable: Map[DFOwnerNamed, List[DFMember]] =
    Map(namedOwnerMemberList: _*)

    // holds the topological order of owner block dependency
  lazy val blockMemberList: List[(DFBlock, List[DFMember])] =
    OMLGen[DFBlock](_.getOwnerBlock)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse // head will always be the TOP block

      // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val blockMemberTable: Map[DFBlock, List[DFMember]] =
    Map(blockMemberList: _*)

  // holds the topological order of design block dependency
  lazy val designMemberList: List[(DFDesignBlock, List[DFMember])] =
    OMLGen[DFDesignBlock](_.getOwnerDesign)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse // head will always be the TOP block

      // holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val designMemberTable: Map[DFDesignBlock, List[DFMember]] =
    Map(designMemberList: _*)

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
  private def getValAccess(dfVal: DFVal, net: DFNet)(connToDcls: Map[DFVal.Dcl, DFNet]): Access =
    def isExternalConn =
      if (net.isLateConnection) dfVal isSameOwnerDesignAs net
      else dfVal.getOwnerDesign isSameOwnerDesignAs net
    def isInternalConn =
      if (net.isLateConnection) net.getOwnerDesign isSameOwnerDesignAs dfVal
      else dfVal isSameOwnerDesignAs net
    dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier match
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
          case VAR | WIRE | (_: REG) if isInternalConn =>
            // if already was connected as write, then it must be read
            if (connToDcls.contains(dcl)) Read
            // otherwise it is unknown
            else Unknown
          // illegal connection
          case _ => Error
      case _ => Read
    end match
  end getValAccess
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
      val lhsMembers = getSet.getMembersOf(lhsIfc, MemberView.Folded)
      val rhsMembers = getSet.getMembersOf(rhsIfc, MemberView.Folded)
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
      connToDcls: Map[DFVal.Dcl, DFNet],
      errors: List[String]
  ): Map[DFVal.Dcl, DFNet] =
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
        val owner = net.ownerRef.get
        val (lhsAccess, rhsAccess) = net.op match
          // assignment is always from right to left
          case Assignment => (Write, Read)
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
        val toDclOption = toValOption.flatMap(v =>
          val dclOpt = v.dealias
          if (dclOpt.isEmpty)
            newError(s"Unexpected write access to the immutable value ${v.relValString}.")
          dclOpt
        )
        toDclOption match
          // found target variable or port declaration for the given connection/assignment
          case Some(toDcl) =>
            connToDcls.get(toDcl) match
              // already has assignments, but multiple assignments are allowed
              case Some(prevNet) if prevNet.isAssignment && net.isAssignment =>
              // previous net is either a connection or an assignment
              case Some(prevNet) =>
                newError(
                  s"""Multiple connections write to the same variable/port ${toDcl.getFullName}.
                     |The previous write occured at ${prevNet.meta.position}""".stripMargin
                )
              // no previous connection is OK
              case None =>
            getConnToDcls(otherNets, pendingNets, connToDcls + (toDcl -> net), newErrors)
          // unable to determine net directionality, so move net to pending
          case None =>
            getConnToDcls(otherNets, flatNet :: pendingNets, connToDcls, newErrors)
        end match
      case Nil if errors.nonEmpty =>
        throw new IllegalArgumentException(
          errors.view.reverse.mkString("\n\n")
        )
      case Nil if pendingNets.nonEmpty =>
        val reexamine = pendingNets.exists {
          case n
              if n.lhsVal.dealias
                .exists(connToDcls.contains) | n.rhsVal.dealias.exists(connToDcls.contains) =>
            true
          case _ => false
        }
        if (reexamine) getConnToDcls(pendingNets, Nil, connToDcls, errors)
        else
          throw new IllegalArgumentException(
            s"""DFiant HDL connectivity errors!
               |Unable to determine directionality for the following nets:
               |${pendingNets.map(_.net.meta.position).mkString("\n")}""".stripMargin
          )
      case Nil => connToDcls
    end match
  end getConnToDcls

  // There can only be a single connection to a value (but multiple assignments are possible)
  //                               To       Via
  lazy val connectionTable: Map[DFVal.Dcl, DFNet] =
    val flatNets = members.flatMap {
      case net: DFNet => FlatNet(net)
      case _          => Nil
    }
    getConnToDcls(flatNets, Nil, Map(), Nil).filter(_._2.isConnection)

  //                                    From       Via
  lazy val connectionTableInverted: Map[DFVal, List[DFNet]] =
    members
      .collect { case n @ DFNet.Connection(_: DFVal, fromVal: DFVal, _) => (fromVal, n) }
      .groupMap(_._1)(_._2)

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
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member: M): M
  def getMembersOf(owner: DFOwner, memberView: MemberView): List[DFMember]
  def setGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any, tag: CT): Unit
  def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT]

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]
