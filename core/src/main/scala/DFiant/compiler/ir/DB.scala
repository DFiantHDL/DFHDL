package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec
import scala.collection.mutable

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRefAny, DFMember],
    globalTags: Map[(Any, ClassTag[_]), DFTag]
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
          owner match
            case d: DFDesignBlock => designMemberTable(d)
            case b: DFBlock       => blockMemberTable(b)
            case _                => ownerMemberTable(owner)
        case MemberView.Flattened => ownerMemberTable(owner)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = {}
    def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT] =
      globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
  end getSet

  lazy val top: DFDesignBlock = members.head match
    case m: DFDesignBlock => m
    case _ => throw new IllegalArgumentException("Unexpected member as Top.")

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

  // holds a hash table that lists members of each owner owner. The member list order is maintained.
  lazy val ownerMemberTable: Map[DFOwner, List[DFMember]] =
    Map(ownerMemberList: _*)

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

  private def ifChainGen: Map[DFIfElseBlock, List[DFIfElseBlock]] =
    val handled = mutable.Set.empty[DFIfElseBlock]
    members.foldRight(Map.empty[DFIfElseBlock, List[DFIfElseBlock]]) {
      case (m: DFIfElseBlock, chainMap) if !handled.contains(m) =>
        @tailrec def getChain(
            ifBlock: DFIfElseBlock,
            chain: List[DFIfElseBlock]
        ): List[DFIfElseBlock] =
          handled += ifBlock
          if (ifBlock.prevBlockRef.isEmpty) ifBlock :: chain
          else getChain(ifBlock.prevBlockRef.get, ifBlock :: chain)
        val chain = getChain(m, Nil)
        chainMap + (chain.head -> chain)
      case (_, chainMap) => chainMap
    }
  end ifChainGen
  // Maps the head IfElse with the entire branch chain (including the head)
  lazy val ifChainTable: Map[DFIfElseBlock, List[DFIfElseBlock]] = ifChainGen
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
