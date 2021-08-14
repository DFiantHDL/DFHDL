package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}
import scala.annotation.tailrec

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRef, DFMember],
    globalTags: Map[(Any, ClassTag[_]), DFTag]
):
  private val self = this
  given getSet: MemberGetSet with
    val designDB: DB = self
    def apply[M <: DFMember, M0 <: M](ref: DFRef.Of[M]): M0 =
      refTable(ref).asInstanceOf[M0]
    def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M =
      newMemberFunc(originalMember)
    def replace[M <: DFMember](originalMember: M)(newMember: M): M = newMember
    def remove[M <: DFMember](member: M): M = member
    def getMembersOf(owner: DFOwner): List[DFMember] = ownerMemberTable(owner)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = {}
    def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT] =
      globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]

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
      //current member indeed belongs to current owner
      case m :: mList if getOwnerFunc(m) == localOwner =>
        val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
        m match
          //Deep borrowing into block as the new owner
          case o: O if classTag[O].runtimeClass.isInstance(o) =>
            val updatedStack2 = (o -> List()) :: updatedStack1
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack2)
          //Just a member
          case _ =>
            OMLGen[O](getOwnerFunc)(oml, mList, updatedStack1)
      //current member does not belong to current owner
      case x :: xs =>
        val updatedOML = (localOwner -> localMembers.reverse) :: oml
        OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
      case Nil if updatedStack0.nonEmpty =>
        val updatedOML = (localOwner -> localMembers.reverse) :: oml
        OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
      case Nil =>
        (localOwner -> localMembers.reverse) :: oml
  end OMLGen

  //holds the topological order of owner owner dependency
  lazy val ownerMemberList: List[(DFOwner, List[DFMember])] =
    OMLGen[DFOwner](_.getOwner)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse //head will always be the TOP owner
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

  //holds a hash table that lists members of each owner owner. The member list order is maintained.
  lazy val ownerMemberTable: Map[DFOwner, List[DFMember]] =
    Map(ownerMemberList: _*)

  //holds the topological order of owner block dependency
  lazy val blockMemberList: List[(DFBlock, List[DFMember])] =
    OMLGen[DFBlock](_.getOwnerBlock)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse //head will always be the TOP block

  //holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val blockMemberTable: Map[DFBlock, List[DFMember]] =
    Map(blockMemberList: _*)

  //holds the topological order of design block dependency
  lazy val designMemberList: List[(DFDesignBlock, List[DFMember])] =
    OMLGen[DFDesignBlock](_.getOwnerDesign)(
      List(),
      members.drop(1),
      List(top -> List())
    ).reverse //head will always be the TOP block

  //holds a hash table that lists members of each owner block. The member list order is maintained.
  lazy val designMemberTable: Map[DFDesignBlock, List[DFMember]] =
    Map(designMemberList: _*)

end DB

//object DB:
//end DB

trait MemberGetSet:
  val designDB: DB
  def apply[M <: DFMember, M0 <: M](ref: DFRef.Of[M]): M0
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member: M): M
  def getMembersOf(owner: DFOwner): List[DFMember]
  def setGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any, tag: CT): Unit
  def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT]

def getSet(using MemberGetSet): MemberGetSet = summon[MemberGetSet]