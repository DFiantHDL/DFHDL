package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

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
    def getMembersOf(owner: DFOwner): List[DFMember] =
      ??? //ownerMemberTable(owner)
    def setGlobalTag[CT <: DFTag: ClassTag](
        taggedElement: Any,
        tag: CT
    ): Unit = {}
    def getGlobalTag[CT <: DFTag: ClassTag](taggedElement: Any): Option[CT] =
      globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
end DB

object DB:
end DB

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
