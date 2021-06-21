package DFiant.compiler.ir
import scala.reflect.ClassTag

final case class DB(
    members: List[DFMember],
    refTable: Map[DFRef, DFMember],
    globalTags: Map[(Any, ClassTag[_]), DFTag]
):
  private val self = this
//  given getSet: MemberGetSet with
//    val designDB: DB = self
end DB

object DB:
end DB

trait MemberGetSet: 
  val designDB: DB
  def apply[M <: DFMember, M0 <: M](ref : DFRef.Of[M]) : M0
  def set[M <: DFMember](originalMember: M)(newMemberFunc: M => M): M
  def replace[M <: DFMember](originalMember: M)(newMember: M): M
  def remove[M <: DFMember](member : M) : M
  def getMembersOf(owner : DFOwner) : List[DFMember]
  def setGlobalTag[CT <: DFTag : ClassTag](taggedElement : Any, tag : CT) : Unit
  def getGlobalTag[CT <: DFTag : ClassTag](taggedElement : Any) : Option[CT]
