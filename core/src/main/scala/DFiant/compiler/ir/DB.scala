package DFiant.compiler.ir
import scala.reflect.ClassTag

final case class DB(
    members: List[DFMember],
    refTable: Map[DFMember.Ref, DFMember],
    globalTags: Map[(Any, ClassTag[_]), DFMember.Tag]
)

trait MemberGetSet {
  val designDB: DB
//  def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0
//  def set[M <: DFMember](originalMember : M)(newMemberFunc : M => M) : M
//  def replace[M <: DFMember](originalMember : M)(newMember : M) : M
//  def remove[M <: DFMember](member : M) : M
//  def getMembersOf(owner : DFOwner) : List[DFMember]
//  def setGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any, tag : CT) : Unit
//  def getGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any) : Option[CT]
}
