package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import scala.reflect.ClassTag

extension (owner: DFOwner)
  def members(using MemberGetSet): List[DFMember] = getSet.getMembersOf(owner)

extension (member: DFMember)
  def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
    member.tags.getTagOf[CT]
