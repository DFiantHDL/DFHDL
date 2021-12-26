package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import scala.reflect.ClassTag

extension (owner: DFOwner)
  def members(memberView: MemberView)(using MemberGetSet): List[DFMember] =
    getSet.getMembersOf(owner, memberView)

extension (member: DFMember)
  def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
    member.tags.getTagOf[CT]

  // does not work?
  def isTaggedWith[CT <: DFTag: ClassTag]: Boolean =
    getTagOf[CT].isDefined
