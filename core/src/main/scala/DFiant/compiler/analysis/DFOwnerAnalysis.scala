package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import scala.reflect.ClassTag

extension (owner: DFOwner)
  def members(memberView: MemberView)(using MemberGetSet): List[DFMember] =
    getSet.getMembersOf(owner, memberView)
  def getVeryLastMember(using getSet: MemberGetSet): Option[DFMember] =
    import getSet.designDB
    val last = owner match
      case block: DFDesignBlock => designDB.designMemberTable(block).lastOption
      case block: DFBlock       => designDB.blockMemberTable(block).lastOption
      case _                    => designDB.ownerMemberTable(owner).lastOption
    last match
      // if last member is an owner then we search further
      case Some(o: DFOwner) =>
        o.getVeryLastMember match
          case None => Some(o) // found empty owner as last member ==> return the owner
          case x    => x // return the very last member
      case x => x
end extension

extension (member: DFMember)
  def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
    member.tags.getTagOf[CT]
  // does not work?
  def contains[CT <: DFTag: ClassTag]: Boolean =
    member.tags.contains[CT]
