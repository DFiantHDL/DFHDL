package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*

extension (owner : DFOwner)
  def members(using MemberGetSet) : List[DFMember] = getSet.getMembersOf(owner)