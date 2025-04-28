package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import scala.annotation.tailrec
extension (owner: DFOwner)
  def members(memberView: MemberView)(using MemberGetSet): List[DFMember] =
    getSet.designDB.getMembersOf(owner, memberView)
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

extension (domainOwner: DFDomainOwner)
  // true if the domainOwner is dependent at any level of thatDomainOwner's configuration
  @tailrec def isDependentOn(thatDomainOwner: DFDomainOwner)(using getSet: MemberGetSet): Boolean =
    getSet.designDB.dependentRTDomainOwners.get(domainOwner) match
      case Some(dependency) =>
        if (dependency == thatDomainOwner) true
        else dependency.isDependentOn(thatDomainOwner)
      case None => false
