package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import scala.collection.mutable
extension (owner: DFOwner)
  def members(memberView: MemberView)(using MemberGetSet): List[DFMember] =
    getSet.designDB.getMembersOf(owner, memberView)
  def getVeryLastMember(using getSet: MemberGetSet): Option[DFMember] =
    import getSet.designDB
    val last = owner match
      case block: DFDesignBlock => designDB.designMemberTable(block).lastOption
      case block: DFBlock       => designDB.blockMemberTable(block).lastOption
    last match
      // if last member is an owner then we search further
      case Some(o: DFOwner) =>
        o.getVeryLastMember match
          case None => Some(o) // found empty owner as last member ==> return the owner
          case x    => x // return the very last member
      case x => x
end extension

extension (design: DFDesignBlock)
  // collect all local parameters that are used in IOs
  def getIOLocalParams(using getSet: MemberGetSet): List[DFVal.CanBeExpr] =
    val ioLocalParams = mutable.LinkedHashSet.empty[DFVal.CanBeExpr]
    def collectIOLocalParams(ref: DFRefAny): Unit = ref.get match
      // skip existing design parameters
      case _: DFVal.DesignParam => // do nothing
      // skip global parameters
      case gp: DFVal.CanBeGlobal if gp.isGlobal => // do nothing
      // check this value and its dependencies
      case dfVal: DFVal.CanBeExpr =>
        // already collected
        if (!ioLocalParams.contains(dfVal))
          // collect dependencies
          dfVal.getRefs.foreach(collectIOLocalParams)
          // if the value is named collect this value too
          if (!dfVal.isAnonymous)
            ioLocalParams.add(dfVal)
      case _ => // do nothing
    design.members(MemberView.Folded).foreach {
      case port @ DclPort() => port.getRefs.foreach(collectIOLocalParams)
      case _                => // do nothing
    }
    ioLocalParams.toList
end extension
