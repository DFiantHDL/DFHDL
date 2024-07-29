package dfhdl.compiler.patching
import dfhdl.compiler.ir.*
import dfhdl.internals.invert
private final case class ReplacementContext(
    refTable: Map[DFRefAny, DFMember],
    memberTable: Map[DFMember, Set[DFRefAny]],
    memberRepTable: Map[DFMember, List[(DFMember, Patch.Replace.RefFilter)]]
)(using getSet: MemberGetSet):
  def changeRef(origRef: DFRefAny, updateMember: DFMember): ReplacementContext =
    //        println("changeRef", origRef, updateMember)
    copy(refTable = refTable.updated(origRef, updateMember))

  def getLatestRepOf[M <: DFMember](member: M): M =
    memberRepTable.get(member) match
      case Some((repMember, refFilter) :: _) =>
        assert(refFilter == Patch.Replace.RefFilter.All)
        repMember.asInstanceOf[M]
      case _ => member.asInstanceOf[M]

  def getUpdatedRefTable(refTable: Map[DFRefAny, DFMember]): Map[DFRefAny, DFMember] =
    refTable.map {
      case (ref: DFRef.TwoWayAny, member) =>
        val replacementHistory = memberRepTable.getOrElse(member, List())
        replacementHistory
          .collectFirst {
            case (repMember, Patch.Replace.RefFilter.All) => ref -> repMember
            case (repMember, Patch.Replace.RefFilter.Outside(owner))
                if member.isOutsideOwner(owner) =>
              ref -> repMember
            case (repMember, Patch.Replace.RefFilter.Inside(owner))
                if member.isInsideOwner(owner) =>
              ref -> repMember
          }
          .getOrElse(ref -> member)
      case x => x
    }

  def replaceMember(
      origMember: DFMember,
      repMember: DFMember,
      config: Patch.Replace.Config,
      refFilter: Patch.Replace.RefFilter,
      keepRefs: List[DFRefAny]
  ): ReplacementContext =
    if (origMember == repMember) this // nothing to do if the member is replacing itself
    else
      val refs = memberTable.getOrElse(origMember, Set())
      val latestRepMember = getLatestRepOf(repMember)
      // in case the replacement member already was replaced in the past, then we used the previous replacement
      // as the most updated member
      val replacementHistory =
        (latestRepMember, refFilter) :: this.memberRepTable.getOrElse(repMember, List())
      // when replacing a member, the original member refs are redundant unless
      // it's being replaced by a copy of itself that has the same references
      val droppedOrigRefsTable = config match
        case Patch.Replace.Config.ChangeRefOnly => refTable
        case _ => refTable -- refFilter(origMember.getRefs.toSet -- keepRefs)
      val updatedRefTable: Map[DFRefAny, DFMember] =
        replacementHistory.foldRight(droppedOrigRefsTable) { case ((rm, rf), rt) =>
          rf(refs).foldLeft(rt)((rt2, r) => rt2.updated(r, rm))
        }
      val updatedMemberRepTable: Map[DFMember, List[(DFMember, Patch.Replace.RefFilter)]] =
        refFilter match
          // An all inclusive filter is purging all other replacement histories, so we only save it alone
          case Patch.Replace.RefFilter.All =>
            memberRepTable + (origMember -> List((latestRepMember, refFilter)))
          case _ =>
            memberRepTable + (origMember -> replacementHistory)
      // add another entry for the replacing member, but still keep the old one
      val updatedMemberTable = memberTable + (latestRepMember -> refs)
      ReplacementContext(updatedRefTable, updatedMemberTable, updatedMemberRepTable)
    end if
  end replaceMember

  def removeMember(origMember: DFMember): ReplacementContext =
    // total references to be removed are both
    // * refs - directly referencing the member
    // * originRefs - the member is referencing other members with a two-way
    //                reference that points back to it.
    val totalRefs =
      memberTable.getOrElse(origMember, Set()) ++ origMember.getRefs.filter {
        case _: DFRef.TypeRef => false
        case _                => true
      }
    this.copy(refTable = this.refTable -- totalRefs)

end ReplacementContext

private object ReplacementContext:
  def fromRefTable(refTable: Map[DFRefAny, DFMember])(using MemberGetSet): ReplacementContext =
    ReplacementContext(refTable, refTable.invert, Map())
