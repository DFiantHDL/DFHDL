package dfhdl.compiler.patching
import dfhdl.compiler.ir.*
import dfhdl.internals.invert
private final case class ReplacementContext(
    refTable: Map[DFRefAny, DFMember],
    memberTable: Map[DFMember, Set[DFRefAny]],
    memberRepTable: Map[DFMember, List[(DFMember, Patch.Replace.RefFilter)]],
    typeRefRepeats: Map[DFRef.TypeRef, Int]
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
    refTable.map { case (ref, member) =>
      val replacementHistory = memberRepTable.getOrElse(member, List())
      replacementHistory
        .collectFirst {
          case (repMember, Patch.Replace.RefFilter.All) => ref -> repMember
          case (repMember, Patch.Replace.RefFilter.Outside(owner))
              if member.isOutsideOwner(owner) =>
            ref -> repMember
          case (repMember, Patch.Replace.RefFilter.Inside(owner)) if member.isInsideOwner(owner) =>
            ref -> repMember
        }
        .getOrElse(ref -> member)
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
      val purgedRefs = config match
        case Patch.Replace.Config.ChangeRefOnly => Set()
        case _                                  => refFilter(origMember.getRefs.toSet -- keepRefs)
      val (updatedTypeRefRepeats, purgedRefsWithoutRepeatedTypeRefs) =
        getUpdatedTypeRefCount(purgedRefs)
      val updatedRefTable: Map[DFRefAny, DFMember] =
        replacementHistory.foldRight(refTable) { case ((rm, rf), rt) =>
          rf(refs).foldLeft(rt)((rt2, r) => rt2.updated(r, rm))
        } -- purgedRefsWithoutRepeatedTypeRefs
      val updatedMemberRepTable: Map[DFMember, List[(DFMember, Patch.Replace.RefFilter)]] =
        refFilter match
          // An all inclusive filter is purging all other replacement histories, so we only save it alone
          case Patch.Replace.RefFilter.All =>
            memberRepTable + (origMember -> List((latestRepMember, refFilter)))
          case _ =>
            memberRepTable + (origMember -> replacementHistory)
      // add another entry for the replacing member, and still keep the old one, but without the purged references
      val updatedMemberTable = purgedRefs.foldLeft(memberTable) { case (mt, r) =>
        mt.updatedWith(r.get)(SomeRefs => Some(SomeRefs.get - r))
      } + (latestRepMember -> refs)
      ReplacementContext(
        updatedRefTable,
        updatedMemberTable,
        updatedMemberRepTable,
        updatedTypeRefRepeats
      )
    end if
  end replaceMember

  // returns the updated type ref count and the purged refs without repeated type refs
  private def getUpdatedTypeRefCount(
      purgedRefs: Set[DFRefAny]
  ): (Map[DFRef.TypeRef, Int], Set[DFRefAny]) =
    val updatedTypeRefRepeats = purgedRefs.view
      .collect { case ref: DFRef.TypeRef => ref }
      .foldLeft(typeRefRepeats) { case (counts, ref) =>
        counts.get(ref) match
          case Some(count) => counts.updated(ref, count - 1)
          case None        => counts
      }
    val purgedRefsWithoutRepeatedTypeRefs = purgedRefs.filter {
      case ref: DFRef.TypeRef => updatedTypeRefRepeats.get(ref).exists(_ == 0)
      case _                  => true
    }
    (updatedTypeRefRepeats, purgedRefsWithoutRepeatedTypeRefs)
  end getUpdatedTypeRefCount

  def removeMember(origMember: DFMember): ReplacementContext =
    // Total references to be removed are:
    // * refs from memberTable - references from other members to this member
    // * refs from origMember - references from this member to other members
    // Together these cover both directions of two-way references
    val purgedRefs =
      memberTable.getOrElse(origMember, Set()) ++ origMember.getRefs + origMember.ownerRef
    val (updatedTypeRefRepeats, purgedRefsWithoutRepeatedTypeRefs) =
      getUpdatedTypeRefCount(purgedRefs)
    val updatedMemberTable = purgedRefsWithoutRepeatedTypeRefs.foldLeft(memberTable) {
      case (mt, r) =>
        mt.updatedWith(r.get)(SomeRefs => Some(SomeRefs.get - r))
    }
    this.copy(
      refTable = this.refTable -- purgedRefsWithoutRepeatedTypeRefs,
      memberTable = updatedMemberTable,
      typeRefRepeats = updatedTypeRefRepeats
    )
  end removeMember

end ReplacementContext

private object ReplacementContext:
  def fromRefTable(refTable: Map[DFRefAny, DFMember])(using MemberGetSet): ReplacementContext =
    val typeRefRepeats =
      getSet.designDB.members.view
        .flatMap(_.getRefs.view.collect { case ref: DFRef.TypeRef => ref })
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toMap
    ReplacementContext(refTable, refTable.invert, Map(), typeRefRepeats)
