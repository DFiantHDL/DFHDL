package dfhdl.compiler
package analysis
import ir.*
import dfhdl.internals.*
import scala.annotation.tailrec

object StateAnalysis:
  @tailrec private def consumeFrom(
      value: DFVal,
      slice: Slice,
      assignMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet, DFBlock): Set[DFVal] =
    val currentBlock = summon[DFBlock]
    value match
      case dfVal: DFVal.Alias if dfVal.relValRef.get.dfType.isUnbounded => currentSet
      case DFVal.Alias.AsIs(dfType = toType, relValRef = relValRef)     =>
        val relVal = relValRef.get
        val widthsEqual = (toType.widthIntOpt, relVal.dfType.widthIntOpt) match
          case (Some(a), Some(b)) => a == b
          case _                  => false
        if (widthsEqual)
          // casting maintains relative bit consumption
          consumeFrom(relVal, slice, assignMap, currentSet)
        else
          // conversion is treated like any function argument and restarts bit consumption
          consumeFrom(
            relVal,
            Slice.fromWidthOpt(relVal.dfType.widthIntOpt),
            assignMap,
            currentSet
          )
      case applyRange @ DFVal.Alias.ApplyRange(
            relValRef = relValRef,
            idxHighRef = idxHighRef,
            idxLowRef = idxLowRef
          ) =>
        // Re-seed the slice to the ApplyRange's full extent in the parent's coordinates.
        // This replicates the pre-existing behavior where the passed-in slice would be
        // replaced by the ApplyRange's own span when encountered.
        val newSlice: Slice = (
          idxHighRef.getIntOpt,
          idxLowRef.getIntOpt,
          applyRange.elementWidthIntOpt
        ) match
          case (Some(idxHigh), Some(idxLow), Some(eW)) =>
            val start = idxLow * eW
            val len = (idxHigh - idxLow) * eW + 1
            Slice.Concrete(Range(start, start + len))
          case _ => Slice.Unknown
        consumeFrom(relValRef.get, newSlice, assignMap, currentSet)
      case DFVal.Alias.ApplyIdx(relValRef = relValRef, relIdx = idxRef) =>
        // For simplification, consuming the entirety of selection index and array
        val rvSet = consumeFrom(relValRef.get, assignMap, currentSet)
        val idxSet = consumeFrom(idxRef.get, assignMap, currentSet)
        (rvSet union idxSet)
      case DFVal.Alias.SelectField(
            dfType = dfType: DFStruct,
            relValRef = relValRef,
            fieldName = fieldName
          ) =>
        //        var rbh = dfType.width - 1
        //        dfType.fieldMap.foreach { case (n, t) =>
        //          val rbl = rbh - t.width + 1
        //          if (n == fieldName)
        //          rbh = rbl - 1
        //        }
        ???
      case IteratorDcl() => currentSet
      // out ports of child designs are not consuming state within the current design
      case dcl @ DclOut()
          if dcl.getOwnerDesign.isOneLevelBelow(currentBlock.getThisOrOwnerDesign) => currentSet
      case dcl: DFVal.Dcl if (dcl.isPortOut || dcl.isVar) && !dcl.isReg && !dcl.isInProcess =>
        value.getConnectionTo match
          case Some(DFNet.Connection(_, fromVal: DFVal, _)) =>
            consumeFrom(fromVal, slice, assignMap, currentSet)
          case _ =>
            val scope = assignMap(value)
            scope.contains(slice, value.dfType.widthIntOpt) match
              case Tri.Yes => currentSet
              case _       => currentSet + value
      case _ => currentSet
    end match
  end consumeFrom

  private def consumeFrom(
      value: DFVal,
      assignMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet, DFBlock): Set[DFVal] =
    value.dfType match
      case _: DFUnbounded => currentSet
      case _              =>
        consumeFrom(value, Slice.fromWidthOpt(value.dfType.widthIntOpt), assignMap, currentSet)

  @tailrec private def assignTo(
      value: DFVal,
      slice: Slice,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    value match
      case DFVal.Alias.AsIs(relValRef = relValRef) =>
        assignTo(relValRef.get, slice, assignMap)
      case applyRange @ DFVal.Alias.ApplyRange(
            relValRef = relValRef,
            idxLowRef = idxLowRef
          ) =>
        val newSlice: Slice = (idxLowRef.getIntOpt, applyRange.elementWidthIntOpt) match
          case (Some(idxLow), Some(eW)) => slice.shift(idxLow * eW)
          case _                        => Slice.Unknown
        assignTo(relValRef.get, newSlice, assignMap)
      case DFVal.Alias.ApplyIdx(relValRef = relValRef, relIdx = idxRef) =>
        // for simplification, assigning the entirety of the array
        assignTo(relValRef.get, assignMap)
      case DFVal.Alias.SelectField(
            dfType = dfType: DFStruct,
            relValRef = relValRef,
            fieldName = fieldName
          ) =>
        ???
      case x => assignMap.assignTo(x, slice)
    end match
  end assignTo

  private def assignTo(
      value: DFVal,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    assignTo(value, Slice.fromWidthOpt(value.dfType.widthIntOpt), assignMap)

  // retrieves a list of variables that are consumed as their implicit previous value.
  // the assignment stack map is pushed on every conditional block entry and popped on the block exit
  // `analysisRoot` is the design block at which the iteration began — recursion
  // past this block is suppressed so that sub-DB analyses do not cross into
  // parent designs whose members are absent from the current scope.
  @tailrec final def getImplicitStateVars(
      remaining: List[DFMember],
      currentBlock: DFBlock,
      analysisRoot: DFDesignBlock,
      scopeMap: AssignMap,
      currentSet: Set[DFVal],
      checkedDomain: DomainType => Boolean
  )(using MemberGetSet): (Set[DFVal], AssignMap) =
    given DFBlock = currentBlock
    remaining match
      case (nextBlock: DFBlock) :: rs if nextBlock.getOwnerBlock == currentBlock => // entering child block
        val (updatedSet, updatedScopeMap): (Set[DFVal], AssignMap) = nextBlock match
          case cb: DFConditional.Block =>
            cb.guardRef.get match
              case dfVal: DFVal => consumeFrom(dfVal, scopeMap, currentSet)
              case _            => // do nothing
            (currentSet, scopeMap.branchEntry(cb.isFirstCB))
          case _ =>
            (currentSet, scopeMap)
        getImplicitStateVars(rs, nextBlock, analysisRoot, updatedScopeMap, updatedSet,
          checkedDomain)
      case r :: rs
          if r.getOwnerBlock == currentBlock && checkedDomain(
            currentBlock.getThisOrOwnerDomain.domainType
          ) => // checking member consumers
        val (updatedSet, updatedScopeMap): (Set[DFVal], AssignMap) = r match
          case net @ DFNet.Assignment(toVal, fromVal) =>
            (consumeFrom(fromVal, scopeMap, currentSet), assignTo(toVal, scopeMap))
          case net @ DFNet.Connection(toVal: DFVal, fromVal: DFVal, _) =>
            (consumeFrom(fromVal, scopeMap, currentSet), assignTo(toVal, scopeMap))
          case func: DFVal.Func =>
            val args = func.args.map(a => consumeFrom(a.get, scopeMap, currentSet))
            (args.reduce(_ union _), scopeMap)
          case textOut: TextOut =>
            val args = textOut.getRefs.view
              .collect { case DFRef(v: DFVal) => v }
              .map(a => consumeFrom(a, scopeMap, currentSet))
            if (args.nonEmpty) (args.reduce(_ union _), scopeMap)
            else (currentSet, scopeMap)
          case matchBlock: DFConditional.DFMatchHeader =>
            (consumeFrom(matchBlock.selectorRef.get, scopeMap, currentSet), scopeMap)
          case outPort @ DclOut() =>
            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          case anyVar @ DclVar() =>
            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ =>
            (currentSet, scopeMap)
        getImplicitStateVars(rs, currentBlock, analysisRoot, updatedScopeMap, updatedSet,
          checkedDomain)
      case _ => // exiting child block or no more members
        val updatedSet = currentBlock match
          case d: DFDesignBlock if remaining.isEmpty =>
            val outPorts: List[DFVal] = getSet.designDB.designMemberTable(d).collect {
              case p @ DclOut() => p
              case p @ DclVar() => p
            }
            outPorts.foldLeft(currentSet) { case (cs, p) => consumeFrom(p, scopeMap, cs) }
          case _ =>
            currentSet
        val exitingBlock = remaining match
          case r :: _ if r.getOwnerBlock != currentBlock =>
            true // another member but not a child of current
          case Nil if !currentBlock.isTop && !(currentBlock eq analysisRoot) =>
            true // there are no more members, but still not at top
          case _ => false // no more members and we are currently back at top
        if (exitingBlock)
          val updatedScopeMap = currentBlock match
            case cb: DFConditional.Block =>
              //                println(s"exiting $cb", cb.isLastCB, cb.isExhaustive)
              //                val ret =
              scopeMap.branchExit(cb.isLastCB, cb.isExhaustive.getOrElse(false))
            //                println(s"${if (scopeMap.nonEmpty) scopeMap.head._2.toString else "<>"} => ${if (ret.nonEmpty) ret.head._2.toString else "<>"}")
            //                ret
            case _ => scopeMap
          getImplicitStateVars(remaining, currentBlock.getOwnerBlock, analysisRoot,
            updatedScopeMap, updatedSet, checkedDomain)
        else (updatedSet, scopeMap)
    end match
  end getImplicitStateVars

  type AssignMap = Map[DFVal, AssignedScope]

  extension (sm: AssignMap)(using MemberGetSet)
    def assignTo(toVal: DFVal, slice: Slice): AssignMap =
      val widthOpt = toVal.dfType.widthIntOpt
      sm + (toVal -> sm.getOrElse(toVal, AssignedScope.empty).assign(slice, widthOpt))
    def branchEntry(firstBranch: Boolean): AssignMap =
      sm.view.mapValues(_.branchEntry(firstBranch)).toMap
    def branchExit(lastBranch: Boolean, exhaustive: Boolean): AssignMap =
      sm.view.mapValues(_.branchExit(lastBranch, exhaustive)).toMap

  final case class AssignedScope(
      latest: Coverage,
      branchHistory: Option[Coverage],
      parentScopeOption: Option[AssignedScope],
      hasAssignments: Boolean
  ) derives CanEqual:
    @tailrec private def getLatest(
        acc: Coverage,
        parentScopeOption: Option[AssignedScope]
    ): Coverage =
      parentScopeOption match
        case Some(s) => getLatest(acc | s.latest, s.parentScopeOption)
        case None    => acc
    def getLatest: Coverage = getLatest(latest, parentScopeOption)

    /** Proof that `slice` is fully covered by this scope's assignments. Returns `Tri.Yes` only when
      * provably covered; `Tri.No` or `Tri.Unknown` otherwise (both are treated as "still consuming
      * state" by callers).
      */
    def contains(slice: Slice, widthOpt: Option[Int]): Tri =
      getLatest.contains(slice, widthOpt)
    def assign(slice: Slice, widthOpt: Option[Int]): AssignedScope =
      copy(latest = latest.assign(slice, widthOpt), hasAssignments = true)
    def branchEntry(firstBranch: Boolean): AssignedScope =
      val parentScope = if (firstBranch) this.copy(branchHistory = Some(getLatest)) else this
      AssignedScope(Coverage.empty, None, Some(this), hasAssignments)
    def branchExit(lastBranch: Boolean, exhaustive: Boolean): AssignedScope =
      parentScopeOption match
        case Some(parentScope) =>
          val updatedHistory = parentScope.branchHistory match
            case Some(h) => latest & h
            case None    => latest
          if (lastBranch)
            if (exhaustive)
              AssignedScope(
                parentScope.latest | updatedHistory,
                None,
                parentScope.parentScopeOption,
                hasAssignments
              )
            else
              AssignedScope(parentScope.latest, None, parentScope.parentScopeOption, hasAssignments)
          else
            AssignedScope(
              parentScope.latest,
              Some(updatedHistory),
              parentScope.parentScopeOption,
              hasAssignments
            )
        case None => this
  end AssignedScope
  private object AssignedScope:
    val empty: AssignedScope = AssignedScope(Coverage.empty, None, None, hasAssignments = false)
end StateAnalysis

extension (designDB: DB)
  // The implicit MemberGetSet is used for ref resolution during the walk
  // (allowing callers to pass an outer flat-DB getSet when analyzing a
  // sub-DB whose own getSet can't resolve cross-sub-DB refs).
  private def getImplicitStateVars(
      checkedDomain: DomainType => Boolean
  )(using MemberGetSet): Set[DFVal] =
    val (currentSet, scopeMap) = StateAnalysis.getImplicitStateVars(
      designDB.membersNoGlobals.drop(1), designDB.top, designDB.top, Map(), Set(), checkedDomain
    )
    currentSet.filter(p => scopeMap(p).hasAssignments)

  def getImplicitStateVarsDF(using MemberGetSet): Set[DFVal] =
    getImplicitStateVars(_ == DomainType.DF)
  def getImplicitStateVarsRT(using MemberGetSet): Set[DFVal] =
    getImplicitStateVars(_ == DomainType.RT)
end extension
