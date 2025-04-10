package dfhdl.compiler
package analysis
import ir.*
import dfhdl.internals.*
import scala.annotation.tailrec
import scala.collection.immutable

object StateAnalysis:
  @tailrec private def consumeFrom(
      value: DFVal,
      relWidth: Int,
      relBitLow: Int,
      assignMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet): Set[DFVal] =
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match
      case DFVal.Alias.AsIs(dfType = toType, relValRef = relValRef) =>
        val relVal = relValRef.get
        if (toType.width == relVal.width)
          // casting maintains relative bit consumption
          consumeFrom(relVal, relWidth, relBitLow, assignMap, currentSet)
        else
          // conversion is treated like any function argument and restarts bit consumption
          consumeFrom(relVal, relVal.width, 0, assignMap, currentSet)
      case DFVal.Alias.ApplyRange(relValRef = relValRef, relBitHigh = rbh, relBitLow = rbl) =>
        consumeFrom(relValRef.get, rbh - rbl + 1, relBitLow + rbl, assignMap, currentSet)
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
      case dcl: DFVal.Dcl if (dcl.isPortOut || dcl.isVar) && !dcl.isReg && !dcl.isInProcess =>
        value.getConnectionTo match
          case Some(DFNet.Connection(_, fromVal: DFVal, _)) =>
            consumeFrom(fromVal, relWidth, relBitLow, assignMap, currentSet)
          case _ =>
            val scope = assignMap(value)
            if (scope.isConsumingStateAt(access)) currentSet union Set(value) else currentSet
      case _ => currentSet
    end match
  end consumeFrom

  private def consumeFrom(
      value: DFVal,
      assignMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet): Set[DFVal] =
    value.dfType match
      case _: DFUnbounded => currentSet
      case _ =>
        consumeFrom(value, value.dfType.width, 0, assignMap, currentSet)

  @tailrec private def assignTo(
      value: DFVal,
      relWidth: Int,
      relBitLow: Int,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match
      case DFVal.Alias.AsIs(relValRef = relValRef) =>
        assignTo(relValRef.get, relWidth, relBitLow, assignMap)
      case DFVal.Alias.ApplyRange(relValRef = relValRef, relBitHigh = rbh, relBitLow = rbl) =>
        assignTo(relValRef.get, relWidth, rbl + relBitLow, assignMap)
      case DFVal.Alias.ApplyIdx(relValRef = relValRef, relIdx = idxRef) =>
        // for simplification, assigning the entirety of the array
        assignTo(relValRef.get, assignMap)
      case DFVal.Alias.SelectField(
            dfType = dfType: DFStruct,
            relValRef = relValRef,
            fieldName = fieldName
          ) =>
        ???
      case x => assignMap.assignTo(x, access)
    end match
  end assignTo

  private def assignTo(
      value: DFVal,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    assignTo(value, value.dfType.width, 0, assignMap)

  // retrieves a list of variables that are consumed as their implicit previous value.
  // the assignment stack map is pushed on every conditional block entry and popped on the block exit
  @tailrec final def getImplicitStateVars(
      remaining: List[DFMember],
      currentBlock: DFBlock,
      scopeMap: AssignMap,
      currentSet: Set[DFVal],
      checkedDomain: DomainType => Boolean
  )(using MemberGetSet): (Set[DFVal], AssignMap) =
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
        getImplicitStateVars(rs, nextBlock, updatedScopeMap, updatedSet, checkedDomain)
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
            val args = textOut.getRefs.collect { case DFRef(v: DFVal) => v }.map(a =>
              consumeFrom(a, scopeMap, currentSet)
            )
            (args.reduce(_ union _), scopeMap)
          case matchBlock: DFConditional.DFMatchHeader =>
            (consumeFrom(matchBlock.selectorRef.get, scopeMap, currentSet), scopeMap)
          case outPort @ DclOut() =>
            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          case anyVar @ DclVar() =>
            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ =>
            (currentSet, scopeMap)
        getImplicitStateVars(rs, currentBlock, updatedScopeMap, updatedSet, checkedDomain)
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
          case Nil if !currentBlock.isTop =>
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
          getImplicitStateVars(remaining, currentBlock.getOwnerBlock, updatedScopeMap, updatedSet,
            checkedDomain)
        else (updatedSet, scopeMap)

  type AssignMap = Map[DFVal, AssignedScope]

  extension (sm: AssignMap)(using MemberGetSet)
    def assignTo(toVal: DFVal, assignBitSet: immutable.BitSet): AssignMap =
      sm + (toVal -> sm.getOrElse(toVal, AssignedScope.empty).assign(assignBitSet))
    def branchEntry(firstBranch: Boolean): AssignMap =
      sm.view.mapValues(_.branchEntry(firstBranch)).toMap
    def branchExit(lastBranch: Boolean, exhaustive: Boolean): AssignMap =
      sm.view.mapValues(_.branchExit(lastBranch, exhaustive)).toMap

  final case class AssignedScope(
      latest: immutable.BitSet,
      branchHistory: Option[immutable.BitSet],
      parentScopeOption: Option[AssignedScope],
      hasAssignments: Boolean
  ) derives CanEqual:
    @tailrec private def getLatest(
        latest: immutable.BitSet,
        parentScopeOption: Option[AssignedScope]
    ): immutable.BitSet =
      parentScopeOption match
        case Some(s) => getLatest(latest | s.latest, s.parentScopeOption)
        case None    => latest
    def getLatest: immutable.BitSet = getLatest(latest, parentScopeOption)
    def isConsumingStateAt(consumeBitSet: immutable.BitSet): Boolean =
      (consumeBitSet &~ getLatest).nonEmpty
    def assign(assignBitSet: immutable.BitSet): AssignedScope =
      copy(latest = latest | assignBitSet, hasAssignments = true)
    def branchEntry(firstBranch: Boolean): AssignedScope =
      val parentScope = if (firstBranch) this.copy(branchHistory = Some(getLatest)) else this
      AssignedScope(immutable.BitSet(), None, Some(this), hasAssignments)
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
    val empty: AssignedScope = AssignedScope(immutable.BitSet(), None, None, hasAssignments = false)
end StateAnalysis

extension (designDB: DB)
  private def getImplicitStateVars(
      checkedDomain: DomainType => Boolean
  ): Set[DFVal] =
    import designDB.getSet
    val (currentSet, scopeMap) = StateAnalysis.getImplicitStateVars(
      designDB.membersNoGlobals.drop(1), designDB.top, Map(), Set(), checkedDomain
    )
    currentSet.filter(p => scopeMap(p).hasAssignments)

  def getImplicitStateVarsDF: Set[DFVal] = getImplicitStateVars(_ == DomainType.DF)
  def getImplicitStateVarsRT: Set[DFVal] = getImplicitStateVars(_.isInstanceOf[DomainType.RT])
end extension
