package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

import scala.annotation.tailrec
import scala.collection.immutable

case object ExplicitState extends Stage:
  def dependencies: List[Stage] = List(ExplicitNamedVars, DropLocalDcls)
  def nullifies: Set[Stage] = Set()

  @tailrec private def consumeFrom(
      value: DFVal,
      relWidth: Int,
      relBitLow: Int,
      assignMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet): Set[DFVal] =
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match
      case DFVal.Alias.AsIs(toType, relValRef, _, _, _) =>
        val relVal = relValRef.get
        if (toType.width == relVal.width)
          // casting maintains relative bit consumption
          consumeFrom(relVal, relWidth, relBitLow, assignMap, currentSet)
        else
          // conversion is treated like any function argument and restarts bit consumption
          consumeFrom(relVal, relVal.width, 0, assignMap, currentSet)
      case DFVal.Alias.ApplyRange(relValRef, rbh, rbl, _, _, _) =>
        consumeFrom(relValRef.get, rbh - rbl + 1, relBitLow + rbl, assignMap, currentSet)
      case DFVal.Alias.ApplyIdx(_, relValRef, idxRef, _, _, _) =>
        // For simplification, consuming the entirety of selection index and array
        val rvSet = consumeFrom(relValRef.get, assignMap, currentSet)
        val idxSet = consumeFrom(idxRef.get, assignMap, currentSet)
        (rvSet union idxSet)
      case DFVal.Alias.SelectField(dfType: DFStruct, relValRef, fieldName, _, _, _) =>
//        var rbh = dfType.width - 1
//        dfType.fieldMap.foreach { case (n, t) =>
//          val rbl = rbh - t.width + 1
//          if (n == fieldName)
//          rbh = rbl - 1
//        }
        ???
      case DclOut() | DclVar() =>
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
    consumeFrom(value, value.dfType.width, 0, assignMap, currentSet)

  @tailrec private def assignTo(
      value: DFVal,
      relWidth: Int,
      relBitLow: Int,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match
      case DFVal.Alias.AsIs(_, relValRef, _, _, _) =>
        assignTo(relValRef.get, relWidth, relBitLow, assignMap)
      case DFVal.Alias.ApplyRange(relValRef, rbh, rbl, _, _, _) =>
        assignTo(relValRef.get, relWidth, rbl + relBitLow, assignMap)
      case DFVal.Alias.ApplyIdx(_, relValRef, idxRef, _, _, _) =>
        // for simplification, assigning the entirety of the array
        assignTo(relValRef.get, assignMap)
      case DFVal.Alias.SelectField(dfType: DFStruct, relValRef, fieldName, _, _, _) =>
        ???
      case x => assignMap.assignTo(x, access)
  end assignTo

  private def assignTo(
      value: DFVal,
      assignMap: AssignMap
  )(using MemberGetSet): AssignMap =
    assignTo(value, value.dfType.width, 0, assignMap)

  // retrieves a list of variables that are consumed as their implicit previous value.
  // the assignment stack map is pushed on every conditional block entry and popped on the block exit
  @tailrec private def getImplicitStateVars(
      remaining: List[DFMember],
      currentBlock: DFBlock,
      scopeMap: AssignMap,
      currentSet: Set[DFVal]
  )(using MemberGetSet): (Set[DFVal], AssignMap) =
    remaining match
      case (nextBlock: DFBlock) :: rs
          if nextBlock.getOwnerBlock == currentBlock => // entering child block
        val (updatedSet, updatedScopeMap): (Set[DFVal], AssignMap) = nextBlock match
          case cb: DFConditional.Block =>
            cb.guardRef.get match
              case dfVal: DFVal => consumeFrom(dfVal, scopeMap, currentSet)
              case _            => // do nothing
            (currentSet, scopeMap.branchEntry(cb.isFirstCB))
          case _ =>
            (currentSet, scopeMap)
        getImplicitStateVars(rs, nextBlock, updatedScopeMap, updatedSet)
      case r :: rs
          if r.getOwnerBlock == currentBlock && currentBlock.getThisOrOwnerDomain
            .domainType == DomainType.DF => // checking member consumers
        val (updatedSet, updatedScopeMap): (Set[DFVal], AssignMap) = r match
          case net @ DFNet.Assignment(toVal, fromVal) =>
            (consumeFrom(fromVal, scopeMap, currentSet), assignTo(toVal, scopeMap))
          case net @ DFNet.Connection(toVal: DFVal, fromVal: DFVal, _) =>
            (consumeFrom(fromVal, scopeMap, currentSet), assignTo(toVal, scopeMap))
          case func: DFVal.Func =>
            val args = func.args.map(a => consumeFrom(a.get, scopeMap, currentSet))
            (args.reduce(_ union _), scopeMap)
          case assert: DFSimMember.Assert =>
//            val dfAnySet: Seq[DFVal] =
//              (assert.msgRef.seq.collect { case Left(x) => x } ++ assert.condOptionRef).map(_.get)
//            val consume = dfAnySet.foldLeft(currentSet) { case (set, x) =>
//              set union consumeFrom(x, scopeMap, currentSet)
//            }
//            (consume, scopeMap)
            ???
          case matchBlock: DFConditional.DFMatchHeader =>
            (consumeFrom(matchBlock.selectorRef.get, scopeMap, currentSet), scopeMap)
          case outPort @ DclOut() =>
            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          case anyVar @ DclVar() =>
            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ =>
            (currentSet, scopeMap)
        getImplicitStateVars(rs, currentBlock, updatedScopeMap, updatedSet)
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
          getImplicitStateVars(remaining, currentBlock.getOwnerBlock, updatedScopeMap, updatedSet)
        else (updatedSet, scopeMap)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val (currentSet, scopeMap) =
      getImplicitStateVars(designDB.membersNoGlobals.drop(1), designDB.top, Map(), Set())
//    println("scopeMap:")
//    println(scopeMap.mkString("\n"))
    val (explicitStateSet, defaultsSet) = currentSet.partition(p => scopeMap(p).hasAssignments)
//    println("explicitStateSet")
//    println(explicitStateSet)
//    println("defaultSet")
//    println(defaultsSet)
    val patchList = explicitStateSet.flatMap {
      // for initialized ports and variables we just add an explicit prev self-assignment
      case e: DFVal.Dcl if e.initRefList.nonEmpty =>
        Some(
          new MetaDesign(e, Patch.Add.Config.After):
            e.asVarAny := e.asValAny.asInitialized.prev // .prev works the same as .reg for meta programming in RT domain
          .patch
        )
      // if not initialized we also need to add bubble tagging to the initialization
      case e: DFVal.Dcl =>
        val explicitStateAssignDsn = new MetaDesign(e, Patch.Add.Config.After):
          val modifier = dfhdl.core.Modifier(e.modifier)
          val dfType = new dfhdl.core.DFType(e.dfType)
          val bubble = dfhdl.core.Bubble.constValOf(dfType, named = false)
          val dclWithInit =
            dfhdl.core.DFVal.Dcl(dfType, modifier, List(bubble))(using
              dfc.setMeta(e.meta)
            ).asVarAny.asInitialized
          dclWithInit := dclWithInit.prev // .prev works the same as .reg for meta programming in RT domain
        val dclPatch = e -> Patch.Replace(
          explicitStateAssignDsn.dclWithInit.asIR,
          Patch.Replace.Config.ChangeRefAndRemove
        )
        List(dclPatch, explicitStateAssignDsn.patch)
      case _ => None
    }.toList
    designDB.patch(patchList)
  end transform
end ExplicitState

private type AssignMap = Map[DFVal, AssignedScope]

extension (sm: AssignMap)(using MemberGetSet)
  def assignTo(toVal: DFVal, assignBitSet: immutable.BitSet): AssignMap =
    sm + (toVal -> sm.getOrElse(toVal, AssignedScope.empty).assign(assignBitSet))
  def branchEntry(firstBranch: Boolean): AssignMap =
    sm.view.mapValues(_.branchEntry(firstBranch)).toMap
  def branchExit(lastBranch: Boolean, exhaustive: Boolean): AssignMap =
    sm.view.mapValues(_.branchExit(lastBranch, exhaustive)).toMap

private final case class AssignedScope(
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
  def branchExit(lastBranch: Boolean, exhaustive: Boolean): AssignedScope = parentScopeOption match
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
        else AssignedScope(parentScope.latest, None, parentScope.parentScopeOption, hasAssignments)
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

extension [T: HasDB](t: T)
  def explicitState(using CompilerOptions): DB =
    StageRunner.run(ExplicitState)(t.db)
