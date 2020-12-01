package DFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.immutable
import sim.DFSimMember

import analysis.ConditionalBlockAnalysis

final class ExplicitPrev[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def consumeFrom(
    value : DFAny.Member, relWidth : Int, relBitLow : Int,
    assignMap : Map[DFAny.Member, AssignedScope], currentSet : Set[DFAny.Member]
  ) : Set[DFAny.Member] = {
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match {
      case DFAny.Alias.AsIs(_,_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
      case DFAny.Alias.Invert(_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
      case DFAny.Alias.BitsWL(_,_,rv,rw,rbl,_,_) => consumeFrom(rv.get, rw, relBitLow + rbl, assignMap, currentSet)
      case DFAny.Alias.ApplySel(_,_,rv,idx,_,_) =>
        //For simplification, consuming the entirety of selection index and array
        val rvSet = consumeFrom(rv.get, assignMap, currentSet)
        val idxSet = consumeFrom(idx.get, assignMap, currentSet)
        (rvSet union idxSet)
      case DFAny.Port.Out() | DFAny.Var() =>
        designDB.getConnectionTo(value) match {
          case Some(n) => consumeFrom(n.fromRef.get, relWidth, relBitLow, assignMap, currentSet)
          case None =>
            val scope = assignMap(value)
            if (scope.isConsumingPrevAt(access)) currentSet union Set(value) else currentSet
        }
      case _ => currentSet
    }
  }
  private def consumeFrom(
    value : DFAny.Member,
    assignMap : Map[DFAny.Member, AssignedScope], currentSet : Set[DFAny.Member]
  ) : Set[DFAny.Member] =
    consumeFrom(value, value.width, 0, assignMap, currentSet)

  @tailrec private def assignTo(
    value : DFAny.Member, relWidth : Int, relBitLow : Int,
    assignMap : Map[DFAny.Member, AssignedScope]
  ) : Map[DFAny.Member, AssignedScope] = {
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match {
      case DFAny.Alias.AsIs(_,_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
      case DFAny.Alias.Invert(_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
      case DFAny.Alias.BitsWL(_,_,rv,rw,rbl,_,_) => assignTo(rv.get, relWidth, rbl + relBitLow, assignMap)
      case DFAny.Alias.ApplySel(_,_,rv,_,_,_) =>
        //for simplification, assigning the entirety of the array
        assignTo(rv.get, assignMap)
      case x => assignMap.assignTo(x, access)
    }
  }
  final implicit class ScopeMap(sm : Map[DFAny.Member, AssignedScope]) {
    def assignTo(toVal : DFAny.Member, assignBitSet : immutable.BitSet) : Map[DFAny.Member, AssignedScope] =
      sm + (toVal -> sm.getOrElse(toVal, AssignedScope.empty).assign(assignBitSet))
    def branchEntry(firstBranch : Boolean) : Map[DFAny.Member, AssignedScope] =
      sm.view.mapValues(_.branchEntry(firstBranch)).toMap
    def branchExit(lastBranch : Boolean, exhaustive : Boolean) : Map[DFAny.Member, AssignedScope] =
      sm.view.mapValues(_.branchExit(lastBranch, exhaustive)).toMap
  }
  private def assignTo(
    value : DFAny.Member, assignMap : Map[DFAny.Member, AssignedScope]
  ) : Map[DFAny.Member, AssignedScope] =
    assignTo(value, value.width, 0, assignMap)
  //retrieves a list of variables that are consumed as their implicit previous value.
  //the assignment stack map is pushed on every conditional block entry and popped on the block exit
  @tailrec private def getImplicitPrevVars(
    remaining : List[DFMember], currentBlock : DFBlock,
    scopeMap : Map[DFAny.Member, AssignedScope], currentSet : Set[DFAny.Member]
  ) : (Set[DFAny.Member], Map[DFAny.Member, AssignedScope]) = {
    remaining match {
      case (nextBlock : DFBlock) :: rs if nextBlock.getOwnerBlock == currentBlock => //entering child block
        val (updatedSet, updatedScopeMap) : (Set[DFAny.Member], Map[DFAny.Member, AssignedScope]) = nextBlock match {
          case DFConditional.IfElseBlock(Some(condRef),_,_,_) =>
            (consumeFrom(condRef.get, scopeMap, currentSet), scopeMap.branchEntry(firstBranch = true))
          case cb : DFConditional.Block =>
            (currentSet, scopeMap.branchEntry(cb.isFirstCB))
          case _ =>
            (currentSet, scopeMap)
        }
        getImplicitPrevVars(rs, nextBlock, updatedScopeMap, updatedSet)
      case r :: rs if r.getOwnerBlock == currentBlock => //checking member consumers
        val (updatedSet, updatedScopeMap) : (Set[DFAny.Member], Map[DFAny.Member, AssignedScope]) = r match {
          case net : DFNet =>
            (consumeFrom(net.fromRef.get, scopeMap, currentSet), assignTo(net.toRef.get, scopeMap))
          case func : DFAny.Func2 =>
            val left = consumeFrom(func.leftArgRef.get, scopeMap, currentSet)
            val right = consumeFrom(func.rightArgRef.get, scopeMap, currentSet)
            (left union right, scopeMap)
          case assert : DFSimMember.Assert =>
            val dfAnySet : Seq[DFAny.Member] =
              (assert.msgRef.seq.collect{case Left(x) => x} ++ assert.condOptionRef).map(_.get)
            val consume = dfAnySet.foldLeft(currentSet){
              case (set, x) => set union consumeFrom(x, scopeMap, currentSet)
            }
            (consume, scopeMap)
          case matchBlock : DFConditional.MatchHeader =>
            (consumeFrom(matchBlock.matchValRef.get, scopeMap, currentSet), scopeMap)
          case outPort @ DFAny.Port.Out() =>
            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          case anyVar @ DFAny.NewVar() =>
            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ =>
            (currentSet, scopeMap)
        }
        getImplicitPrevVars(rs, currentBlock, updatedScopeMap, updatedSet)
      case _ => //exiting child block or no more members
        val updatedSet = currentBlock match {
          case d : DFDesign.Block if remaining.isEmpty =>
            val outPorts : List[DFAny.Member] = designDB.designMemberTable(d).collect {
              case p @ DFAny.Port.Out() => p
              case p @ DFAny.NewVar() => p
            }
            outPorts.foldLeft(currentSet){case (cs, p) => consumeFrom(p, scopeMap, cs)}
          case _ =>
            currentSet
        }
        val exitingBlock = remaining match {
          case r :: _ if r.getOwnerBlock != currentBlock => true //another member but not a child of current
          case Nil if (currentBlock != designDB.top) => true //there are no more members, but still not at top
          case _ => false //no more members and we are currently back at top
        }
        if (exitingBlock) {
          val updatedScopeMap = currentBlock match {
            case cb : DFConditional.Block =>
              //                println(s"exiting $cb", cb.isLastCB, cb.isExhaustive)
              //                val ret =
              scopeMap.branchExit(cb.isLastCB, cb.isExhaustive)
            //                println(s"${if (scopeMap.nonEmpty) scopeMap.head._2.toString else "<>"} => ${if (ret.nonEmpty) ret.head._2.toString else "<>"}")
            //                ret
            case _ => scopeMap
          }
          getImplicitPrevVars(remaining, currentBlock.getOwnerBlock, updatedScopeMap, updatedSet)
        } else (updatedSet, scopeMap)
    }
  }

  def explicitPrev : IRCompilation[D] = {
    val (currentSet, scopeMap) = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
    val (explicitPrevSet, defaultsSet) = currentSet.partition(p => scopeMap(p).hasAssignments)
    val defaultsPatchList = Nil //Used to contain a default set of initialization. Currently, there is no real need.
//      defaultsSet
//      .flatMap {
//        case d @ DFAny.Dcl(dfType,_,Some(initHead +: _),_,_) if !initHead.isBubble => Some(
//          d -> Patch.Add(new MetaDesign() {
//            val initConst = DFAny.Const.forced(dfType, initHead)
//            d.assign(initConst)
//          }, Patch.Add.Config.After)
//        )
//        case _ => None
//      }
    val outs = explicitPrevSet.collect{case p @ DFAny.Port.Out() => p}.toList.groupBy(p => p.getOwnerDesign).toList
    val outPrevPatchList = outs.flatMap {
      case (block, portSet) =>
        val portDsns = portSet.map { p =>
          val dsn = new MetaDesign() {
            final val p_var = DFAny.NewVar(p.dfType) setName (s"${p.name}_var")
            private val p_sig_noinit = DFAny.NewVar(p.dfType) setName (s"${p.name}_sig")
            final val p_sig = __getset.set[DFAny.Member](p_sig_noinit)(_ => p_sig_noinit.member.asInstanceOf[DFAny.Dcl].copy(externalInit = p.externalInit))
            DFNet.Assignment(p_var, DFAny.Alias.Prev(p_sig, 1, DFAny.Alias.Prev.State))
          }
          (p, dsn)
        }
        val addedVarPatches = portDsns.map {
          case (p, dsn) => p -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.ChangeRefOnly, Patch.Replace.RefFilter.Inside(block)))
        }
        val addedAssignments = block -> Patch.Add(new MetaDesign() {
          portDsns.foreach {
            case (p, dsn) =>
              DFNet.Connection(dsn.p_sig, dsn.p_var)
              DFNet.Connection(p, dsn.p_sig)
          }
        }, Patch.Add.Config.InsideLast)
        addedAssignments :: addedVarPatches
    }
    val patchList = explicitPrevSet.toList.flatMap {
      case p @ DFAny.Port.Out() => None
      case e => Some(e -> Patch.Add(new MetaDesign() {
        DFNet.Assignment(e, DFAny.Alias.Prev(e, 1, DFAny.Alias.Prev.State))
      }, Patch.Add.Config.After))
    } ++ outPrevPatchList ++ defaultsPatchList

    //      println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
    c.newStage(designDB.patch(patchList))
  }
}