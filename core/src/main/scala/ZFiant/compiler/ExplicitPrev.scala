package ZFiant
package compiler

import DFiant.internals._
import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.collection.immutable

final class ExplicitPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  implicit class ConditionalBlockExtension(cb : ConditionalBlock) {
    def isFirstCB : Boolean = cb match {
      case _ : ConditionalBlock.IfBlock => true
      case x : ConditionalBlock.CasePatternBlock[_] if x.prevCaseRef.isEmpty => true
      case _ => false
    }
    def isLastCB : Boolean = {
      val refs = designDB.memberTable.getOrElse(cb, Set())
      //the conditional block is last if there is no reference to it as a previous block
      refs.flatMap {
        case r@ConditionalBlock.PrevBlockRef() => Some(r)
        case _ => None
      }.isEmpty
    }
    @tailrec private def getPatterns(casePattenBlock : ConditionalBlock.CasePatternBlock[_], patterns : List[DFAny.Pattern[_]]) : List[DFAny.Pattern[_]] = {
      val updatedPattens = casePattenBlock.pattern :: patterns
      casePattenBlock.prevCaseRef match {
        case Some(r) => getPatterns(r.get, updatedPattens)
        case None => updatedPattens
      }
    }
    def isExhaustive : Boolean = cb match {
      case _ : ConditionalBlock.ElseBlock => true
      case _ : ConditionalBlock.Case_Block => true
      case x : ConditionalBlock.CasePatternBlock[_] if x.isLastCB =>
        val matchVal = x.matchHeaderRef.matchValRef.get
        val patterns = getPatterns(x, List())
        matchVal.dfType match {
          case _ : DFUInt.Type[_] =>
            val union = patterns.asInstanceOf[List[DFUInt.Pattern]].foldLeft(IntervalSet.empty[BigInt]){case (is, p) => is | p.patternSet}
            val fullRange = Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(matchVal.width))
            union.contains(fullRange)
          case _ : DFSInt.Type[_] =>
            val union = patterns.asInstanceOf[List[DFSInt.Pattern]].foldLeft(IntervalSet.empty[BigInt]){case (is, p) => is | p.patternSet}
            val fullRange = Interval.closed(BigInt.minSignedFromWidth(matchVal.width), BigInt.maxSignedFromWidth(matchVal.width))
            union.contains(fullRange)
          case _ : DFBits.Type[_] =>
            val union = patterns.asInstanceOf[List[DFBits.Pattern]].foldLeft(Set.empty[BitVector]){case (s, p) => s | p.patternSet}
            union.size == BigInt.maxUnsignedFromWidth(matchVal.width).toInt
          case _ : DFBool.Type =>
            val union = patterns.asInstanceOf[List[DFBool.Pattern]].foldLeft(Set.empty[Boolean]){case (s, p) => s | p.patternSet}
            union.size == 2
          case e : DFEnum.Type[_] =>
            val union = patterns.asInstanceOf[List[DFEnum.Pattern[Enum]]].foldLeft(Set.empty[Enum#Entry]){case (s, p) => s | p.patternSet}
            union.size == e.enumType.entries.size
        }
      case _ => false
    }
  }

  @tailrec private def consumeFrom(value : DFAny, relWidth : Int, relBitLow : Int, assignMap : Map[DFAny, AssignedScope], currentSet : Set[DFAny]) : Set[DFAny] = {
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match {
      case DFAny.Alias.AsIs(_,_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
      case DFAny.Alias.Invert(_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
      case DFAny.Alias.BitsWL(_,_,rv,rw,rbl,_,_) => consumeFrom(rv.get, rw, relBitLow + rbl, assignMap, currentSet)
      case x if x.modifier.isInstanceOf[DFAny.Modifier.Assignable] =>
        designDB.getConnectionTo(value) match {
          case Some(v) => consumeFrom(v, relWidth, relBitLow, assignMap, currentSet)
          case None =>
            val scope = assignMap(value)
            if (scope.isConsumingPrevAt(access)) currentSet union Set(value) else currentSet
        }
      case _ => currentSet
    }
  }
  private def consumeFrom(value : DFAny, assignMap : Map[DFAny, AssignedScope], currentSet : Set[DFAny]) : Set[DFAny] =
    consumeFrom(value, value.width, 0, assignMap, currentSet)

  @tailrec private def assignTo(value : DFAny, relWidth : Int, relBitLow : Int, assignMap : Map[DFAny, AssignedScope]) : Map[DFAny, AssignedScope] = {
    val access = immutable.BitSet.empty ++ (relBitLow until relBitLow + relWidth)
    value match {
      case DFAny.Alias.AsIs(_,_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
      case DFAny.Alias.Invert(_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
      case DFAny.Alias.BitsWL(_,_,rv,rw,rbl,_,_) => assignTo(rv.get, relWidth, rbl + relBitLow, assignMap)
      case x => assignMap.assignTo(x, access)
    }
  }
  implicit class ScopeMap(sm : Map[DFAny, AssignedScope]) {
    def assignTo(toVal : DFAny, assignBitSet : immutable.BitSet) : Map[DFAny, AssignedScope] =
      sm + (toVal -> sm.getOrElse(toVal, AssignedScope.empty).assign(assignBitSet))
    def branchEntry(firstBranch : Boolean) : Map[DFAny, AssignedScope] =
      sm.view.mapValues(_.branchEntry(firstBranch)).toMap
    def branchExit(lastBranch : Boolean, exhaustive : Boolean) : Map[DFAny, AssignedScope] =
      sm.view.mapValues(_.branchExit(lastBranch, exhaustive)).toMap
  }
  private def assignTo(value : DFAny, assignMap : Map[DFAny, AssignedScope]) : Map[DFAny, AssignedScope] =
    assignTo(value, value.width, 0, assignMap)
  //retrieves a list of variables that are consumed as their implicit previous value.
  //the assignment stack map is pushed on every conditional block entry and popped on the block exit
  @tailrec private def getImplicitPrevVars(remaining : List[DFMember], currentBlock : DFBlock, scopeMap : Map[DFAny, AssignedScope], currentSet : Set[DFAny]) : Set[DFAny] = {
    remaining match {
      case (nextBlock : DFBlock) :: rs if nextBlock.ownerRef.get == currentBlock => //entering child block
        val updatedScopeMap = nextBlock match {
          case cb : ConditionalBlock =>
            //              println(s"entering $cb", cb.isFirstCB)
            //              val ret =
            scopeMap.branchEntry(cb.isFirstCB)
          //              println(s"${if (scopeMap.nonEmpty) scopeMap.head._2.toString else "<>"} => ${if (ret.nonEmpty) ret.head._2.toString else "<>"}")
          //              ret
          case _ => scopeMap
        }
        getImplicitPrevVars(rs, nextBlock, updatedScopeMap, currentSet)
      case r :: rs if r.ownerRef.get == currentBlock => //checking member consumers
        val (updatedSet, updatedScopeMap) : (Set[DFAny], Map[DFAny, AssignedScope]) = r match {
          case net : DFNet =>
            (consumeFrom(net.fromRef.get, scopeMap, currentSet), assignTo(net.toRef.get, scopeMap))
          case func : DFAny.Func2[_,_,_,_] =>
            val left = consumeFrom(func.leftArgRef.get, scopeMap, currentSet)
            val right = consumeFrom(func.rightArgRef.get, scopeMap, currentSet)
            (left union right, scopeMap)
          case ifBlock : ConditionalBlock.IfBlock =>
            (consumeFrom(ifBlock.condRef.get, scopeMap, currentSet), scopeMap)
          case elseIfBlock : ConditionalBlock.ElseIfBlock =>
            (consumeFrom(elseIfBlock.condRef.get, scopeMap, currentSet), scopeMap)
          case matchBlock : ConditionalBlock.MatchHeader =>
            (consumeFrom(matchBlock.matchValRef.get, scopeMap, currentSet), scopeMap)
          case outPort : DFAny.Port.Out[_,_] =>
            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          case anyVar : DFAny.NewVar[_,_] =>
            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ =>
            (currentSet, scopeMap)
        }
        getImplicitPrevVars(rs, currentBlock, updatedScopeMap, updatedSet)
      case _ => //exiting child block or no more members
        val exitingBlock = remaining match {
          case r :: _ if r.ownerRef.get != currentBlock => true //another member but not a child of current
          case Nil if (currentBlock != designDB.top) => true //there are no more members, but still not at top
          case _ => false //no more members and we are currently back at top
        }
        if (exitingBlock) {
          val updatedScopeMap = currentBlock match {
            case cb : ConditionalBlock =>
              //                println(s"exiting $cb", cb.isLastCB, cb.isExhaustive)
              //                val ret =
              scopeMap.branchExit(cb.isLastCB, cb.isExhaustive)
            //                println(s"${if (scopeMap.nonEmpty) scopeMap.head._2.toString else "<>"} => ${if (ret.nonEmpty) ret.head._2.toString else "<>"}")
            //                ret
            case _ => scopeMap
          }
          getImplicitPrevVars(remaining, currentBlock.getOwner, updatedScopeMap, currentSet)
        } else {
          assert(currentBlock == designDB.top)
          val outPorts : List[DFAny] = designDB.ownerMemberTable(designDB.top).collect{case p : DFAny.Port.Out[_,_] => p}
          //consuming from top-level output ports
          outPorts.foldLeft(currentSet){case (cs, p) => consumeFrom(p, scopeMap, cs)}
        }
    }
  }

  def explicitPrev = {
    val explicitPrevSet = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
    val patchList = explicitPrevSet.toList.map(e => e -> Patch.Add(new MetaDesign() {
      DFNet.Assignment(e, DFAny.Alias.Prev(e, 1))
    }, Patch.Add.Config.After))

    //      println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
    c.newStage[ExplicitPrev](designDB.patch(patchList), Seq())
  }
}

trait ExplicitPrev extends Compilable.Stage
