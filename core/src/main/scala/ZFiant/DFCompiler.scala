/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package ZFiant
import DFiant.internals._
import DFDesign.DB.Patch

import collection.immutable
import scala.annotation.tailrec

trait Compilable[-T] {
  def apply(t : T) : DFDesign.DB
}
object Compilable {
  def apply[T](implicit comp : Compilable[T]) : Compilable[T] = comp
  implicit val fromDB : Compilable[DFDesign.DB] = t => t
  implicit val fromDFDesign : Compilable[DFDesign] = t => t.db
}


object DFCompiler {
  implicit class Discovery(designDB : DFDesign.DB) {
    def discovery : DFDesign.DB = {
      ???
    }
  }

  implicit class Utils[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    def fixAnonymous : DFDesign.DB = {
      val anonymizeList = designDB.designMemberList.flatMap {
        case (block, members) =>
          members.filterNot(m => m.isAnonymous).groupBy(m => (m.tags.meta.namePosition, m.name)).flatMap {
          //In case an anonymous member got a name from its owner. For example:
          //val ret = DFBits(8).ifdf(cond) {
          //  i & i
          //}
          //The `i & i` function would also get the `ret` name just as the if block itself
          case ((pos, _), gm) if (pos == block.tags.meta.namePosition) => gm
          case (_, gm) if (gm.length > 1) =>
            //In case an anonymous member was used as an argument to an owner. For example:
            //val ret = DFBits(8).ifdf(i & i) {
            //}
            //The `i & i` function would also get the `ret` name just as the if block itself
            if (gm.collectFirst{case x : DFBlock => x}.isDefined) gm.collect {case a : DFAny.CanBeAnonymous => a}
            //In case an anonymous member inside a composition, we anonymize all but the last. For example:
            //val ret = i & i | i
            //Only the final 'Or' operation would be considered for the name `ret`
            else gm.dropRight(1)
          case _ => List()
        }
      }
      designDB.patch(anonymizeList.map(a => a -> Patch.Replace(a.anonymize, Patch.Replace.Config.FullReplacement)))
    }
    def uniqueNames : DFDesign.DB = ???
    @tailrec private def mcf(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
      remaining match {
        case (block : DFBlock) :: mList =>
          val members = designDB.ownerMemberTable(block)
          val sortedMembers = block match {
            case _ : DFDesign.Block =>
              val split = members.partition {
                case _ : CanBeGuarded => false
                case _ => true
              }
              split._1 ++ split._2
            case _ => members
          }
          mcf(sortedMembers ++ mList, block :: retList)
        case m :: mList => mcf(mList, m :: retList)
        case Nil => retList.reverse
      }
    def moveConnectableFirst : DFDesign.DB = designDB.copy(members = mcf(List(designDB.top), List()))
  }

  protected final case class AssignedScope(latest : immutable.BitSet, branchHistory : Option[immutable.BitSet], parentScopeOption : Option[AssignedScope]) {
    @tailrec private def getLatest(latest : immutable.BitSet, parentScopeOption : Option[AssignedScope]) : immutable.BitSet =
      parentScopeOption match {
        case Some(s) => getLatest(latest | s.latest, s.parentScopeOption)
        case None => latest
      }
    def getLatest : immutable.BitSet = getLatest(latest, parentScopeOption)
    def isConsumingPrevAt(consumeBitSet : immutable.BitSet) : Boolean = (consumeBitSet &~ getLatest).nonEmpty
    def assign(assignBitSet : immutable.BitSet) : AssignedScope = copy(latest | assignBitSet)
    def branchEntry(firstBranch : Boolean) : AssignedScope = {
      val parentScope = if (firstBranch) this.copy(branchHistory = Some(getLatest)) else this
      AssignedScope(immutable.BitSet(), None, Some(this))
    }
    def branchExit(lastBranch : Boolean, exhaustive : Boolean) : AssignedScope = parentScopeOption match {
      case Some(parentScope) =>
        val updatedHistory = parentScope.branchHistory match {
          case Some(h) => latest & h
          case None => latest
        }
        if (lastBranch) {
          if (exhaustive) AssignedScope(parentScope.latest | updatedHistory, None, parentScope.parentScopeOption)
          else AssignedScope(parentScope.latest, None, parentScope.parentScopeOption)
        } else
          AssignedScope(parentScope.latest, Some(updatedHistory), parentScope.parentScopeOption)
      case None => this
    }
  }
  protected object AssignedScope {
    val empty : AssignedScope = AssignedScope(immutable.BitSet(), None, None)
  }

  implicit class ExplicitPrev[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
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
          val scope = assignMap(value)
          if (scope.isConsumingPrevAt(access)) currentSet union Set(value) else currentSet
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
        sm + (toVal -> sm(toVal).assign(assignBitSet))
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

    def explicitPrev : DFDesign.DB = {
      val explicitPrevSet = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
      val patchList = explicitPrevSet.toList.map(e => e -> Patch.Add(new MetaDesign() {
        DFNet.Assignment(e, DFAny.Alias.Prev(e, 1))
      }, Patch.Add.Config.After))

//      println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
      designDB.patch(patchList)
    }
  }


  implicit class Flatten[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    private def flattenName(member : DFMember) : DFMember = member.setName(s"${member.getOwner.name}_${member.name}")
    private def flattenPort(port : DFAny) : List[(DFMember, Patch)] = {
      val incomingBlock = port match {
        case DFAny.In() => port.getOwnerDesign.getOwnerDesign
        case DFAny.Out() => port.getOwnerDesign
      }
      val producersToPort = designDB.consumerDependencyTable(port)
      if (producersToPort.size == 1) {
        val producerToPort = producersToPort.head
        val ibMembers = designDB.ownerMemberTable(incomingBlock) //TODO: perhaps at any hierarchy?
        val unusedNet = ibMembers.collectFirst{
          case m : DFNet.Connection if m.toRef.get == port => m
        }.get
        val replacement = if (producerToPort.isAnonymous) {
          if (designDB.producerDependencyTable(producerToPort).size > 1) producerToPort.setName(port.name)
          else producerToPort
        } else producerToPort
        List((port : DFMember, Patch.Replace(replacement, Patch.Replace.Config.FullReplacement)), (unusedNet, Patch.Remove))
      } else {
        List(port -> Patch.Replace(flattenName(DFAny.NewVar(port.dfType, DFAny.NewVar.Uninitialized, port.ownerRef, port.tags)), Patch.Replace.Config.FullReplacement))
      }
    }
    private def flattenPatch(block : DFBlock) : List[(DFMember, Patch)] = {
      if (block.isTop) List() else {
        val members = designDB.ownerMemberTable(block)
        val owner = block.getOwnerDesign
        (block -> Patch.Replace(owner, Patch.Replace.Config.FullReplacement)) :: members.flatMap {
          case p : DFAny.Port.In[_,_] => flattenPort(p)
          case p : DFAny.Port.Out[_,_] => flattenPort(p)
          case m if !m.isAnonymous => List(m -> Patch.Replace(flattenName(m), Patch.Replace.Config.FullReplacement))
          case _ => None
        }
      }
    }
    def flattenInline : DFDesign.DB = {
      val inlineBlocks = designDB.members.collect{case ib@DFDesign.Block.Internal(_,_,Some(_)) => ib}
      val patchList = inlineBlocks.flatMap(ib => flattenPatch(ib))
      designDB.patch(patchList)
    }
    def flatten(design : DFDesign*) : DFDesign.DB = designDB.patch(design.flatMap(d => flattenPatch(d.block)).toList)
  }

  implicit class Checker[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    def connectionCheck : DFDesign.DB = {
//      designDB.members.collect {
//
//      }
      ???
    }
  }

  /*
  Internal design blocks will be connected via dedicated "wiring" variables.
  This is very useful when compiling to the basic RTL languages (VHDL/Verilog) that require extra signaling
  and cannot directly connect between sibling design blocks or use output ports. So each port PPP for block BBB
  becomes a connection PPP.BBB <> PPP_BBB, where the extra variable `PPP_BBB` is used to represent an RTL
  signal. This of course does not change the correctness of any DFiant design, but is required for later phase
  where the code is compiled to RTL.

  For example:
    trait ID extends DFDesign {
      val i = DFUInt(8) <> IN
      val o = DFUInt(8) <> OUT
      o <> i
    }

    trait IDTop extends DFDesign {
      val x = DFUInt(8) <> IN
      val y = DFUInt(8) <> OUT
      val id1 = new ID {}
      val id2 = new ID {}
      id1.i <> x
      id1.o <> id2.i //This cannot be done in VHDL/Verilog
      id2.o <> y
    }

    Will become:
    trait IDTop extends DFDesign {
      final val x = DFUInt(8) <> IN
      final val y = DFUInt(8) <> OUT
      final val id1_i = DFUInt(8)
      final val id1_o = DFUInt(8)
      final val id1 = new ID {
        i <> id1_i
        id1_o <> o
      }
      final val id2_i = DFUInt(8)
      final val id2_o = DFUInt(8)
      final val id2 = new ID {
        i <> id2_i
        id2_o <> o
      }
      id1_i <> x
      id2_i <> id1_o
      y <> id2_o
    }
  */
  implicit class ViaPortConnection[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    def viaPortConnection : DFDesign.DB = {
      val internalBlocks : List[DFDesign.Block.Internal] = designDB.members.collect{case d : DFDesign.Block.Internal => d}
      val patchList : List[(DFMember, Patch)] = internalBlocks.flatMap{ib =>
        //getting only ports that are not already connected to variables
        val ports : List[DFAny] = designDB.ownerMemberTable(ib).flatMap {
          case p : DFAny.Port.Out[_,_] =>
            val conns = designDB.getConnectionFrom(p)
            if ((conns.size == 1) && conns.head.isInstanceOf[DFAny.NewVar[_,_]]) None
            else Some(p)
          case p : DFAny.Port.In[_,_] =>
            import designDB.getset
            designDB.getConnectionTo(p) match {
              case Some(_ : DFAny.NewVar[_,_]) => None
              case Some(_ : DFAny.Port.In[_,_]) => Some(p)
              case Some(x) if x.isMemberOfDesign(ib) || x.isMemberOfDesign(ib.getOwnerDesign) => None
              case _ => Some(p)
            }
          case _ => None
        }
        val addVarsDsn = new MetaDesign() {
          val portsToVars : List[(DFAny, DFAny)] = ports.map {p =>
            p -> (DFAny.NewVar(p.dfType) setName(s"${ib.name}_${p.name}"))
          }
        }
        val connectDsn = new MetaDesign(true) {
          val refPatches : List[(DFMember, Patch)] = addVarsDsn.portsToVars.map {case (p, v) =>
            p match {
              case _ : DFAny.Port.Out[_,_] => DFNet.Connection(v, p)
              case _ : DFAny.Port.In[_,_] => DFNet.Connection(p, v)
              case _ => ???
            }
            (p, Patch.Replace(v, Patch.Replace.Config.ChangeRefOnly, Patch.Replace.Scope.Outside(ib)))
          }
        }
        (ib -> Patch.Add(addVarsDsn, Patch.Add.Config.Before)) ::
          (ib -> Patch.Add(connectDsn, Patch.Add.Config.Inside)) :: connectDsn.refPatches
      }
      designDB.patch(patchList)
    }
  }

  implicit class Calculator[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    @tailrec private def calcInit(remaining : List[DFAny], calc : Map[DFAny, Seq[DFAny.Token]], requestedCalc : Set[DFAny]) : Map[DFAny, Seq[DFAny.Token]] = {
      def getInit[T <: DFAny](member : T) : Option[Seq[member.TToken]] = member.tags.init match {
        case Some(init) => Some(init).asInstanceOf[Option[Seq[member.TToken]]]
        case None => calc.get(member).asInstanceOf[Option[Seq[member.TToken]]]
      }
      def getRetVal(cb : ConditionalBlock.WithRetVal[_]) : DFAny = {
        designDB.ownerMemberTable(cb).last match {
          case n : DFNet.Assignment if n.toRef.get == cb.retVarRef.get => n.fromRef.get
          case m => throw new IllegalArgumentException(s"Unexpected last member in conditional block. Expected assignment to the return variable, but got $m")
        }
      }
      remaining match {
        case m :: mList => if (getInit(m).isDefined) calcInit(mList, calc, requestedCalc) else m match {
          case c : DFAny.Const[_] => calcInit(mList, calc + (m -> Seq(c.token)), requestedCalc)
          case f : DFAny.Func2[_,_,_,_] =>
            val leftArg = f.leftArgRef.get
            val rightArg = f.rightArgRef.get
            (getInit(leftArg), getInit(rightArg)) match {
              case (Some(leftInit), Some(rightInit)) =>
                calcInit(mList, calc + (m -> f.initFunc(leftInit, rightInit)), requestedCalc)
              case _ if requestedCalc.contains(m) =>
                calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
              case _ =>
                calcInit(leftArg :: rightArg :: remaining, calc, requestedCalc + m) //first need to calculate args
            }
          case a : DFAny.Alias[_,_,_] => designDB.getConnectionTo(a) match {
            //connection overrides the calculated alias init
            case Some(s) => getInit(s) match {
              case Some(init) => calcInit(mList, calc + (m -> init), requestedCalc)
              case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
              case None => calcInit(s :: remaining, calc, requestedCalc + m)
            }
            //no connection => use the calculated aliased init
            case None =>
              val relVal = a.relValRef.get
              getInit(relVal) match {
                case Some(relInit) => calcInit(mList, calc + (m -> a.initFunc(relInit)), requestedCalc)
                case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
                case None => calcInit(relVal :: remaining, calc, requestedCalc + m)
              }
          }
          case rv@DFAny.NewVar(_,DFAny.Modifier.MatchRetVar, _, _) =>
            calcInit(mList, calc + (m -> Seq()), requestedCalc)
          case rv@DFAny.NewVar(_,DFAny.Modifier.IfRetVar, _, _) =>
//            val members = designDB.ownerMemberTable(rv.getOwner)
//            val cbs = members.collect{case m : ConditionalBlock.WithRetVal[_] if m.retVarRef.get == rv => m}
//            val ifConds : List[Either[(DFBool, DFAny), DFAny]] = cbs.collect {
//              case b : ConditionalBlock.IfBlock => Left(b.condRef.get, getRetVal(b))
//              case b : ConditionalBlock.ElseIfBlock => Left(b.condRef.get, getRetVal(b))
//              case b : ConditionalBlock.ElseBlock => Right(getRetVal(b))
//            }
//            DFBool.Token.select()
//            val ifInits : List[Either[(Option[Seq[DFBool.Token]], Option[Seq[DFAny.Token]]), Option[Seq[DFAny.Token]]]] = ifConds.map {
//              case Left((cond, retVal)) => Left(getInit(cond), getInit(retVal))
//              case Right(retVal) => Right(getInit(retVal))
//            }
            calcInit(mList, calc + (m -> Seq()), requestedCalc)
          case v : DFAny.Value[_,_] => v.modifier match { //Handles NewVar, Port.In, Port.Out
            //external init has priority over connection init
            case i : DFAny.Modifier.Initialized[_] => calcInit(mList, calc + (m -> i.externalInit), requestedCalc)
            case _ => designDB.getConnectionTo(v) match {
              //uses connection init
              case Some(s) => getInit(s) match {
                case Some(init) => calcInit(mList, calc + (m -> init), requestedCalc)
                case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
                case None => calcInit(s :: remaining, calc, requestedCalc + m)
              }
              //no connection and no external init, so use an empty sequence
              case None =>
                //TODO: add connection via alias init fetch support here
                calcInit(mList, calc + (m -> Seq()), requestedCalc)
            }
          }
        }
        case Nil => calc
      }
    }

    def calcInit : DFDesign.DB = {
      //we request init calculation for all members that can have initialization and currently do not have
      //a calculated init tag (the tag is empty).
      val calcMembers = designDB.members.collect{case v : DFAny if v.tags.init.isEmpty => v}
      val initMap = calcInit(calcMembers, Map(), Set())
      designDB.patch(initMap.toList.map{
        case (v, init) => v -> Patch.Replace(v.setTags(v.tags.setInit(init.asInstanceOf[Seq[v.TToken]])), Patch.Replace.Config.FullReplacement)
      })
    }
  }
}