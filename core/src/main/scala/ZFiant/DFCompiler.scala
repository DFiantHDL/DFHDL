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
  implicit class CompilerOps[T : Compilable](t : T) {
    private val designDB : DFDesign.DB = Compilable[T].apply(t)
    def boom : DFDesign.DB = {
      ???
    }
  }

  implicit val fromDB : Compilable[DFDesign.DB] = t => t
  implicit val fromDFDesign : Compilable[DFDesign] = t => t.db
}

object DFCompiler {
  val delim : String = "  "
  implicit class Discovery(designDB : DFDesign.DB) {
    def discovery : DFDesign.DB = {
      ???
    }
  }

  implicit class Utils[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    import designDB.getset
    def fixAnonymous : DFDesign.DB = {
      val anonymizeList = designDB.ownerMemberList.flatMap {
        case (block, members) =>
          members.filterNot(m => m.isAnonymous).groupBy(m => m.tags.meta.namePosition).flatMap {
          //In case an anonymous member got a name from its owner. For example:
          //val ret = DFBits(8).ifdf(cond) {
          //  i & i
          //}
          //The `i & i` function would also get the `ret` name just as the if block itself
          case (pos, gm) if (pos == block.tags.meta.namePosition) => gm
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

  protected final case class AssignedScope(latest : immutable.BitSet, branchHistory : immutable.BitSet, parentScopeOption : Option[AssignedScope]) {
    def isConsumingPrevAt(consumeBitSet : immutable.BitSet) : Boolean = (consumeBitSet &~ latest).nonEmpty
    def assign(assignBitSet : immutable.BitSet) : AssignedScope = copy(latest = latest | assignBitSet)
    def branchEntry(firstBranch : Boolean) : AssignedScope = {
      val parentScope = if (firstBranch) this.copy(branchHistory = latest) else this
      AssignedScope(latest, immutable.BitSet(), Some(parentScope))
    }
    def branchExit(lastBranch : Boolean, exhaustive : Boolean) : AssignedScope = parentScopeOption match {
      case Some(parentScope) =>
        if (lastBranch) {
          if (exhaustive) AssignedScope(parentScope.latest | branchHistory, immutable.BitSet(), parentScope.parentScopeOption)
          else AssignedScope(parentScope.latest, immutable.BitSet(), parentScope.parentScopeOption)
        } else AssignedScope(parentScope.latest, parentScope.branchHistory & latest, parentScope.parentScopeOption)
      case None => this
    }
  }
  protected object AssignedScope {
    val empty : AssignedScope = AssignedScope(immutable.BitSet(), immutable.BitSet(), None)
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
          case r : ConditionalBlock.PrevBlockRef[_] => Some(r)
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
      val access = immutable.BitSet.empty ++ (relBitLow until relWidth)
      value match {
        case DFAny.Alias.AsIs(_,_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
        case DFAny.Alias.Invert(_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, assignMap, currentSet)
        case DFAny.Alias.BitsWL(_,rv,rw,rbl,_,_) => consumeFrom(rv.get, rw, relBitLow + rbl, assignMap, currentSet)
        case x if x.modifier.isInstanceOf[DFAny.Modifier.Assignable] =>
          val scope = assignMap.getOrElse(value, AssignedScope.empty)
          if (scope.isConsumingPrevAt(access)) currentSet union Set(value) else currentSet
        case _ => currentSet
      }
    }
    private def consumeFrom(value : DFAny, assignMap : Map[DFAny, AssignedScope], currentSet : Set[DFAny]) : Set[DFAny] =
      consumeFrom(value, value.width, 0, assignMap, currentSet)

    @tailrec private def assignTo(value : DFAny, relWidth : Int, relBitLow : Int, assignMap : Map[DFAny, AssignedScope]) : Map[DFAny, AssignedScope] = {
      val access = immutable.BitSet.empty ++ (relBitLow until relWidth)
      value match {
        case DFAny.Alias.AsIs(_,_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
        case DFAny.Alias.Invert(_,rv,_,_) => assignTo(rv.get, relWidth, relBitLow, assignMap)
        case DFAny.Alias.BitsWL(_,rv,rw,rbl,_,_) => assignTo(rv.get, relWidth, rbl + relBitLow, assignMap)
        case x => assignMap.assignTo(x, access)
      }
    }
    implicit class ScopeMap(sm : Map[DFAny, AssignedScope]) {
      def assignTo(toVal : DFAny, assignBitSet : immutable.BitSet) : Map[DFAny, AssignedScope] = {
        val scope = sm.getOrElse(toVal, AssignedScope.empty)
        sm + (toVal -> scope.assign(assignBitSet))
      }
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
              println(s"entering $cb", cb.isFirstCB)
              scopeMap.branchEntry(cb.isFirstCB)
            case _ => scopeMap
          }
          getImplicitPrevVars(rs, nextBlock, updatedScopeMap, currentSet)
        case r :: _ if r.ownerRef.get != currentBlock => //existing child block
          val updatedScopeMap = currentBlock match {
            case cb : ConditionalBlock =>
              println(s"exiting $cb", cb.isLastCB, cb.isExhaustive)
              scopeMap.branchExit(cb.isLastCB, cb.isExhaustive)
            case _ => scopeMap
          }
          getImplicitPrevVars(remaining, currentBlock.getOwner, updatedScopeMap, currentSet)
        case r :: rs => //checking member consumers
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
            case _ =>
              (currentSet, scopeMap)
          }
          getImplicitPrevVars(rs, currentBlock, updatedScopeMap, updatedSet)
        case Nil =>
          currentSet
      }
    }
    def explicitPrev : DFDesign.DB = {
      val explicitPrevSet = getImplicitPrevVars(designDB.members.drop(1), designDB.top, Map(), Set())
      println(explicitPrevSet.map(e => e.getFullName).mkString(", "))
      designDB
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

  sealed trait PrintConfig
  object PrintConfig {
    implicit case object Default extends PrintConfig
    case object ShowInits extends PrintConfig
  }
  final implicit class CodeString[C](c : C)(implicit comp : Compilable[C]) {
    private val designDB = comp(c)
    private val fixedDB = designDB.fixAnonymous
    import fixedDB.getset

    def blockBodyCodeString(block : DFBlock, members : List[DFMember])(implicit printConfig : PrintConfig) : String = {
      val membersCodeString = members.flatMap {
        case mh : ConditionalBlock.MatchHeader => Some(mh.codeString)
        case cb : ConditionalBlock => Some(cb.codeString(blockBodyCodeString(cb, fixedDB.ownerMemberTable(cb))))
        case DFDesign.Block.Internal(_,_,Some(_)) => None
        case d : DFDesign.Block => Some(s"final val ${d.name} = new ${d.typeName} {}") //TODO: fix
        case n : DFNet => n.toRef.get.getOwner match {
          case DFDesign.Block.Internal(_,_,Some(_)) => None //ignoring inlined block connection
          case _ => Some(n.codeString)
        }
        case a : DFAny if !a.isAnonymous =>
          val initInfo = printConfig match {
            case PrintConfig.Default => ""
            case PrintConfig.ShowInits => a.tags.init match {
              case Some(init) => s"//init = ${init.codeString}"
              case None => "//init = Unknown"
            }
          }
          Some(s"final val ${a.name} = ${a.codeString}$initInfo")
        case _ => None
      }
      membersCodeString.mkString("\n")
    }
    def codeString(implicit printConfig : PrintConfig) : String = {
      val bodyDB = new DSLOwnerConstruct.DB[DFBlock, String]{
        override def ownerToString(ownerTypeName: String, ownerBody: String): String =
          s"trait $ownerTypeName extends DFDesign {\n${ownerBody.delimRowsBy(delim)}\n}"
      }
      fixedDB.ownerMemberList.foreach {
        case (DFDesign.Block.Internal(_,_,Some(_)), _) =>
        case (block : DFDesign.Block, members) =>
          bodyDB.addOwnerBody(block.typeName, blockBodyCodeString(block, members), block)
        case _ =>
      }
      bodyDB.dbString
    }
    def printCodeString()(implicit printConfig : PrintConfig) : DFDesign.DB = {
      println(codeString)
      fixedDB
    }
  }
}