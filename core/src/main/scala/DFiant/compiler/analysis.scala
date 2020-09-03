package DFiant
package compiler

import scala.annotation.tailrec
import internals._

object analysis {
  type AssignmentTable = Map[DFAny.Member, AssignmentRev]
  //DFMember = DFNet | DFAny.Dynamic.DontConsume | DFAny.Dynamic.Consume
  type AssignmentRev = Vector[AssignmentSource]
  type AssignmentSource = List[SourceElement]
  final case class SourceElement(
    dfVal : DFAny.Member, relBitHigh : Int, relBitLow : Int, reversed : Boolean, inverted : Boolean, prevStep : Int
  )

  final implicit class DFAnyAnalysis(value : DFAny.Member)(implicit getSet: MemberGetSet) {
    @tailrec def dealias : DFAny.Member = {
      value match {
        case alias : DFAny.Alias => alias.relValRef.get.dealias
        case v : DFAny.Member => v
      }
    }
    //true if and only is is assigned at any of its dealiasing stages
    @tailrec def isNonAliasAssigned : Boolean = {
      value match {
        case alias : DFAny.Alias =>
          if (getSet.designDB.getAssignmentsTo(alias).nonEmpty) true
          else alias.relValRef.get.isNonAliasAssigned
        case v : DFAny.Member => getSet.designDB.getAssignmentsTo(v).nonEmpty
      }
    }
  }

  final implicit class MatchHeaderAnalysis(mh : ConditionalBlock.MatchHeader)(implicit getSet: MemberGetSet) {
    import ConditionalBlock.CaseBlock
    def getCases : Iterable[CaseBlock] = {
      getSet.designDB.blockMemberTable(mh.getOwnerBlock).dropWhile(_ != mh).drop(1).takeWhile {
        case _ : CaseBlock => true
        case _ => false
      }.collect {
        case cb : CaseBlock => cb
      }
    }

    @tailrec def caseOwnerOf(member : DFMember) : Option[CaseBlock] = member match {
      case cb : CaseBlock if cb.matchHeaderRef.get == mh => Some(cb)
      case _ : DFDesign.Block => None
      case _ => caseOwnerOf(member.getOwnerBlock)
    }
    def anyCaseContains(member : DFMember) : Boolean = caseOwnerOf(member).isDefined
    //gets all the assignments within the match statement (at any level)
    def getAssignments : Iterable[DFNet.Assignment] = {
      getSet.designDB.members.toIterable
        .dropWhile(_ != mh).drop(1) //reaching the match header
        .takeWhile(anyCaseContains) //taking all case members
        .collect{case n : DFNet.Assignment => n} //collecting assigned values
    }
    //gets all the assigned variables within the match statement (at any level), but defined externally
    def getExternalAssignedVars : Iterable[DFAny.Member] = {
      val assignments = getAssignments
      val casesNum = getCases.size
      assignments
        .map(a => (a.toRef.get, caseOwnerOf(a).get)) //map to (toVar, caseOwner)
        .filterNot(x => anyCaseContains(x._1)) //filtering out variables defined inside the cases
        .groupBy(x => x._1) //group with toVar
        .map(x => (x._1, x._2.map(_._2).toSet)) //map to (toVar -> Set[CaseBlock))
        .filter(x => x._2.size == casesNum) //keeping only variables assigned in all cases (fully covered)
        .keys
    }
  }

  implicit class ConditionalBlockAnalysis(cb : ConditionalBlock.Owner)(implicit getSet: MemberGetSet) {
    def isFirstCB : Boolean = cb.prevBlockRefOption match {
      case None => true
      case _ => false
    }
    def getNextCB : Option[ConditionalBlock.Owner] = {
      val refs = getSet.designDB.memberTable.getOrElse(cb, Set())
      //the conditional block is last if there is no reference to it as a previous block
      refs.collectFirst {
        case r @ ConditionalBlock.PrevBlockRefOption() => r.owner.get
      }.collectFirst {
        case cb : ConditionalBlock.Owner => cb
      }
    }
    def isLastCB : Boolean = getNextCB.isEmpty
    @tailrec private def getPatterns(casePattenBlock : ConditionalBlock.CaseBlock, patterns : List[DFAny.Pattern]) : List[DFAny.Pattern] = {
      val updatedPattens = casePattenBlock.patternOption.toList ++ patterns
      casePattenBlock.prevBlockRefOption match {
        case Some(r) => getPatterns(r.get, updatedPattens)
        case None => updatedPattens
      }
    }
    def isExhaustive : Boolean = cb match {
      case ConditionalBlock.IfElseBlock(None,_,_,_) => true //elsedf block
      case ConditionalBlock.CaseBlock(_,_,None,_,_) => true //casedf_ block
      case x : ConditionalBlock.CaseBlock if x.isLastCB =>
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
            val union = patterns.asInstanceOf[List[DFBits.Pattern]].foldLeft(Set.empty[DFBits.Token]){case (s, p) => s | p.patternSet}
            union.size == BigInt.maxUnsignedFromWidth(matchVal.width).toInt + 1
          case _ : DFBool.Type =>
            val union = patterns.asInstanceOf[List[DFBool.Pattern]].foldLeft(Set.empty[Boolean]){case (s, p) => s | p.patternSet}
            union.size == 2
          case e : DFEnum.Type[_] =>
            val union = patterns.asInstanceOf[List[DFEnum.Pattern]].foldLeft(Set.empty[EnumType.Entry]){case (s, p) => s | p.patternSet}
            union.size == e.enumType.entries.size
        }
      case _ => false
    }
  }

}
