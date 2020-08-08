package DFiant
package compiler

import scala.annotation.tailrec
import internals._

object analysis {
  type AssignmentTable = Map[DFAny, AssignmentRev]
  //DFMember = DFNet | DFAny.Dynamic.DontConsume | DFAny.Dynamic.Consume
  type AssignmentRev = Vector[AssignmentSource]
  type AssignmentSource = List[SourceElement]
  final case class SourceElement(
    dfVal : DFAny, relBitHigh : Int, relBitLow : Int, reversed : Boolean, inverted : Boolean, prevStep : Int
  )

  implicit class DFAnyAnalysis(value : DFAny)(implicit getSet: MemberGetSet) {
    @tailrec final def dealias : DFAny = {
      value match {
        case alias : DFAny.Alias[_,_,_] => alias.relValRef.get.dealias
        case v : DFAny => v
      }
    }
  }

  implicit class MatchHeaderAnalysis(mh : ConditionalBlock.MatchHeader)(implicit getSet: MemberGetSet) {
    import ConditionalBlock.CaseBlock
    def getCases : Iterable[CaseBlock] = {
      getSet.designDB.blockMemberTable(mh.getOwnerBlock).dropWhile(_ != mh).drop(1).takeWhile {
        case _ : CaseBlock => true
        case _ => false
      }.collect {
        case cb : CaseBlock => cb
      }
    }

    @tailrec final def anyCaseContains(member : DFMember) : Boolean = member match {
      case cb : CaseBlock if cb.matchHeaderRef.get == mh => true
      case _ : DFDesign.Block => false
      case _ => anyCaseContains(member.getOwnerBlock)
    }
    //gets all the assigned variables within the match statement (at any level)
    def getAssignedVars : Iterable[DFAny.VarOf[DFAny.Type]] = {
      getSet.designDB.members.toIterable
        .dropWhile(_ != mh).drop(1) //reaching the match header
        .takeWhile(anyCaseContains) //taking all case members
        .collect{case n : DFNet.Assignment => n.toRef.get} //collecting assigned values
        .map(_.dealias.asInstanceOf[DFAny.VarOf[DFAny.Type]]) //dealiasing and casting
        .distinct
    }
    //gets all the assigned variables within the match statement (at any level), but defined externally
    def getExternalAssignedVars : Iterable[DFAny.VarOf[DFAny.Type]] =
      getAssignedVars.filterNot(anyCaseContains)
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
