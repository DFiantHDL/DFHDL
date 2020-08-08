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

  implicit class MatchHeaderAnalysis[MVType <: DFAny.Type](mh : ConditionalBlock.MatchHeader)(implicit getSet: MemberGetSet) {
    import ConditionalBlock.CaseBlock
    def getCases : Iterable[CaseBlock] = {
      getSet.designDB.blockMemberTable(mh.getOwnerBlock).dropWhile(_ != mh).drop(1).takeWhile {
        case _ : CaseBlock => true
      }.collect {
        case cb : CaseBlock => cb
      }
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
