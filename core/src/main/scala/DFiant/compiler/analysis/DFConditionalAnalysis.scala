package DFiant.compiler
package analysis
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFiant.compiler.ir.DFVal.Modifier
import DFiant.internals.*
import ir.*

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

extension (cb: DFConditional.Block)(using MemberGetSet)
  @tailrec def getFirstCB: DFConditional.Block = cb.prevBlockOrHeaderRef.get match
    case _: DFConditional.Header        => cb
    case prevBlock: DFConditional.Block => prevBlock.getFirstCB
  def getHeaderCB: DFConditional.Header =
    cb.getFirstCB.prevBlockOrHeaderRef.get.asInstanceOf[DFConditional.Header]
  def isFirstCB: Boolean = cb.prevBlockOrHeaderRef.get match
    case _: DFConditional.DFMatchHeader => true
    case _                              => false
  def getNextCB: Option[DFConditional.Block] =
    val refs = getSet.designDB.memberTable.getOrElse(cb, Set())
    // the conditional block is last if there is no reference to it as a previous block
    val cbTags: Set[ClassTag[_]] =
      Set(classTag[DFConditional.DFCaseBlock], classTag[DFConditional.DFIfElseBlock])
    refs
      .collectFirst {
        case r @ DFRef.TwoWay(originRef) if cbTags.contains(r.refType) => originRef.get
      }
      .collectFirst { case cb: DFConditional.Block => cb }
  def isLastCB: Boolean = getNextCB.isEmpty
//  @tailrec private def getPatterns(
//      casePattenBlock: DFConditional.CaseBlock,
//      patterns: List[DFAny.Pattern]
//  ): List[DFAny.Pattern] =
//    val updatedPattens = casePattenBlock.patternOption.toList ++ patterns
//    casePattenBlock.prevBlockRefOption match
//      case Some(r) => getPatterns(r.get, updatedPattens)
//      case None    => updatedPattens
  @tailrec private def getLeadingCBChain(
      block: DFConditional.Block,
      chain: List[DFConditional.Block]
  ): List[DFConditional.Block] =
    block.prevBlockOrHeaderRef.get match
      case prevBlock: DFConditional.Block =>
        getLeadingCBChain(prevBlock, prevBlock :: chain)
      case _ => chain
  def getLeadingChain: List[DFConditional.Block] = getLeadingCBChain(cb, List(cb))
  def isExhaustive: Boolean = true // cb match
//    case DFConditional.IfElseBlock(None, _, _, _)  => true // elsedf block
//    case DFConditional.CaseBlock(_, _, None, _, _) => true // casedf(?) block
//    case x: DFConditional.CaseBlock if x.isLastCB =>
//      val matchVal = x.matchHeaderRef.matchValRef.get
//      val patterns = getPatterns(x, List())
//      matchVal.dfType match
//        case dec: DFDecimal.Type[_, _, _]
//            if !dec.signed.getValue && dec.fractionWidth.getValue == 0 =>
//          val union =
//            patterns.asInstanceOf[List[DFDecimal.Pattern]].foldLeft(IntervalSet.empty[BigInt]) {
//              case (is, p) => is | p.patternSet
//            }
//          val fullRange = Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(matchVal.width))
//          union.contains(fullRange)
//        case dec: DFDecimal.Type[_, _, _]
//            if dec.signed.getValue && dec.fractionWidth.getValue == 0 =>
//          val union =
//            patterns.asInstanceOf[List[DFDecimal.Pattern]].foldLeft(IntervalSet.empty[BigInt]) {
//              case (is, p) => is | p.patternSet
//            }
//          val fullRange = Interval.closed(
//            BigInt.minSignedFromWidth(matchVal.width),
//            BigInt.maxSignedFromWidth(matchVal.width)
//          )
//          union.contains(fullRange)
//        case _: DFBits.Type[_] =>
//          val union =
//            patterns.asInstanceOf[List[DFBits.Pattern]].foldLeft(Set.empty[DFBits.Token]) {
//              case (s, p) => s | p.patternSet
//            }
//          union.size == BigInt.maxUnsignedFromWidth(matchVal.width).toInt + 1
//        case _: DFBool.Type =>
//          val union = patterns.asInstanceOf[List[DFBool.Pattern]].foldLeft(Set.empty[Boolean]) {
//            case (s, p) => s | p.patternSet
//          }
//          union.size == 2
//        case e: DFEnum.Type[_] =>
//          val union =
//            patterns.asInstanceOf[List[DFEnum.Pattern]].foldLeft(Set.empty[DFEnum.Entries.Entry]) {
//              case (s, p) => s | p.patternSet
//            }
//          union.size == e.entries.all.size
//      end match
//    case _ => false
  // Gets the topmost conditional header of an if/match chain.
  @tailrec private def getTopConditionalMember(
      currentBlock: DFConditional.Block
  ): DFConditional.Header =
    currentBlock.getOwnerBlock match
      case cb: DFConditional.Block => getTopConditionalMember(cb)
      case _                       => currentBlock.getHeaderCB
  def getTopConditionalMember: DFMember = getTopConditionalMember(cb)
end extension
