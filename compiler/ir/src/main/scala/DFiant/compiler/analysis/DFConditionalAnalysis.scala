package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import DFConditional.DFCaseBlock.Pattern
import DFVal.Modifier

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
  // returns Some(true/false) if the is conditional structure is known to be exhaustive or not.
  // returns None if the coverage is not known.
  def isExhaustive: Option[Boolean] = cb match
    case ib: DFConditional.DFIfElseBlock if ib.guardRef.get == DFMember.Empty =>
      Some(true) // else block
    case DFConditional.DFCaseBlock(Pattern.CatchAll, gr, _, _, _, _) if gr.get == DFMember.Empty =>
      Some(true) // case _ => block
    case cb: DFConditional.Block if cb.guardRef.get != DFMember.Empty =>
      None // if not all guards are empty, then we cannot know if the coverage is exhaustive
    // Now we need to check various cases for their patterns
    case x: DFConditional.DFCaseBlock if x.isLastCB =>
      val header = x.getHeaderCB.asInstanceOf[DFConditional.DFMatchHeader]
      val selectorVal = header.selectorRef.get
      val cases = getLeadingChain.asInstanceOf[List[DFConditional.DFCaseBlock]]
      // Just checking singleton token patterns.
      // If we stumble upon more complex patterns, we return None to
      lazy val tokenSet = cases.view
        .map(_.pattern)
        .flattenPatterns // getting rid of the pattern alternative
        .collect {
          case Pattern.Singleton(token) => token
          case _                        => return None
        }
        .toSet
      selectorVal.dfType match
        case DFBits(width) =>
          if (tokenSet.exists(_.isBubble)) None // currently not checking don't-care patterns
          else Some((1 << width) == tokenSet.size)
        case dec: DFDecimal =>
          // A decimal is considered covered when all its values are covered.
          // All the possible values are determined by the width of the decimal.
          Some((1 << dec.width) == tokenSet.size)
        case DFEnum(name, width, entries) =>
          // An enum is considered covered when all its entries are covered.
          // Since both token set and entries set are unique and type checking
          // already confirmed, then we can safely assume that everything is
          // covered when both set sizes are the same.
          Some(entries.size == tokenSet.size)
        case _ => None
      end match
    case _ => Some(false)
  // Gets the topmost conditional header of an if/match chain.
  @tailrec private def getTopConditionalHeader(
      currentBlock: DFConditional.Block
  ): DFConditional.Header =
    currentBlock.getOwnerBlock match
      case cb: DFConditional.Block => getTopConditionalHeader(cb)
      case _                       => currentBlock.getHeaderCB

  def getTopConditionalHeader: DFConditional.Header = getTopConditionalHeader(cb)
end extension

extension (patterns: Iterable[Pattern])
  def flattenPatterns: Iterable[Pattern] = patterns.flatMap {
    case Pattern.Alternative(list) => list.flattenPatterns
    case p                         => Some(p)
  }
