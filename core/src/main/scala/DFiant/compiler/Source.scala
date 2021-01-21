package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter.FormatString
import SourceElement.ListOps

import scala.collection.immutable.ListMap
final case class SourceValue(member : DFAny.Member, version : SourceVersion) {
  def codeString(implicit dependencyContext: DependencyContext) : String = {
    import dependencyContext.getSet
    import CSPrinter.defaultPrinter
    val memberStr = member match {
      case func : DFAny.Func1 =>
        s"${dependencyContext.dependencyMap(func.leftArgRef).codeString}.${func.op}"
      case func : DFAny.Func2 =>
        s"${dependencyContext.dependencyMap(func.leftArgRef).codeString} ${func.op} ${dependencyContext.dependencyMap(func.rightArgRef).codeString}"
      case _ =>
        if (member.isAnonymous) member.codeString.uncolor else member.getFullName
    }
    version match {
      case SourceVersion.Empty => s"$memberStr$$EMPTY(${member.width})$$"
      case SourceVersion.Idx(block, idx) =>
        val versionMap = member match {
          case dcl : DFAny.Dcl => dependencyContext.assignmentMap(dcl)(block)
          case _ => ???
        }
        val idxStr =
          if (versionMap.size > 1) s"$$V$idx"
          else ""
        val blockStr = block match {
          case _ : DFDesign.Block => ""
          case cb : DFConditional.Block => cb match {
            case DFConditional.IfElseBlock(Some(condRef), None, _, _) =>
              s"$$ifdf(${dependencyContext.dependencyMap(condRef).codeString})"
            case DFConditional.IfElseBlock(Some(condRef), Some(_), _, _) =>
              s"$$elseifdf(${dependencyContext.dependencyMap(condRef).codeString})"
            case DFConditional.IfElseBlock(None, Some(_), _, _) =>
              s"$$elsedf"
            case DFConditional.CaseBlock(_, _, Some(pattern), _, _) =>
              s"$$casedf(${pattern.codeString})"
            case DFConditional.CaseBlock(_, _, None, _, _) =>
              s"$$casedf_"
            case _ => ??? //not possible
          }
        }
        s"$memberStr$blockStr$idxStr"
      case SourceVersion.Latest => memberStr
      case SourceVersion.IfElse(_,branchVersions,fallbackVersion) =>
        val branchVersionStr = branchVersions.map(bv => (bv._1.codeString, SourceValue(member, bv._2).codeString))
        val fallBackStr = SourceValue(member, fallbackVersion).codeString
        s"$memberStr$$ifdf($branchVersionStr, $fallBackStr){...}"
      case SourceVersion.Match(_,caseVersions,fallbackVersion) =>
        val caseVersionStr = caseVersions.map(bv => (bv._1.codeString, SourceValue(member, bv._2).codeString))
        val fallBackStr = fallbackVersion.map(fb => SourceValue(member, fb).codeString)
        s"$memberStr$$casedf($caseVersionStr, $fallBackStr){...}"
    }

  }
}

final case class SourceElement(
  srcVal : SourceValue, withInit : Boolean, relBitHigh : Int, relBitLow : Int
) { left =>
  val relWidth: Int = relBitHigh - relBitLow + 1
  assert(relWidth > 0)
  val dfType : DFAny.Type = if (relWidth == srcVal.member.width) srcVal.member.dfType else DFBits.Type(relWidth)
  def bits(relBitHigh : Int, relBitLow : Int) : List[SourceElement] =
    if (relBitHigh < relBitLow) Nil
    else List(copy(relBitHigh = this.relBitLow + relBitHigh, relBitLow = this.relBitLow + relBitLow))
  def concat (elements : List[SourceElement]) : List[SourceElement] = elements match {
    case ::(right, rightNext) =>
      //two elements can be coalesced when they match all their arguments except for the bit indexing.
      //the bit indexing needs to be a continuation from the left element's MSbit to the right element's LSbit.
      //The left and right be reversed, so that should be checked as well to match, but with an exception
      //if the left or right elements width are 1, since then reversing has no effect.
      val matchingArguments =
        right.srcVal == left.srcVal && right.withInit == left.withInit
      val coalesceNormal = (left.relWidth == 1) && (right.relWidth == 1)

      if (matchingArguments && coalesceNormal && left.relBitLow == right.relBitHigh + 1)
        SourceElement(srcVal, withInit, left.relBitHigh, right.relBitLow) :: rightNext
      else left :: elements //cannot be coalesced, so this element is just concatenated to the rest
    case Nil => List(left)
  }
  def versioned(dcl : DFAny.Dcl, version : SourceVersion) : SourceElement =
    copy(srcVal = SourceValue(dcl, version))
  def codeString(implicit dependencyContext: DependencyContext) : String = {
    import dependencyContext.getSet
    import CSPrinter.defaultPrinter
    import srcVal.member
    val srcStr = srcVal.codeString
    if (member.width == relWidth) srcStr.applyBrackets()
    else s"${srcStr.applyBrackets()}($relBitHigh, $relBitLow)"
  }
}
object SourceElement {
  def apply(member : DFAny.Member, version : SourceVersion, withInit : Boolean): SourceElement =
    SourceElement(SourceValue(member, version), withInit, member.width-1, 0)

  final implicit class ListOps(elements : List[SourceElement]) {
    def concat (that : List[SourceElement]) : List[SourceElement] = {
      elements match {
        case leftInit :+ left => leftInit ++ (left concat that)
        case Nil => that
        case _ => ???
      }
    }
  }
}

sealed trait SourceVersion extends Product with Serializable
object SourceVersion {
  case object Empty extends SourceVersion
  final case class Idx(block : DFBlock, idx : Int) extends SourceVersion
  final case class IfElse(
      headBranch: DFConditional.IfElseBlock,
      branchVersions: List[(Source, SourceVersion)],
      fallbackVersion: SourceVersion
  ) extends SourceVersion
  final case class Match(
      headCase: DFConditional.CaseBlock,
      caseVersions: List[(DFAny.Pattern, SourceVersion)],
      fallbackVersion: Option[SourceVersion]
  ) extends SourceVersion
  case object Latest extends SourceVersion
}

final case class Source(elements : List[SourceElement]) { lhs =>
  val width : Int = elements.map(v => v.relWidth).sum
  def ++ (src : Source) : Source = Source(elements concat src.elements)
  //returns a tuple (elementIdx, relBitIdx)
  //elementIdx - position within the elements list
  //relBitIdx - position withing the element selected by elementIdx
  private def bitIdxToElementIdx(bitIdx: Int): (Int, Int) = {
    var relBitLow = width
    elements.zipWithIndex
      .map {
        case (e, i) =>
          relBitLow = relBitLow - e.relWidth
          (i, bitIdx - relBitLow)
      }
      .iterator
      .dropWhile(_._2 < 0)
      .next()
  }

  def bits(relBitHigh: Int, relBitLow: Int): Source = {
    val (highElemIdx, highRelIdx) = bitIdxToElementIdx(relBitHigh)
    val (lowElemIdx, lowRelIdx)   = bitIdxToElementIdx(relBitLow)
    val dfType                    = DFBits.Type(relBitHigh - relBitLow + 1)
    if (highElemIdx == lowElemIdx) { //same element
      val (_, selElem :: _) = elements.splitAt(highElemIdx)
      Source(selElem.bits(highRelIdx, lowRelIdx))
    } else {
      val (left, lowElem :: _)         = elements.splitAt(lowElemIdx)
      val (_, highElem :: midElements) = left.splitAt(highElemIdx)
      val highSel = highElem.bits(highRelIdx, 0)
      val lowSel = lowElem.bits(lowElem.relWidth - 1, lowRelIdx)
      Source(highSel concat midElements concat lowSel)
    }
  }
  def bitsWL(relWidth : Int, relBitLow : Int) : Source = bits(relBitLow + relWidth - 1, relBitLow)

  def assign(
      src: Source,
      relBitHigh: Int,
      relBitLow: Int,
      withInit: Boolean
  ): Source = {
    assert(src.width == relBitHigh - relBitLow + 1)
    val (highElemIdx, highRelIdx) = bitIdxToElementIdx(relBitHigh)
    val (lowElemIdx, lowRelIdx)   = bitIdxToElementIdx(relBitLow)
    val srcElements =
      if (!withInit) src.elements.map(e => e.copy(withInit = false))
      else src.elements
    if (highElemIdx == lowElemIdx) { //same element
      val (leftElements, selElem :: rightElements) =
        elements.splitAt(highElemIdx)
      val keptLeft =
        leftElements concat selElem.bits(selElem.relWidth - 1, highRelIdx + 1)
      val keptRight = selElem.bits(lowRelIdx - 1, 0) concat rightElements
      src.copy(elements = keptLeft concat srcElements concat keptRight)
    } else {
      val (left, lowElem :: rightElements) = elements.splitAt(lowElemIdx)
      val (leftElements, highElem :: _)    = left.splitAt(highElemIdx)
      val keptLeft =
        leftElements concat highElem.bits(highElem.relWidth - 1, highRelIdx + 1)
      val keptRight = lowElem.bits(lowRelIdx - 1, 0) concat rightElements
      src.copy(elements = keptLeft concat srcElements concat keptRight)
    }
  }
  def assignWL(
      src: Source,
      relWidth: Int,
      relBitLow: Int,
      withInit: Boolean
  ): Source =
    assign(src, relBitLow + relWidth - 1, relBitLow, withInit)

  def codeString(implicit dependencyContext: DependencyContext) : String = {
    elements.map(_.codeString.applyBrackets()).mkString(" ++ ")
  }
}
object Source {
  def apply(element : SourceElement) : Source = Source(element :: Nil)
  def apply(member : DFAny.Member, version: SourceVersion, withInit : Boolean) : Source =
    Source(SourceElement(member, version, withInit))
  object Empty {
    def apply(dcl: DFAny.Dcl): Source =
      Source(dcl, SourceVersion.Empty, withInit = true)
  }
  object Idx {
    def apply(dcl: DFAny.Dcl, block: DFBlock, idx: Int): Source =
      Source(dcl, SourceVersion.Idx(block, idx), withInit = true)
  }
  object IfElse {
    def apply(
        dcl: DFAny.Dcl,
        headBranch: DFConditional.IfElseBlock,
        branchVersions: List[(Source, SourceVersion)],
        fallbackVersion: SourceVersion,
        withInit: Boolean
    ): Source =
      Source(
        dcl,
        SourceVersion.IfElse(headBranch, branchVersions, fallbackVersion),
        withInit
      )
  }
  object Match {
    def apply(
        dcl: DFAny.Dcl,
        headCase: DFConditional.CaseBlock,
        caseVersions: List[(DFAny.Pattern, SourceVersion)],
        fallbackVersion: Option[SourceVersion],
        withInit: Boolean
    ): Source =
      Source(
        dcl,
        SourceVersion.Match(headCase, caseVersions, fallbackVersion),
        withInit
      )
  }
  object Latest {
    def apply(member : DFAny.Member) : Source = Source(member, SourceVersion.Latest, withInit = true)
  }
}
