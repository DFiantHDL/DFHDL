package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter
import DFiant.compiler.printer.formatter.FormatString
import SourceElement.ListOps

import scala.collection.immutable.ListMap

final case class SourceElement(
  srcVal : SourceValue, withInit : Boolean,
  relBitHigh : Int, relBitLow : Int, reversed : Boolean, inverted : Boolean, prevStep : Int, pipeStep : Int
) { left =>
  val relWidth : Int = relBitHigh - relBitLow + 1
  assert(relWidth > 0)
  val dfType : DFAny.Type = if (relWidth == srcVal.width) srcVal.dfType else DFBits.Type(relWidth)
  def invert : SourceElement = copy(inverted = !inverted)
  def reverse : SourceElement = copy(reversed = !reversed)
  def prev(step : Int) : SourceElement = copy(prevStep = prevStep + step)
  def pipe(step : Int) : SourceElement = copy(pipeStep = pipeStep + step)
  def versioned(dcl : DFAny.Dcl, version : SourceVersion) : SourceElement =
    copy(srcVal = SourceValue.Dcl(dcl, version))
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
        right.srcVal == left.srcVal && right.withInit == left.withInit && right.inverted == left.inverted &&
          right.prevStep == left.prevStep && right.pipeStep == left.pipeStep
      val coalesceNormal = ((!left.reversed || left.relWidth == 1) && (!right.reversed || right.relWidth == 1))
      val coalesceReverse = ((left.reversed || left.relWidth == 1) && (right.reversed || right.relWidth == 1))

      if (matchingArguments && coalesceNormal && left.relBitLow == right.relBitHigh + 1)
        SourceElement(
          srcVal, withInit, left.relBitHigh, right.relBitLow, reversed = false, inverted, prevStep, pipeStep
        ) :: rightNext
      else if (matchingArguments && coalesceReverse && left.relBitHigh == right.relBitLow - 1)
        SourceElement(
          srcVal, withInit, right.relBitHigh, left.relBitLow, reversed = true, inverted, prevStep, pipeStep
        ) :: rightNext
      else left :: elements //cannot be coalesced, so this element is just concatenated to the rest
    case Nil => List(left)
  }
  def propertiesFrom(element : SourceElement) : SourceElement = copy(
    reversed = reversed ^ element.reversed, inverted = inverted ^ element.inverted,
    prevStep = prevStep + element.prevStep
  )
  def codeString(implicit dependencyContext: DependencyContext) : String = {
    import dependencyContext.getSet
    import CSPrinter.defaultPrinter
    val srcStr = srcVal.codeString
    val bitSelStr =
      if (srcVal.width == relWidth) srcStr.applyBrackets()
      else s"${srcStr.applyBrackets()}($relBitHigh, $relBitLow)"
    val reverseStr = if (reversed) s"$bitSelStr.reverse" else bitSelStr
    val invertStr = if (inverted) s"$reverseStr.invert" else reverseStr
    val prevStr = if (prevStep > 0) s"$invertStr.prev($prevStep)" else invertStr
    if (pipeStep > 0) s"$prevStr.pipe($prevStep)" else prevStr
  }
}
object SourceElement {
  def apply(srcVal : SourceValue, withInit : Boolean): SourceElement =
    SourceElement(srcVal, withInit, srcVal.width-1, 0, reversed = false, inverted = false, prevStep = 0, pipeStep = 0)
  def apply(dcl : DFAny.Dcl, sourceVersion: SourceVersion, withInit : Boolean) : SourceElement =
    SourceElement(SourceValue.Dcl(dcl, sourceVersion), withInit)
  final implicit class ListOps(elements : List[SourceElement]) {
    def concat (that : List[SourceElement]) : List[SourceElement] = {
      elements match {
        case leftInit :+ left => leftInit ++ (left concat that)
        case Nil => that
      }
    }
  }
}

sealed trait SourceVersion extends Product with Serializable
object SourceVersion {
  case object Empty extends SourceVersion
  final case class Idx(block : DFBlock, idx : Int) extends SourceVersion
  case object Latest extends SourceVersion
  final case class IfElse(
    headBranch : DFConditional.IfElseBlock,
    branchVersions : List[(Source, SourceVersion)],
    fallbackVersion : SourceVersion
  ) extends SourceVersion
  final case class Match(
    headCase : DFConditional.CaseBlock,
    caseVersions : List[(DFAny.Pattern, SourceVersion)],
    fallbackVersion : Option[SourceVersion]
  ) extends SourceVersion
}
sealed abstract class SourceValue(val dfType : DFAny.Type) extends Product with Serializable {
  val width : Int = dfType.width
  def codeString(implicit dependencyContext: DependencyContext, printer: CSPrinter) : String
}
object SourceValue {
  final case class Const(token : DFAny.Token) extends SourceValue(token.dfType) {
    def codeString(implicit dependencyContext: DependencyContext, printer: CSPrinter) : String =
      s"CONST(${token.codeString.unformatted})"
  }
  final case class Dcl(dcl : DFAny.Dcl, version : SourceVersion) extends SourceValue(dcl.dfType) {
    def codeString(implicit dependencyContext: DependencyContext, printer: CSPrinter) : String = {
      import dependencyContext.getSet
      version match {
        case SourceVersion.Empty => s"${dcl.getFullName}$$EMPTY"
        case SourceVersion.Idx(block, idx) =>
          val versionMap = dependencyContext.assignmentMap(dcl)(block)
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
          s"${dcl.getFullName}$blockStr$idxStr"
        case SourceVersion.Latest => dcl.getFullName
        case SourceVersion.IfElse(_,branchVersions,fallbackVersion) =>
          val branchVersionStr = branchVersions.map(bv => (bv._1.codeString, SourceValue.Dcl(dcl, bv._2).codeString))
          val fallBackStr = SourceValue.Dcl(dcl, fallbackVersion).codeString
          s"${dcl.getFullName}$$ifdf($branchVersionStr, $fallBackStr){...}"
        case SourceVersion.Match(_,caseVersions,fallbackVersion) =>
          val caseVersionStr = caseVersions.map(bv => (bv._1.codeString, SourceValue.Dcl(dcl, bv._2).codeString))
          val fallBackStr = fallbackVersion.map(fb => SourceValue.Dcl(dcl, fb).codeString)
          s"${dcl.getFullName}$$casedf($caseVersionStr, $fallBackStr){...}"
      }
    }
  }
  final case class Func2(dfVal : DFAny.Func2) extends SourceValue(dfVal.dfType) {
    def codeString(implicit dependencyContext: DependencyContext, printer: CSPrinter) : String = {
      val leftStr = dependencyContext.dependencyMap(dfVal.leftArgRef).codeString.applyBrackets()
      val rightStr = dependencyContext.dependencyMap(dfVal.rightArgRef).codeString.applyBrackets()
      s"$leftStr ${dfVal.op} $rightStr"
    }
  }
  final case class ApplySel(override val dfType : DFAny.Type, relSrc : Source, idxSrc : Source) extends SourceValue(dfType) {
    def codeString(implicit dependencyContext: DependencyContext, printer: CSPrinter) : String = {
      s"${relSrc.codeString}(${idxSrc.codeString})"
    }
  }
}

final case class Source(dfType : DFAny.Type, elements : List[SourceElement]) { lhs =>
  val width : Int = elements.map(v => v.relWidth).sum
  def invert : Source = Source(dfType, elements.map(e => e.invert))
  def prev(step : Int) : Source = Source(dfType, elements.map(e => e.prev(step)))
  def pipe(step : Int) : Source = Source(dfType, elements.map(e => e.pipe(step)))
  def reverse : Source = Source(dfType, elements.reverse.map(e => e.reverse))
  def as(dfType : DFAny.Type) : Source = copy(dfType = dfType)
  def ++ (src : Source) : Source =
    Source(DFBits.Type(width + src.width), elements concat src.elements)
  //returns a tuple (elementIdx, relBitIdx)
  //elementIdx - position within the elements list
  //relBitIdx - position withing the element selected by elementIdx
  private def bitIdxToElementIdx(bitIdx : Int) : (Int, Int) = {
    var relBitLow = width
    elements.zipWithIndex.map {case (e, i) =>
      relBitLow = relBitLow - e.relWidth
      (i, bitIdx - relBitLow)
    }.iterator.dropWhile(_._2 < 0).next()
  }

  def bits(relBitHigh : Int, relBitLow : Int) : Source = {
    val (highElemIdx, highRelIdx) = bitIdxToElementIdx(relBitHigh)
    val (lowElemIdx, lowRelIdx) = bitIdxToElementIdx(relBitLow)
    val dfType = DFBits.Type(relBitHigh - relBitLow + 1)
    if (highElemIdx == lowElemIdx) { //same element
      val (_, selElem :: _) = elements.splitAt(highElemIdx)
      Source(dfType, selElem.bits(highRelIdx, lowRelIdx))
    } else {
      val (left, lowElem :: _) = elements.splitAt(lowElemIdx)
      val (_, highElem :: midElements) = left.splitAt(highElemIdx)
      val highSel = highElem.bits(highRelIdx, 0)
      val lowSel = lowElem.bits(lowElem.relWidth - 1, lowRelIdx)
      Source(dfType, highSel concat midElements concat lowSel)
    }
  }
  def bitsWL(relWidth : Int, relBitLow : Int) : Source = bits(relBitLow + relWidth - 1, relBitLow)
  def resize(dfType : DFAny.Type) : Source = {
    if (dfType.width < this.width) bits(dfType.width-1, 0).as(dfType)
    else if (dfType.width > this.width) {
      val deltaWidth = dfType.width - this.width
      dfType match {
        case DFSInt.Type(_) =>
          Source(dfType, List.fill(deltaWidth)(bits(width - 1, width - 1).elements.head) ++ elements)
        case DFUInt.Type(_) | DFBits.Type(_) =>
          Source(dfType, SourceElement(SourceValue.Const(DFBits.Token(deltaWidth, 0)), withInit = true) concat elements)
      }
    }
    else this
  }

  def assign(src : Source, relBitHigh : Int, relBitLow : Int, withInit : Boolean) : Source = {
    assert(src.width == relBitHigh - relBitLow + 1)
    val (highElemIdx, highRelIdx) = bitIdxToElementIdx(relBitHigh)
    val (lowElemIdx, lowRelIdx) = bitIdxToElementIdx(relBitLow)
    val srcElements = if (!withInit) src.elements.map(e => e.copy(withInit = false)) else src.elements
    if (highElemIdx == lowElemIdx) { //same element
      val (leftElements, selElem :: rightElements) = elements.splitAt(highElemIdx)
      val keptLeft = leftElements concat selElem.bits(selElem.relWidth - 1, highRelIdx+1)
      val keptRight = selElem.bits(lowRelIdx-1, 0) concat rightElements
      src.copy(elements = keptLeft concat srcElements concat keptRight)
    } else {
      val (left, lowElem :: rightElements) = elements.splitAt(lowElemIdx)
      val (leftElements, highElem :: _) = left.splitAt(highElemIdx)
      val keptLeft = leftElements concat highElem.bits(highElem.relWidth - 1, highRelIdx+1)
      val keptRight = lowElem.bits(lowRelIdx-1, 0) concat rightElements
      src.copy(elements = keptLeft concat srcElements concat keptRight)
    }
  }
  def assignWL(src : Source, relWidth : Int, relBitLow : Int, withInit : Boolean) : Source =
    assign(src, relBitLow + relWidth - 1, relBitLow, withInit)

  def codeString(implicit dependencyContext: DependencyContext) : String = {
    val concatStr = elements.map(_.codeString.applyBrackets()).mkString(" ++ ")
    elements match {
      case SourceElement(srcVal, _, _, _, false, false, _, _) :: Nil if srcVal.dfType == dfType => concatStr
      case _ =>
        dfType match {
          case DFBits.Type(_) => concatStr
          case DFUInt.Type(_) => s"${concatStr.applyBrackets()}.uint"
          case DFSInt.Type(_) => s"${concatStr.applyBrackets()}.sint"
          case _ => s"${concatStr.applyBrackets()}.as($dfType)"
        }
    }
  }
}
object Source {
  def apply(dfType : DFAny.Type, element : SourceElement) : Source = Source(dfType, element :: Nil)
  def apply(dcl : DFAny.Dcl, sourceVersion: SourceVersion, withInit : Boolean) : Source =
    Source(dcl.dfType, SourceElement(SourceValue.Dcl(dcl, sourceVersion), withInit))
  object Empty {
    def apply(dcl : DFAny.Dcl) : Source = Source(dcl, SourceVersion.Empty, withInit = true)
  }
  object Const {
    def apply(token : DFAny.Token) : Source =
      Source(token.dfType, SourceElement(SourceValue.Const(token), withInit = true))
  }
  object Idx {
    def apply(dcl : DFAny.Dcl, block : DFBlock, idx : Int) : Source =
      Source(dcl, SourceVersion.Idx(block, idx), withInit = true)
  }
  object IfElse {
    def apply(
      dcl : DFAny.Dcl,
      headBranch : DFConditional.IfElseBlock,
      branchVersions : List[(Source, SourceVersion)],
      fallbackVersion : SourceVersion,
      withInit : Boolean
    ) : Source = Source(dcl, SourceVersion.IfElse(headBranch, branchVersions, fallbackVersion), withInit)
  }
  object Match {
    def apply(
      dcl : DFAny.Dcl,
      headCase : DFConditional.CaseBlock,
      caseVersions : List[(DFAny.Pattern, SourceVersion)],
      fallbackVersion : Option[SourceVersion],
      withInit : Boolean
    ) : Source = Source(dcl, SourceVersion.Match(headCase, caseVersions, fallbackVersion), withInit)
  }
  object Latest {
    def apply(dfVal : DFAny.Member) : Source = dfVal match {
      case dcl : DFAny.Dcl => Source(dcl, SourceVersion.Latest, withInit = true)
      case func2 : DFAny.Func2 => Source(dfVal.dfType, SourceElement(SourceValue.Func2(func2), withInit = true))
    }
  }
  object ApplySel {
    def apply(dfType : DFAny.Type, relSrc : Source, idxSrc : Source) : Source =
      Source(dfType, SourceElement(SourceValue.ApplySel(dfType, relSrc, idxSrc), withInit = false))
  }
}