/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import internals._

import scala.annotation.tailrec
import scala.collection.immutable

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Source Aggregator
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
private[DFiant] case class AliasTag(dfVal : DFAny, context : DFBlock, dfNet : Option[DFNet], version : Option[Int], prevStep : Int, inverted : Boolean, latency : Option[Int], pipeStep : Int) {
  def invert : AliasTag = copy(inverted = !inverted)
  def prev(step : Int) : AliasTag = copy(prevStep = prevStep + step)
  private def addPipeToLatency(p : Int) : Option[Int] = latency match {
    case Some(lat) => Some(lat + p)
    case None => None
  }
  @tailrec private def versioned(currentContext : DFBlock) : AliasTag =
    currentContext.netsTo.get(dfVal) match {
      case Some(x) => copy(version = Some(x.length), context = currentContext)
      case None => currentContext match {
        case x : ConditionalBlock[_,_] => versioned(x.owner)
        case _ => copy(version = Some(0), context = currentContext)
      }
    }

  final def versioned : AliasTag =
    if (dfVal.isAssignable && (dfVal.nonTransparentOwner eq context.nonTransparent)) versioned(context) else this

  def atContext(newContext : DFBlock) : AliasTag = copy(context = newContext)
  def atVersion(num : Int) : AliasTag = copy(version = Some(num))
  def via(viaNet : DFNet) : AliasTag = copy(dfNet = Some(viaNet), context = viaNet.owner.asInstanceOf[DFBlock])
  def pipe(step : Int) : AliasTag = copy(latency = addPipeToLatency(step), pipeStep = pipeStep + step)
  def balanceTo(maxLatency : Option[Int]) : AliasTag = (maxLatency, latency) match {
    case (Some(maxLat), Some(lat)) => pipe(maxLat - lat)
    case _ => this
  }
}
private[DFiant] object AliasTag {
  def apply(dfVal : DFAny, context : DFBlock) : AliasTag =
    AliasTag(dfVal = dfVal, context = context, dfNet = None, version = None, prevStep = 0, inverted = false, latency = None, pipeStep = 0)
  def withLatency(dfVal : DFAny, latency : Option[Int]) : AliasTag =
    AliasTag(dfVal = dfVal, context = dfVal.owner.asInstanceOf[DFBlock], dfNet = None, version = None, prevStep = 0, inverted = false, latency = latency, pipeStep = 0)
}

private[DFiant] case class SourceElementOld(relBitHigh: Int, relBitLow : Int, reverseBits : Boolean, aliasTag : Option[AliasTag]) {
  val width : Int = relBitHigh - relBitLow + 1
  def range : Range = if (reverseBits) relBitLow to relBitHigh else relBitHigh to relBitLow by -1
  def reverse : SourceElementOld = copy(reverseBits = !reverseBits)
  def invert : SourceElementOld = copy(aliasTag = aliasTag.map(t => t.invert))
  def prev(step : Int) : SourceElementOld = copy(aliasTag = aliasTag.map(t => t.prev(step)))
  def pipe(step : Int) : SourceElementOld = copy(aliasTag = aliasTag.map(t => t.pipe(step)))
  def via(viaNet : DFNet) : SourceElementOld = copy(aliasTag = aliasTag.map(t => t.via(viaNet)))
  def connectionsOnly : SourceElementOld = aliasTag match {
    case Some(AliasTag(_,_,Some(DFNet.Connection(_,_)),_,_,_,_,_)) => this
    case _ => copy(aliasTag = None)
  }
  def assignmentsOnly : SourceElementOld = aliasTag match {
    case Some(AliasTag(_,_,Some(DFNet.Assignment(_,_)),_,_,_,_,_)) => this
    case _ => copy(aliasTag = None)
  }
  def balanceTo(maxLatency : Option[Int]) : SourceElementOld = copy(aliasTag = aliasTag.map(t => t.balanceTo(maxLatency)))
  def versioned : SourceElementOld = {
    val versionedTag = aliasTag.map(t => t.versioned)
    versionedTag.foreach {
      case v if v.version.isDefined => v.dfVal.consumeAt(width, relBitLow, v.version.get, v.context)
      case _ =>
    }
    copy(aliasTag = versionedTag)
  }
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = aliasTag match {
    case Some(t) =>
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) "~" else ""
      val prevStr = if (t.prevStep == 1) s".prev" else if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      val selStr = if (t.dfVal.width.getValue != width) s"($relBitHigh, $relBitLow)" else ""
      val pipeStr = if (t.pipeStep == 1) s".pipe" else if (t.pipeStep > 0) s".pipe(${t.pipeStep})" else ""
      s"$invertStr${t.dfVal.refCodeString}$selStr$prevStr$pipeStr$reverseStr"
    case None => "NA"
  }
  def latencyString : String = aliasTag match {
    case Some(AliasTag(_,_,_,_,_,_,Some(lat),_)) => lat.toString
    case _ => s"NA"
  }


  override def toString: String = aliasTag match {
    case Some(t) =>
      val ref = t.dfVal match {
        case x : DFAny.Const[_] => s"CONST_${x.token}"
        case _ => t.dfVal.fullName
      }
      val reverseStr = if (reverseBits) ".reverse" else ""
      val invertStr = if (t.inverted) ".invert" else ""
      val prevStr = if (t.prevStep > 0) s".prev(${t.prevStep})" else ""
      val versionStr = if (t.prevStep == 0) t.version.map(i => s"@V$i^${t.context}").getOrElse("") else ""
      s"$ref($relBitHigh, $relBitLow)$prevStr$reverseStr$invertStr$versionStr"
    case None => s"None($relBitHigh, $relBitLow)"
  }
}
sealed trait SourceStage extends Product with Serializable {
  def prev(step : Int) : SourceStage
}
object SourceStage {
  case object Latest extends SourceStage {
    override def prev(step: Int): SourceStage = Prev(step)
  }
  case class Prev(step : Int) extends SourceStage {
    override def prev(step: Int): SourceStage = Prev(step + this.step)
  }
  case class Versioned(num : Int, context : DFBlock) extends SourceStage {
    override def prev(step: Int): SourceStage = Prev(step)
  }
}

sealed trait SourceElement extends Product with Serializable {
  val width : Int
  def reverse : SourceElement
  def invert : SourceElement
  def prev(step : Int) : SourceElement
  def pipe(step : Int) : SourceElement
  def via(viaNet : DFNet) : SourceElement
  def connectionsOnly : SourceElement
  def assignmentsOnly : SourceElement
  def versioned : SourceElement
  def ~= (that : SourceElement) : Boolean = (this, that) match {
    case (_ : SourceElement.Empty, _ : SourceElement.Empty) => true
    case (l : SourceElement.Alias, r : SourceElement.Alias) if (l.dfVal eq r.dfVal) && (l.dfNet == r.dfNet) &&
      (l.inverted == r.inverted && l.stage == r.stage && l.latency == r.latency && l.pipeStep == r.pipeStep) => true
    case _ => false
  }
  def !~= (that : SourceElement) : Boolean = !(this ~= that)
  def ## (that : SourceElement) : List[SourceElement] = (this, that) match {
    case (l : SourceElement.Empty, r : SourceElement.Empty) => List(SourceElement.Empty(l.width + r.width))
    case (l : SourceElement.Alias, r : SourceElement.Alias) if (l ~= r) =>
      if (l.relBitLow == r.relBitHigh + 1 && ((!l.reversed && !r.reversed) || r.width == 1))
        List(l.copy(relBitHigh = l.relBitHigh, relBitLow = r.relBitLow, reversed = l.reversed))
      else if (l.relBitHigh == r.relBitLow - 1 && ((l.reversed && r.reversed) || r.width == 1))
        List(l.copy(relBitHigh = r.relBitHigh, relBitLow = l.relBitLow, reversed = l.reversed))
      else List(l, r)
    case (l, r) => List(l, r)
  }
  def balanceTo(maxLatency : Option[Int]) : SourceElement
  def separate : List[SourceElement]
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String
  def isEmpty : Boolean
}

object SourceElement {
  final case class Empty(width : Int) extends SourceElement {
    def reverse : SourceElement = this
    def invert : SourceElement = this
    def prev(step : Int) : SourceElement = this
    def pipe(step : Int) : SourceElement = this
    def via(viaNet : DFNet) : SourceElement = this
    def connectionsOnly : SourceElement = this
    def assignmentsOnly : SourceElement = this
    def versioned : SourceElement = this
    def balanceTo(maxLatency : Option[Int]) : SourceElement = this
    def separate : List[SourceElement] = List.fill(width)(Empty(1))
    def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = "NA"
    def isEmpty : Boolean = true
  }
  final case class Alias(
    dfVal : DFAny, relBitHigh: Int, relBitLow : Int, reversed : Boolean, inverted : Boolean,
    stage : SourceStage, dfNet : Option[DFNet], latency : Option[Int], pipeStep : Int
  ) extends SourceElement {
    val width : Int = relBitHigh - relBitLow + 1
    private def range : Range = if (reversed) relBitLow to relBitHigh else relBitHigh to relBitLow by -1
    def reverse : SourceElement = copy(reversed = !reversed)
    def invert : SourceElement = copy(inverted = !inverted)
    def prev(step : Int) : SourceElement = copy(stage = stage.prev(step))
    private def addPipeToLatency(p : Int) : Option[Int] = latency match {
      case Some(lat) => Some(lat + p)
      case None => None
    }
    def pipe(step : Int) : SourceElement = copy(latency = addPipeToLatency(step), pipeStep = pipeStep + step)
    def balanceTo(maxLatency : Option[Int]) : SourceElement = (maxLatency, latency) match {
      case (Some(maxLat), Some(lat)) => pipe(maxLat - lat)
      case _ => this
    }
    def via(viaNet : DFNet) : SourceElement = copy(dfNet = Some(viaNet))
    def connectionsOnly : SourceElement = dfNet match {
      case Some(DFNet.Connection(_,_)) => this
      case None => this
      case _ => Empty(width)
    }
    def assignmentsOnly : SourceElement = dfNet match {
      case Some(DFNet.Assignment(_,_)) => this
      case None => this
      case _ => Empty(width)
    }
    @tailrec private def versioned(currentContext : DFBlock) : SourceElement =
      currentContext.netsTo.get(dfVal) match {
        case Some(x) => copy(stage = SourceStage.Versioned(x.length, currentContext))
        case None => currentContext match {
          case x : ConditionalBlock[_,_] => versioned(x.owner)
          case _ => copy(stage = SourceStage.Versioned(0, currentContext))
        }
      }

    def versioned : SourceElement = {
      val ret = (stage, dfNet, dfVal.nonTransparentOwner) match {
        case (SourceStage.Latest, Some(n), o) if dfVal.isAssignable && (o eq n.ctx.owner.nonTransparent) => versioned(n.ctx.owner)
        case (SourceStage.Latest, None, o : DFBlock) if dfVal.isAssignable => versioned(o)
        case _ => this
      }
      ret match {
        case SourceElement.Alias(dfVal,_,_,_,_,SourceStage.Versioned(num, context),_,_,_) =>
          dfVal.consumeAt(width, relBitLow, num, context)
        case _ =>
      }
      ret
    }
    private def prevStr = stage match {
      case SourceStage.Prev(prevStep) if prevStep == 1 => ".prev"
      case SourceStage.Prev(prevStep) if prevStep > 1 => s".prev($prevStep)"
      case _ => ""
    }
    private def reverseStr = if (reversed) ".reverse" else ""
    private def invertStr = if (inverted) "~" else ""
    private def selStr = if (dfVal.width.getValue != width) s"($relBitHigh, $relBitLow)" else ""
    private def pipeStr = if (pipeStep == 1) s".pipe" else if (pipeStep > 0) s".pipe(${pipeStep})" else ""
    private def versionStr = stage match {
      case SourceStage.Versioned(num, context) => s"@V$num^$context"
      case _ => ""
    }
    def refCodeString(implicit callOwner : DSLOwnerConstruct) : String =
        s"$invertStr${dfVal.refCodeString}$selStr$prevStr$pipeStr$reverseStr"
    def latencyString : String = latency match {
      case Some(lat) => lat.toString
      case _ => s"NA"
    }
    override def toString: String = {
      val ref = dfVal match {
        case x : DFAny.Const[_] => s"CONST_${x.token}"
        case _ => dfVal.fullName
      }
      s"$invertStr$ref($relBitHigh, $relBitLow)$prevStr$reverseStr$versionStr"
    }
    def separate : List[SourceElement] = range.toList.map(i => copy(relBitHigh = i, relBitLow = i))
    def isEmpty : Boolean = false
  }
  object Alias {
    def apply(dfVal : DFAny) : Alias = Alias(
      dfVal = dfVal, relBitHigh = dfVal.width-1, relBitLow = 0, reversed = false, inverted = false,
      dfNet = None, stage = SourceStage.Latest, latency = None, pipeStep = 0
    )
  }
}

private[DFiant] case class Source(elements : List[SourceElement]) {
  val width : Int = elements.map(v => v.width).sum
  def coalesce : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) if ls.isEmpty || (ls.last !~= e) => ls :+ e
    case (ls, right) =>
      val left = ls.last
      ls.dropRight(1) ++ (left ## right)
  })
  def separate : Source = Source(elements.foldLeft(List[SourceElement]()) {
    case (ls, e) => ls ++ e.separate
  })
  private def reverseIndex(idx : Int) : Int = width-1-idx
  def bitsWL(relWidth : Int, relBitLow : Int) : Source =
    if (relWidth == width) {assert(relBitLow == 0); this}
    else Source(separate.elements.slice(reverseIndex(relBitLow + relWidth-1), reverseIndex(relBitLow-1))).coalesce
  def bitsHL(relBitHigh : Int, relBitLow : Int) : Source = bitsWL(relBitHigh - relBitLow + 1, relBitLow)
  def replaceWL(relWidth : Int, relBitLow : Int, thatSource : Source) : Source =
    if (relWidth == width) {assert(relBitLow == 0); thatSource}
    else {
      //TODO: do not use separate, but coalesce on the fly
      val elms = separate.elements
      val left = elms.take(reverseIndex(relBitLow + relWidth-1))
      val right = elms.takeRight(relBitLow)
      assert(width - left.length - right.length == thatSource.width, s"$width - ${left.length} - ${right.length} != ${thatSource.width}")
      Source(left ++ thatSource.elements ++ right).coalesce
    }
  def replaceHL(relBitHigh : Int, relBitLow : Int, thatSource : Source) : Source =
    replaceWL(relBitHigh - relBitLow + 1, relBitLow, thatSource)
  def reverse : Source = Source(elements.reverse.map(e => e.reverse))
  def reverse(cond : Boolean) : Source = if (cond) reverse else this
  def invert : Source = Source(elements.map(e => e.invert))
  def invert(cond : Boolean) : Source = if (cond) invert else this
  def prev(step : Int) : Source = Source(elements.map(e => e.prev(step)))
  def pipe(step : Int) : Source = Source(elements.map(e => e.pipe(step)))
  def via(viaNet : DFNet) : Source = Source(elements.map(e => e.via(viaNet)))
  def connectionsOnly : Source = Source(elements.map(e => e.connectionsOnly)).coalesce
  def assignmentsOnly : Source = Source(elements.map(e => e.assignmentsOnly)).coalesce
  def resize(toWidth : Int) : Source =
    if (toWidth > width) {
      Source(List.fill(toWidth - width)(bitsWL(1, width-1).elements.head) ++ elements)
    } else if (toWidth < width) {
      bitsWL(toWidth, 0)
    } else {
      this
    }
  def versioned : Source = Source(elements.map(e => e.versioned))

  def getMaxLatency : Option[Int] = {
    val list = elements.collect{case e : SourceElement.Alias => e}.flatMap(t => t.latency)
    if (list.isEmpty) None else Some(list.max)
  }
  def balanceTo(maxLatency : Option[Int]) : Source = Source(elements.map(e => e.balanceTo(maxLatency))).coalesce
  def balance : Source = balanceTo(getMaxLatency)
  def ## (that : Source) : Source = Source(this.elements ++ that.elements).coalesce
  def copyWithNewDFVal(thatDFVal : DFAny) : Source = {
    assert(thatDFVal.width.getValue == width)
    var pos = width - 1
    Source(elements.map {
      case e : SourceElement.Alias =>
        val se = SourceElement.Alias(
          dfVal = thatDFVal, relBitHigh = pos, relBitLow = pos-e.width+1, reversed = false, inverted = false,
          latency = e.latency, pipeStep = 0, dfNet = None, stage = SourceStage.Latest
        )
        pos -= e.width
        se
      case e : SourceElement.Empty =>
        pos -= e.width
        e
    }).coalesce
  }

  def orElse (that : Source) : Source =
    Source(this.separate.elements.zip(that.separate.elements).collect {
      case (left, right) => if (left.isEmpty) right else left
    }).coalesce
  def isEmpty : Boolean = elements.length == 1 && elements.head.isEmpty
  def nonEmptyAtWL(relWidth : Int, relBitLow : Int) : Boolean = !bitsWL(relWidth, relBitLow).isEmpty
  def nonEmptyAtHL(relBitHigh : Int, relBitLow : Int) : Boolean = nonEmptyAtWL(relBitHigh - relBitLow + 1, relBitLow)
  def isCompletelyAllocated : Boolean = !elements.map(e => e.isEmpty).reduce((l, r) => l | r)
  def refCodeString(implicit callOwner : DSLOwnerConstruct) : String =
    if (elements.length > 1) elements.map(e => e.refCodeString).mkString("(", ", ", ")") else elements.head.refCodeString
  def latencyString : String = "TBD"
//  {
//    val cf = elements.collectFirst{case SourceElementOld(_,_,_,Some(t)) => t.dfVal}
//    if (cf.isEmpty) "NA"
//    else {
//      val coalesedLatency = Source(separate.elements.zipWithIndex.collect { case (e, i) =>
//        SourceElementOld(reverseIndex(i), reverseIndex(i), false, Some(AliasTag.withLatency(cf.get, if (e.aliasTag.isDefined) e.aliasTag.get.latency else None)))
//      }).coalesce
//      var pos = width - 1
//      coalesedLatency.elements.map(e => {
//        val high = pos
//        pos -= e.width
//        val low = pos + 1
//        if (high - low + 1 == width) e.latencyString else s"${e.latencyString}@($high, $low)"
//      }).mkString(", ")
//    }
//  }
  def toUsedBitSet : immutable.BitSet = {
    var bitHi = width-1
    elements.foldLeft(immutable.BitSet()){
      case (usedBits, e) =>
        val extra = e match {
          case _ : SourceElement.Alias => usedBits ++ ((bitHi-e.width+1) to bitHi)
          case _ : SourceElement.Empty => usedBits
        }
        bitHi = bitHi - e.width
        extra
    }
  }
  override def toString: String = elements.mkString(" ## ")
}
object Source {
  def apply(dfVal : DFAny) : Source = Source(List(SourceElement.Alias(dfVal)))
  def withLatency(dfVal : DFAny, latency : Option[Int]) : Source = Source(List(SourceElement.Alias(dfVal).copy(latency = latency)))
  def zeroLatency(dfVal : DFAny) : Source = withLatency(dfVal, Some(0))
  def none(width : Int) : Source = Source(List(SourceElement.Empty(width)))
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



