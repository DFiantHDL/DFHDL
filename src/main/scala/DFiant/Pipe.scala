package DFiant
import internals._

case class PipeValue(width : Int, value : Option[Int]) {
  def + (that : Int) : PipeValue = value match {
    case Some(v) => PipeValue(width, Some(v + that))
    case None => this
  }
  def + (that : PipeValue) : PipeValue = {
    //TODO: consider fixing
//    assert(this.width == that.width)
    (this.value, that.value) match {
      case (Some(vL), Some(vR)) => PipeValue(that.width, Some(vL + vR))
      case (Some(vL), None) => this
      case (None, Some(vR)) => throw new IllegalArgumentException("\nUnexpected delta pipe from None")
      case (None, None) => this
    }
  }
  def - (that : PipeValue) : PipeValue = {
    //TODO: consider fixing
    //    assert(this.width == that.width)
    (this.value, that.value) match {
      case (Some(vL), Some(vR)) => PipeValue(that.width, Some(vL - vR))
      case (Some(vL), None) => this
      case (None, Some(vR)) => throw new IllegalArgumentException("\nUnexpected delta pipe from None")
      case (None, None) => this
    }
  }

  override def toString: String = if (value.isDefined) value.get.toString else "None"
}

case class Pipe(elements : List[PipeValue]){
  val width : Int = elements.map(v => v.width).sum
  private def getMaxPipe : Option[Int] = {
    val vl = elements.flatMap(v => v.value)
    if (vl.isEmpty) None else Some(vl.max)
  }
  def coalesce : Pipe = Pipe(elements.foldLeft(List[PipeValue]()) {
    case (ls, e) if ls.isEmpty || ls.last.value != e.value=> ls :+ e
    case (ls, e) => ls.dropRight(1) :+ PipeValue(ls.last.width + e.width, e.value)
  })
  def separate : Pipe = Pipe(elements.foldLeft(List[PipeValue]()) {
    case (ls, e) => ls ++ List.fill(e.width)(PipeValue(1, e.value))
  })
  private def reverseIndex(idx : Int) : Int = width-1-idx
  def bitsWL(relWidth : Int, relBitLow : Int) : Pipe =
    Pipe(separate.elements.slice(reverseIndex(relBitLow + relWidth-1), reverseIndex(relBitLow-1))).coalesce
  def reverse : Pipe = Pipe(elements.reverse)
  def replaceWL(relWidth : Int, relBitLow : Int, thatSource : Pipe) : Pipe = {
    val elms = separate.elements
    val left = elms.take(reverseIndex(relBitLow + relWidth-1))
    val right = elms.takeRight(relBitLow)
    assert(width - left.length - right.length == thatSource.width, s"$width - ${left.length} - ${right.length} != ${thatSource.width}")
    Pipe(left ++ thatSource.elements ++ right).coalesce
  }
  def ## (that : Pipe) = Pipe(this.elements ++ that.elements)
  def balanced : Pipe = Pipe(width, getMaxPipe)
  def orElse (that : Pipe) : Pipe =
    Pipe(this.separate.elements.zip(that.separate.elements).collect {
      case (left, right) => if (left.value.isDefined) left else right
    }).coalesce
  def + (that : Int) : Pipe = Pipe(elements.map(v => v + that))
  def - (that : Pipe) : Pipe = {
    if (this.elements.length == 1) {
      val refPipe = this.elements.head
      Pipe(that.elements.map{v => refPipe - v})
    } else {
      assert(this.width == that.width)
      val z = (this.elements, that.elements).zipped
      val sameSplit = z.map((pL, pR) => pL.width == pR.width).reduce((l, r) => l && r)
      if (sameSplit) Pipe(z.map((pL, pR) => pL - pR))
      else Pipe((this.separate.elements, that.separate.elements).zipped.map((l, r) => l - r)).coalesce
    }
  }

  override def toString : String =
    if (elements.length == 1) elements.head.toString
    else elements.map(v => s"(${v.width},$v)").mkString("|")
}

object Pipe {
  implicit class PipeList(list : List[Pipe]) {
    def concat : Pipe = list.reduce((l, r) => l ## r)
    def getMaxPipe : Option[Int] = {
      val fl = list.flatMap(p => p.getMaxPipe)
      if (fl.isEmpty) None else Some(fl.max)
    }
    def balance : List[Pipe] = {
      val max = getMaxPipe
      list.map(p => Pipe(p.width, max))
    }
  }
  def apply(width : Int, value : Option[Int]) : Pipe = Pipe(List(PipeValue(width, value)))
  def apply(width : Int, value : Int) : Pipe = Pipe(width, Some(value))
  def none(width : Int) : Pipe = Pipe(List(PipeValue(width, None)))
  def zero(width : Int) : Pipe = Pipe(width, 0)
}


object PipeTest extends App {
  val p8 = Pipe(8, 1)
  val p4 = p8.bitsWL(4,0) + 1
  val p8b = p8.bitsWL(4,4) ## p4
  val p8n = p8b.balanced - p8b
  println(p8n)

}


trait CanBePiped extends DFAny {
  def pipe() : this.type
  def pipe(p : Int) : this.type
}