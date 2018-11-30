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

case class Pipe(valueList : List[PipeValue]){
  val width : Int = valueList.map(v => v.width).sum
  private def getMaxPipe : Option[Int] = {
    val vl = valueList.flatMap(v => v.value)
    if (vl.isEmpty) None else Some(vl.max)
  }
  def coalesce : Pipe = Pipe(valueList.foldLeft(List[PipeValue]()) {
    case (ls, e) if ls.isEmpty || ls.last.value != e.value=> ls :+ e
    case (ls, e) => ls.dropRight(1) :+ PipeValue(ls.last.width + e.width, e.value)
  })
  def separate : Pipe = Pipe(valueList.foldLeft(List[PipeValue]()) {
    case (ls, e) => ls ++ List.fill(e.width)(PipeValue(1, e.value))
  })
  def bitsWL(relWidth : Int, relBitLow : Int) : Pipe = {
    assert(relWidth + relBitLow <= width)
    assert(relBitLow < width)
    Pipe(separate.valueList.reverse.slice(relBitLow, relBitLow + relWidth)).coalesce
  }
  def reverse : Pipe = Pipe(valueList.reverse)
  def ## (that : Pipe) = Pipe(this.valueList ++ that.valueList)
  def balanced : Pipe = Pipe(width, getMaxPipe)
  def + (that : Int) : Pipe = Pipe(valueList.map(v => v + that))
  def - (that : Pipe) : Pipe = {
    if (this.valueList.length == 1) {
      val refPipe = this.valueList.head
      Pipe(that.valueList.map{v => refPipe - v})
    } else {
      assert(this.width == that.width)
      val z = (this.valueList, that.valueList).zipped
      val sameSplit = z.map((pL, pR) => pL.width == pR.width).reduce((l, r) => l && r)
      if (sameSplit) Pipe(z.map((pL, pR) => pL - pR))
      else Pipe((this.separate.valueList, that.separate.valueList).zipped.map((l, r) => l - r)).coalesce
    }
  }

  override def toString : String =
    if (valueList.length == 1) valueList.head.toString
    else valueList.map(v => s"(${v.width},$v)").mkString("|")
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