package DFiant

case class PipeValue(width : Int, value : Option[Int]) {
  def + (that : Int) : PipeValue = value match {
    case Some(v) => PipeValue(width, Some(v + that))
    case None => this
  }
}

case class Pipe(valueList : List[PipeValue]){
  val width : Int = valueList.map(v => v.width).sum
  def coalesce : Pipe = Pipe(valueList.foldLeft(List[PipeValue]()) {
    case (ls, e) if ls.isEmpty || ls.last.value != e.value=> ls :+ e
    case (ls, e) => ls.dropRight(1) :+ PipeValue(ls.last.width + e.width, e.value)
  })
  def separate : Pipe = Pipe(valueList.foldLeft(List[PipeValue]()) {
    case (ls, e) => ls ++ List.fill(e.width)(PipeValue(1, e.value))
  })
  def bits(relWidth : Int, relBitLow : Int) : Pipe = {
    assert(relWidth + relBitLow <= width)
    assert(relBitLow < width)
    Pipe(separate.valueList.reverse.slice(relBitLow, relWidth - relBitLow)).coalesce
  }
  def reverse : Pipe = Pipe(valueList.reverse)
  def balanced : Pipe = Pipe(List(PipeValue(width, Some(valueList.flatMap(v => v.value).max))))
}

object Pipe {
  def const(width : Int) : Pipe = Pipe(List(PipeValue(width, None)))
}