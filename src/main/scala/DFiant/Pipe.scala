package DFiant

sealed trait Pipe
object Pipe {
  case object Ignore extends Pipe
  case class Value(value : Int) extends Pipe

  def max(list : List[Pipe]) : Pipe = {
    val vList = list.collect{case Value(v) => v}
    if (vList.isEmpty) Ignore
    else Value(vList.max)
  }
  def +(pipeL : Pipe, pipeR : Pipe) : Pipe = (pipeL, pipeR) match {
    case (Value(vL), Value(vR)) => Value(vL + vR)
    case _ => Ignore
  }
}
