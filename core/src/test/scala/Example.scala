import DFiant.*

class Example(using DFC) extends DFDesign:
  val x = DFUInt(8) <> VAR
  val z : Int = -1
  x := z
  x := x + z
  x := z
  x := z
  x := z
end Example

@main def hello : Unit = {
  println("start")
  val top = new Example
  top.getDB
  println("done")
}