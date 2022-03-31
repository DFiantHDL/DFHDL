import DFiant.*

class Example extends DFDesign:
  val x = DFUInt(8) X 5 <> VAR init Vector.fill(5)(0)

@main def hello: Unit =
  val top = new Example
  top.printCodeString
