import dfhdl.*

class ID(val width: Int <> CONST) extends DFDesign:
  val x = SInt(width) <> IN
  val y = SInt(width) <> OUT
  y := x

@top class IDTop(val width: Int = 8) extends DFDesign:
  val x = SInt(width) <> IN
  val y = SInt(width) <> OUT
  val id1 = ID(width)
  val id2 = ID(width)
  id1.x <> x
  id1.y <> id2.x
  id2.y <> y
