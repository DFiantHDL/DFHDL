import DFiant.*

class VideoDefs[CW <: Int](val CORDW: Inlined[CW]):
  // declare the struct
  case class Coord(
      x: DFUInt[CW] <> VAL,
      y: DFUInt[CW] <> VAL
  ) extends DFStruct

class Example(using DFC) extends DFDesign:
  val i = DFBits(16) <> IN

  object videoDefs extends VideoDefs(8)
  export videoDefs.*

  // construct the struct variable
  val pixel = Coord <> VAR init Coord(5, 7)

  // field selection and assignment
  pixel.x := pixel.y + 5

  // struct constant composition
  pixel := Coord(d"22", 22)

  // struct value composition
  pixel := Coord(i(15, 8), i(7, 0))

  // casted assignment
  pixel := i.as(Coord)

end Example

@main def hello: Unit =
  println("start")
  val top = new Example
  top.printCodeString
  println("done")
