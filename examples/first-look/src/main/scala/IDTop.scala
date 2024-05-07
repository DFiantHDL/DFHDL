import dfhdl.*

class IDTop extends DFDesign: // This our `IDTop` dataflow design
  val x = SInt(16) <> IN // The input port is a signed 16-bit integer
  val y = SInt(16) <> OUT // The output port is a signed 16-bit integer
  val id1 = ID() // First instance of the `ID` design
  val id2 = ID() // Second instance of the `ID` design
  id1.x <> x // Connecting parent input port to child input port
  id1.y <> id2.x // Connecting sibling instance ports
  id2.y <> y // Connecting parent output port to child output port
