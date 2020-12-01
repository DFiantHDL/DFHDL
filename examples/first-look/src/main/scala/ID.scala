import DFiant._

@df class ID extends DFDesign { //This our `ID` dataflow design
  val x = DFSInt(16) <> IN  //The input port is a signed 16-bit integer
  val y = DFSInt(16) <> OUT //The output port is a signed 16-bit integer
  y := x //trivial direct input-to-output assignment
}
