import dfhdl._

@df class SMA extends DFDesign {
  val x   = SInt(16) <> IN init 0
  val y   = SInt(16) <> OUT
  val sum = (x +^ x.prev) +^ (x.prev(2) +^ x.prev(3))
  y       := (sum >> 2).resize(16)
}
