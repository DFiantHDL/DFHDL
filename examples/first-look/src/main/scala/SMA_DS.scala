import dfhdl._

@df class SMA_DS extends DFDesign {
  val x   = SInt(16) <> IN init 0
  val y   = SInt(16) <> OUT
  val s0  = x +^ x.prev
  val s2  = x.prev(2) +^ x.prev(3)
  val sum = s0 +^ s2
  y       := (sum / 4).resize(16)
}
