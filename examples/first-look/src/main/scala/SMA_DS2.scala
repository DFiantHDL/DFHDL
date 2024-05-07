import dfhdl._

@df class SMA_DS2 extends DFDesign {
  val x   = SInt(16) <> IN init 0
  val y   = SInt(16) <> OUT
  val s0  = x +^ x.prev
  val s2  = s0.prev(2)
  val sum = s0 +^ s2
  y       := (sum / 4).resize(16)
}
