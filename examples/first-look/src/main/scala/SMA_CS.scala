import dfhdl._

@df class SMA_CS extends DFDesign {
  val x   = SInt(16) <> IN init 0
  val y   = SInt(16) <> OUT
  val acc = SInt(18) <> VAR init 0
  acc := acc - x.prev(4) + x
  y   := (acc / 4).resize(16)
}
