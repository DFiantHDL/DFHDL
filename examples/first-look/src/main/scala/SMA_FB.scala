import DFiant._

@df class SMA_FB extends DFDesign {
  val x   = DFSInt(16) <> IN init 0
  val y   = DFSInt(16) <> OUT
  val acc = DFSInt(18) init 0
  acc := acc - x.prev(4) + x
  y   := (acc >> 2).resize(16)
}
