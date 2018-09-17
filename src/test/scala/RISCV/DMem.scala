package RISCV

import DFiant._

trait DMem extends DFDesign {
  final val addr        = DFBits(32) <> IN
  final val dataToMem   = DFBits(32) <> IN
  final val wrEnToMem   = DFBool()   <> IN
  final val dataFromMem = DFBits(32) <> OUT
}
