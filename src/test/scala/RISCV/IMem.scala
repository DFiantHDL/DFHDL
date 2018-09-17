package RISCV
import DFiant._

trait IMem extends DFDesign {
  final val addr = DFBits(32) <> IN
  final val inst = DFBits(32) <> OUT
}
