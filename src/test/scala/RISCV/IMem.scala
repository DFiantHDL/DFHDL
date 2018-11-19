package RISCV
import DFiant._

trait IMem extends DFDesign {
  private val addr = DFBits[32] <> IN
  private val inst = DFBits[32] <> OUT

  def readConn(addr : DFBits[32])(implicit ctx : DFDesign.Context) : DFBits[32] = {
    this.addr <> addr
    this.inst
  }
}

