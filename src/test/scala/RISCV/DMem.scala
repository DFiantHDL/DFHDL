package RISCV

import DFiant._

trait DMem extends DFDesign {
  private val addr        = DFBits[32] <> IN
  private val dataToMem   = DFBits[32] <> IN
  private val wrEnToMem   = DFBool()   <> IN
  private val dataFromMem = DFBits[32] <> OUT

  def readWriteConn(addr : DFBits[32], dataToMem : DFBits[32], wrEnToMem : DFBool)(implicit ctx : DFDesign.Context) : DFBits[32] = {
    this.addr <> addr
    this.dataToMem <> dataToMem
    this.wrEnToMem <> wrEnToMem
    this.dataFromMem
  }
}
