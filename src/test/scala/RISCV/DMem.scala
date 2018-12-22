package RISCV

import DFiant._

class DMem_Bram()(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka  = Clock()
  final val wea   = DFBits(1)  <> IN
  final val addra = DFBits(12) <> IN
  final val dina  = DFBits(32) <> IN
  final val douta = DFBits(32) <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
}

trait DMem extends DFDesign {
  private val addr        = DFBits[32] <> IN
  private val dataToMem   = DFBits[32] <> IN
  private val wrEnToMem   = DFBool()   <> IN
  private val dataFromMem = DFBits[32] <> OUT

  val bram = new DMem_Bram()
  bram.addra <> addr(13, 2)
  bram.wea <> wrEnToMem
  bram.dina <> dataToMem
  bram.douta <> dataFromMem

  def readWriteConn(addr : DFBits[32], dataToMem : DFBits[32], wrEnToMem : DFBool)(implicit ctx : DFDesign.Context) : DFBits[32] = {
    this.addr <> addr
    this.dataToMem <> dataToMem
    this.wrEnToMem <> wrEnToMem
    this.dataFromMem
  }
}
