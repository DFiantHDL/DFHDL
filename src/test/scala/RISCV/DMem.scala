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
  private val dmemSel     = DFEnum[DMemSel] <> IN
  private val dataFromMem = DFBits[32] <> OUT
  private val wrEnToMem   = DFBool()
  wrEnToMem := false
  matchdf(dmemSel)
    .casedf(DMemSel.LB)   {}
    .casedf(DMemSel.LH)   {}
    .casedf(DMemSel.LW)   {}
    .casedf(DMemSel.LBU)  {}
    .casedf(DMemSel.LHU)  {}
    .casedf(DMemSel.SB)   {wrEnToMem := true}
    .casedf(DMemSel.SH)   {wrEnToMem := true}
    .casedf(DMemSel.SW)   {wrEnToMem := true}
  val bram = new DMem_Bram()
  bram.addra <> addr(13, 2)
  bram.wea <> wrEnToMem
  bram.dina <> dataToMem
  bram.douta <> dataFromMem

  def readWriteConn(addr : DFBits[32], dataToMem : DFBits[32], dmemSel : DFEnum[DMemSel])(implicit ctx : DFDesign.Context) : DFBits[32] = {
    this.addr <> addr
    this.dataToMem <> dataToMem
    this.dmemSel <> dmemSel
    this.dataFromMem
  }
}
