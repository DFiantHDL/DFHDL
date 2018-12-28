//package RISCV2
//
//import DFiant._
//
//class DMem_Bram()(implicit ctx : RTComponent.Context) extends RTComponent {
//  final val clka  = Clock()
//  final val wea   = DFBits(4)  <> IN
//  final val addra = DFBits(12) <> IN
//  final val dina  = DFBits(32) <> IN
//  final val douta = DFBits(32) <> OUT
//  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
//}
//
//trait DMem extends DFDesign {
//  private val addr        = DFBits[32] <> IN
//  private val dataToMem   = DFBits[32] <> IN
//  private val dmemSel     = DFEnum[DMemSel] <> IN
//  private val dataFromMem = DFBits[32] <> OUT
//  private val wrEnToMem   = DFBits(4)
//  val bram = new DMem_Bram()
//  wrEnToMem := b"0000"
//  dataFromMem := bram.douta
//  matchdf(dmemSel)
//    .casedf(DMemSel.LB) {
//      matchdf(addr(1, 0))
//        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).sint.extendTo(32).bits}
//        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).sint.extendTo(32).bits}
//        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).sint.extendTo(32).bits}
//        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).sint.extendTo(32).bits}
//    }
//    .casedf(DMemSel.LH) {
//      matchdf(addr(1, 1))
//        .casedf(b"0")     {dataFromMem := bram.douta(15,  0).sint.extendTo(32).bits}
//        .casedf(b"1")     {dataFromMem := bram.douta(31, 16).sint.extendTo(32).bits}
//    }
//    .casedf(DMemSel.LW)   {dataFromMem := bram.douta}
//    .casedf(DMemSel.LBU) {
//      matchdf(addr(1, 0))
//        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).extendLeftTo(32)}
//        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).extendLeftTo(32)}
//        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).extendLeftTo(32)}
//        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).extendLeftTo(32)}
//    }
//    .casedf(DMemSel.LHU) {
//      matchdf(addr(1, 1))
//        .casedf(b"0")    {dataFromMem := bram.douta(15,  0).extendLeftTo(32)}
//        .casedf(b"1")    {dataFromMem := bram.douta(31, 16).extendLeftTo(32)}
//    }
//    .casedf(DMemSel.SB) {
//      matchdf(addr(1, 0))
//        .casedf(b"00")    {wrEnToMem := b"0001"}
//        .casedf(b"01")    {wrEnToMem := b"0010"}
//        .casedf(b"10")    {wrEnToMem := b"0100"}
//        .casedf(b"11")    {wrEnToMem := b"1000"}
//    }
//    .casedf(DMemSel.SH) {
//      matchdf(addr(1, 1))
//        .casedf(b"0")     {wrEnToMem := b"0011"}
//        .casedf(b"1")     {wrEnToMem := b"1100"}
//    }
//    .casedf(DMemSel.SW)   {wrEnToMem := b"1111"}
//  bram.addra <> addr(13, 2)
//  bram.wea <> wrEnToMem
//  bram.dina <> dataToMem
//
//  def readWriteConn(addr : DFBits[32], dataToMem : DFBits[32], dmemSel : DFEnum[DMemSel])(implicit ctx : DFDesign.Context) : DFBits[32] = {
//    this.addr <> addr
//    this.dataToMem <> dataToMem
//    this.dmemSel <> dmemSel
//    this.dataFromMem
//  }
//}
