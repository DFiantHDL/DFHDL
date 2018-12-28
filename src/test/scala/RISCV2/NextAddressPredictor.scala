//package RISCV2
//
//import DFiant._
//
//trait NextAddressPredictor extends DFDesign {
//  final val predPC = DFBits[32] <> IN
//  final val predNextPC = DFBits[32] <> OUT
//  final val updPC = DFBits[32] <> IN
//  final val updNextPC = DFBits[32] <> IN
//
//  def predictionConn(pc : DFBits[32])(implicit ctx : DFDesign.Context) : DFBits[32] = {
//    predPC <> pc
//    predNextPC
//  }
//  def updateConn(pc : DFBits[32], nextPC : DFBits[32])(implicit ctx : DFDesign.Context) : Unit = {
//    updPC <> pc
//    updNextPC <> nextPC
//  }
//}
//
//trait NeverTaken extends NextAddressPredictor {
//  predNextPC := (predPC.uint + 4).bits
//}