//package fpga2020
//import DFiant._
//
//trait MA extends DFDesign {
//  val src   = DFSInt[16] <> IN init 0
//  val avg   = DFSInt[16] <> OUT
//  val acc = DFSInt[18] init 0
//  acc := acc - src.prev(5) + src.prev
//  val ret = (acc >> 2).resize(16)
//  avg := ret.pipe()
//}
//
//
//trait MA4 extends DFDesign {
//  val a    = DFSInt[16] <> IN init 0
//  val b    = DFSInt[16] <> IN init 0
//  val c    = DFSInt[16] <> IN init 0
//  val d    = DFSInt[16] <> IN init 0
//  val o    = DFSInt[16] <> OUT
//
//  val maA = new MA {}; maA.src <> a
//  val maB = new MA {}; maB.src <> b
//  val maC = new MA {}; maC.src <> c
//  val maD = new MA {}; maD.src <> d
//
//  val avgL = ((maA.avg + maB.avg).wc >> 1).toWidth(16)
//  val avgR =  ((maC.avg + maD.avg).wc >> 1).toWidth(16)
//  val avgF =  ((avgL.pipe + avgR.pipe).wc >> 1).toWidth(16)
//  o := avgF.pipe()
//}
//
//
//
////object MA4App extends DFApp.VHDLCompiler[MA4]
//
