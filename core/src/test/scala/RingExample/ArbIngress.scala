//package RingExample
//
//import DFiant._
//
//abstract class ArbIngress(nodesNum : Int, nodeID : Int) {
//  val inFlit : DFFlit#IN
//  val localCtrlFlit : DFFlit#OUT
//  val localDataFlit : DFFlit#OUT
//  val remoteCtrlFlit : DFFlit#OUT
//  val remoteDataFlit : DFFlit#OUT
//
//  //changing default to no token production
//  localCtrlFlit.dontProduce()
//  localDataFlit.dontProduce()
//  remoteCtrlFlit.dontProduce()
//  remoteDataFlit.dontProduce()
//
//  ifdf (inFlit.ctrl) { //Input is control flit
//    ifdf (inFlit.dst == nodeID) {
//      localCtrlFlit := inFlit
//    } elsedf {
//      remoteCtrlFlit := inFlit
//    }
//  } elsedf { //Input is data flit
//    ifdf (inFlit.dst == nodeID) {
//      localDataFlit := inFlit
//    } elsedf {
//      remoteDataFlit := inFlit
//    }
//  }
//}
