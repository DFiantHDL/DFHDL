//package RingExample
//
//import DFiant.core._
//
////Generates new arbitration for every apply
//case class Arb2to1(nodesNum : Int, nodeID : Int) {
//  def apply(ctrlFlit : DFFlit, dataFlit : DFFlit) : DFFlit = {
//    val toggle = DFBool.init(false)
//    val outFilt = DFFlit(nodesNum)
//    ifdf (ctrlFlit.isNotEmpty && !dataFlit.isNotEmpty) {
//      outFilt := ctrlFlit
//    } .elseifdf(!ctrlFlit.isNotEmpty && dataFlit.isNotEmpty) {
//      outFilt := dataFlit
//    } .elseifdf(ctrlFlit.isNotEmpty && dataFlit.isNotEmpty) {
//      ifdf (ctrlFlit.ctrl && !dataFlit.ctrl) {
//        outFilt := ctrlFlit
//        dataFlit.dontConsume()
//      } .elseifdf(!ctrlFlit.ctrl && dataFlit.ctrl) {
//        outFilt := dataFlit
//        ctrlFlit.dontConsume()
//      } elsedf {
//        ifdf (toggle) {
//          outFilt := ctrlFlit
//          dataFlit.dontConsume()
//        } elsedf {
//          outFilt := dataFlit
//          ctrlFlit.dontConsume()
//        }
//        toggle := !toggle
//      }
//    } elsedf {
//      outFilt.dontProduce()
//    }
//    outFilt
//  }
//}
//
//
//abstract class ArbEgress(nodesNum : Int, nodeID : Int) {
//  val outFlit         : DFFlit#OUT
//  val localCtrlFlit   : DFFlit#IN
//  val localDataFlit   : DFFlit#IN
//  val remoteCtrlFlit  : DFFlit#IN
//  val remoteDataFlit  : DFFlit#IN
//
//  val arb2to1 = Arb2to1(nodesNum, nodeID)
//  outFlit := arb2to1(arb2to1(localCtrlFlit, localDataFlit), arb2to1(remoteCtrlFlit, remoteDataFlit))
//}
//
