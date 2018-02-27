//package RingExample
//
//import DFiant._
//
//abstract class LR2Client(nodesNum : Int, nodeID : Int) {
//  val leftFlits : DFFlit#IN
//  val rightFlits : DFFlit#IN
//
//  def getPacket() : DFPacket = {
//    val packet = DFPacket(nodesNum)
//    ifdf (leftFlits.isNotEmpty) {
//      packet := leftFlits.toPacket(nodeID)
//      rightFlits.dontConsume()
//    } .elseifdf(rightFlits.isNotEmpty) {
//      packet := rightFlits.toPacket(nodeID)
//      leftFlits.dontConsume()
//    } elsedf {
//      rightFlits.dontConsume()
//      leftFlits.dontConsume()
//      packet.dontProduce()
//    }
//    packet
//  }
//
//}
