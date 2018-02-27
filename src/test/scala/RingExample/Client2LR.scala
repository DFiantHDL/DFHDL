//package RingExample
//
//import DFiant._
//
//
//object LRDir extends Enumeration {
//  type LRDir = Value
//  val LEFT, RIGHT = Value
//}
//
//import LRDir._
//
//abstract class Client2LR(nodesNum : Int, nodeID : Int) {
//  val leftFlits : DFFlit#OUT
//  val rightFlits : DFFlit#OUT
//
//  private val lastPacket = DFPacket(nodesNum)
//
//  //Calculates nearest
//  private def directionSel(src : DFUInt, dst : DFUInt) : DFEnum[LRDir] = {
//    val dir = DFEnum(LRDir)
//    val half = nodesNum / 2
//    ifdf (dst > half) {
//      ifdf (src < half) {
//        dir := LEFT
//      } elsedf {
//        dir := RIGHT
//      }
//    } elsedf {
//      ifdf (src < half) {
//        dir := RIGHT
//      } elsedf {
//        dir := LEFT
//      }
//    }
//    dir
//  }
//
//  def sendPacket(packet : DFPacket): Unit = {
//    lastPacket := packet
//    val flits = packet.toFlits(nodeID)
//    val src = DFUInt.rangeUntil(nodesNum) := nodeID
//    val dir = directionSel(src, packet.peer)
//    ifdf (dir == LEFT) {
//      for (i <- 0 until Defs.flitsNum)
//        leftFlits.assignNext(i, flits.next(i))
//    } elsedf { //RIGHT
//      for (i <- 0 until Defs.flitsNum)
//        rightFlits.assignNext(i, flits.next(i))
//    }
//  }
//
//  def resendLastPacket() : Unit = {
//    sendPacket(lastPacket)
//  }
//}
