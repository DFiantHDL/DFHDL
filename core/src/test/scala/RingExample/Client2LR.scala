/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
