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
