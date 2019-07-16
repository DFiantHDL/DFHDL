/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

//package RingExample
//
//import DFiant._
//
//abstract class RingNode(nodesNum : Int, nodeID : Int) {
//  //I/O
//  val leftInFlit                    : DFFlit#IN
//  val leftOutFlit                   : DFFlit#OUT
//  val rightInFlit                   : DFFlit#IN
//  val rightOutFlit                  : DFFlit#OUT
//
//  private val r2lCtrlFlitFromBranch = DFFlit(nodesNum)
//  private val r2lCtrlFlitToBranch   = DFFlit(nodesNum)
//  private val r2lDataFlitFromBranch = DFFlit(nodesNum)
//  private val r2lDataFlitToBranch   = DFFlit(nodesNum)
//  private val l2rCtrlFlitFromBranch = DFFlit(nodesNum)
//  private val l2rCtrlFlitToBranch   = DFFlit(nodesNum)
//  private val l2rDataFlitFromBranch = DFFlit(nodesNum)
//  private val l2rDataFlitToBranch   = DFFlit(nodesNum)
//
//  private val r2lBranch = new Branch(nodesNum, nodeID) {
//    val localCtrlFlitFromBranch     : DFFlit#OUT = r2lCtrlFlitFromBranch
//    val localCtrlFlitToBranch       : DFFlit#IN  = r2lCtrlFlitToBranch
//    val localDataFlitFromBranch     : DFFlit#OUT = r2lDataFlitFromBranch
//    val localDataFlitToBranch       : DFFlit#IN  = r2lDataFlitToBranch
//    val outFlit                     : DFFlit#OUT = leftOutFlit
//    val inFlit                      : DFFlit#IN  = rightInFlit
//  }
//
//  private val l2rBranch = new Branch(nodesNum, nodeID) {
//    val localCtrlFlitFromBranch     : DFFlit#OUT = l2rCtrlFlitFromBranch
//    val localCtrlFlitToBranch       : DFFlit#IN  = l2rCtrlFlitToBranch
//    val localDataFlitFromBranch     : DFFlit#OUT = l2rDataFlitFromBranch
//    val localDataFlitToBranch       : DFFlit#IN  = l2rDataFlitToBranch
//    val outFlit                     : DFFlit#OUT = rightOutFlit
//    val inFlit                      : DFFlit#IN  = leftInFlit
//  }
//
//  private val client2LR = new Client2LR(nodesNum, nodeID) {
//    val leftFlits                   : DFFlit#OUT = r2lDataFlitToBranch
//    val rightFlits                  : DFFlit#OUT = l2rDataFlitToBranch
//  }
//
//  private val lr2Client = new LR2Client(nodesNum, nodeID) {
//    val leftFlits                   : DFFlit#IN = r2lDataFlitFromBranch
//    val rightFlits                  : DFFlit#IN = l2rDataFlitFromBranch
//  }
//
//  def sendPacket(packet : DFPacket) = client2LR.sendPacket(packet)
//  def getPacket() : DFPacket = lr2Client.getPacket()
//
//
//}
