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
//abstract class Branch(nodesNum : Int, nodeID : Int) {
//  Branch =>
//  //IO ports
//  val inFlit                  : DFFlit#IN
//  val outFlit                 : DFFlit#OUT
//  val localCtrlFlitFromBranch : DFFlit#OUT
//  val localDataFlitFromBranch : DFFlit#OUT
//  val localCtrlFlitToBranch   : DFFlit#IN
//  val localDataFlitToBranch   : DFFlit#IN
//
//  private val remoteCtrlFlit  = DFFlit(nodesNum)
//  private val remoteDataFlit  = DFFlit(nodesNum)
//
//  new ArbIngress(nodesNum, nodeID) {
//    val inFlit                : DFFlit#IN  = Branch.inFlit
//    val localCtrlFlit         : DFFlit#OUT = Branch.localCtrlFlitFromBranch
//    val localDataFlit         : DFFlit#OUT = Branch.localDataFlitFromBranch
//    val remoteCtrlFlit        : DFFlit#OUT = Branch.remoteCtrlFlit
//    val remoteDataFlit        : DFFlit#OUT = Branch.remoteDataFlit
//  }
//
//  new ArbEgress(nodesNum, nodeID) {
//    val outFlit               : DFFlit#OUT = Branch.outFlit
//    val localCtrlFlit         : DFFlit#IN  = Branch.localCtrlFlitToBranch
//    val localDataFlit         : DFFlit#IN  = Branch.localDataFlitToBranch
//    val remoteCtrlFlit        : DFFlit#IN  = Branch.remoteCtrlFlit
//    val remoteDataFlit        : DFFlit#IN  = Branch.remoteDataFlit
//  }
//}
