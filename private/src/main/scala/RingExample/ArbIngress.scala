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
