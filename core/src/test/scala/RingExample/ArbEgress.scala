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
