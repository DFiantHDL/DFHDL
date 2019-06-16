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
//case class Ring (nodesNum : Int) {
//  private def nextNodeIdx(currentNodeIdx : Int): Int = {
//    (currentNodeIdx + 1) % nodesNum
//  }
//  private def prevNodeIdx(currentNodeIdx : Int): Int = {
//    (currentNodeIdx - 1) % nodesNum
//  }
//  private val ringNodes = {
//    Defs.nodesNum = nodesNum
//    val l2rFlitConn = Array.tabulate(nodesNum-1)(_ => DFFlit(nodesNum))
//    val r2lFlitConn = Array.tabulate(nodesNum-1)(_ => DFFlit(nodesNum))
//    val _ringNodes = Array.tabulate(nodesNum)(i => new RingNode(nodesNum, i) {
//        val leftInFlit    : DFFlit#IN  = l2rFlitConn(prevNodeIdx(i))
//        val leftOutFlit   : DFFlit#OUT = r2lFlitConn(prevNodeIdx(i))
//        val rightInFlit   : DFFlit#IN  = r2lFlitConn(nextNodeIdx(i))
//        val rightOutFlit  : DFFlit#OUT = l2rFlitConn(nextNodeIdx(i))
//      }
//    )
//    _ringNodes
//  }
//
//  def node(i : Int) = ringNodes(i)
//}
