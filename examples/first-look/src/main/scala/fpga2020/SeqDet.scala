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
package fpga2020
import DFiant._

import fsm._
@df class SeqDet extends DFDesign {
  final val seqIn  = DFBool() <> IN
  final val detOut = DFBool() <> OUT
  private val S0    = step {detOut := 0}
  private val S1    = step {detOut := 0}
  private val S10   = step {detOut := 0}
//  private val S100  = step {detOut := 0}
//  private val S1001 = step {detOut := 1}

  private val det_fsm =
    S0    =?> seqIn ==> S1    ++
    S1    =?> seqIn ==> S1    ++ S1     ==> S10   ++
    S10   =?> seqIn ==> S1    ++ S10    ==> S0//S100  ++
//    S100  =?> seqIn ==> S1001 ++ S100   ==> S0    ++
//    S1001 =?> seqIn ==> S1    ++ S1001  ==> S10

  det_fsm.elaborate
}

//trait SeqDetTest extends DFSimulator {
//  val TestSeq = Seq(1, 1, 0, 1, 0, 0, 1, 0, 1)
//  val seqIn = DFBool() init TestSeq.reverse
//  val dut = new SeqDet {}
//  dut.seq <> seqIn.prev(TestSeq.length)
//  sim.report(dfs"det: ${dut.det}")
//}
//
object SeqDetApp extends App {
  val seqDet = new SeqDet
  seqDet.printCodeString()
}