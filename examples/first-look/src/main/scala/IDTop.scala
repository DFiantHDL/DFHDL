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
package example2

import DFiant._ //Required in any DFiant compilation program

trait ID extends DFDesign { //This our `ID` dataflow design
  val i = DFUInt(8) <> IN init 0 //The input port is a signed 16-bit integer
  val o = DFUInt(8) <> OUT init 0	//The output port is a signed 16-bit integer
  o := o + i.prev
  sim.assert(true, msg"HAHA$i")
  sim.finish()
}

trait IDTop extends DFDesign { //This our `IDTop` dataflow design
  val x = DFUInt(8) <> IN  //The input port is a signed 16-bit integer
  val y = DFUInt(8) <> OUT	//The output port is a signed 16-bit integer
  val id1 = new ID {} //First instance of the `ID` design
  val id2 = new ID {} //Second instance of the `ID` design
  id1.i <> x      //Connecting parent input port to child input port
  id1.o <> id2.i  //Connecting sibling instance ports
  id2.o <> y      //Connecting parent output port to child output port
}
//trait IDTop extends DFDesign {
//  final val x = DFUInt(8) <> IN
//  final val y = DFUInt(8) <> OUT
//  final val id1_i = DFUInt(8)
//  final val id1_o = DFUInt(8)
//  final val id1 = new ID {
//    i <> id1_i
//    id1_o <> o
//  }
//  final val id2_i = DFUInt(8)
//  final val id2_o = DFUInt(8)
//  final val id2 = new ID {
//    i <> id2_i
//    id2_o <> o
//  }
//  id1_i <> x
//  id2_i <> id1_o
//  y <> id2_o
//}

trait ContainerConnLoop extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new ID {}
  io.i <> io.o
  o <> io.o
}

object IDTopTest extends DFSimulator {
//  val iii = DFUInt(8) <> IN
//  val ooo = DFUInt(8) <> OUT
  val ididid = new IDTop {}
//  ididid.x <> iii
//  ididid.y <> ooo
}


object IDTopApp extends App {
//  val top = new IDTopTest {}
  import compiler._
  import backend.vhdl._
  val designDB = IDTopTest.compile.printCodeString().printGenFiles()
//  val cmp = new Compiled(designDB, designDB.top)
//  println(cmp.entity)

}
//object IDTopApp extends DFApp.VHDLCompiler[IDTop] //The IDTop compilation program entry-point