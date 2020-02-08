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

import ZFiant._ //Required in any DFiant compilation program

trait ID extends DFDesign { //This our `ID` dataflow design
  val i = DFUInt(8) <> IN  //The input port is a signed 16-bit integer
  val o = DFUInt(8) <> OUT	//The output port is a signed 16-bit integer
  o <> i //Trivial direct input-to-output connection
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

trait ContainerConnLoop extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new ID {}
  io.i <> io.o
  o <> io.o
}


object IDTopApp extends App {
  val top = new IDTop {}
  import compiler._
  top.printCodeString() //.flatten(top.id1)

}
//object IDTopApp extends DFApp.VHDLCompiler[IDTop] //The IDTop compilation program entry-point