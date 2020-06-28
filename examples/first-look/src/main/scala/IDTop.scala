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

import DFiant._
import DFiant.sim.DFSimDesign
import internals._
@df class ID extends DFDesign { //This our `ID` dataflow design
  val i = DFUInt(8) <> IN init 0 //The input port is a signed 16-bit integer
  val o = DFUInt(8) <> OUT init 0	//The output port is a signed 16-bit integer
  o := o + i.prev
  val a = new Ifc <> FLIP
  val b = new Ifc <> ASIS
  a <> b
  sim.assert(true, msg"HAHA$i")
  sim.finish()
}

@df class IDTop extends DFDesign { //This our `IDTop` dataflow design
  val x = DFUInt(8) <> IN  //The input port is a signed 16-bit integer
  val y = DFUInt(8) <> OUT	//The output port is a signed 16-bit integer
  val id1 = new ID //First instance of the `ID` design
  val id2 = new ID //Second instance of the `ID` design
  id1.i <> x       //Connecting parent input port to child input port
  id1.o <> id2.i   //Connecting sibling instance ports
  id2.o <> y       //Connecting parent output port to child output port
}

@df class Ifc extends DFInterface {
//  val AA = DFUInt(8) <> IN
//  val BB = DFUInt(8) <> OUT
  val z = new Ifc2 <> FLIP //setNameFlatten(DFInterface.NameFlatten.IgnoreOwnerName)
}
@df class Ifc2 extends DFInterface(DFOwner.NameFlatten.IgnoreOwnerName) {
  val AA = DFUInt(8) <> IN
  val BB = DFUInt(8) <> OUT
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

@df class ContainerConnLoop extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new ID
  io.i <> io.o
  o <> io.o
}

@df class IDTopTest extends DFSimDesign {
  val i = DFBits(8) <> IN
  val o = DFUInt(8) <> OUT
//  val ididid = new IDTop
//  ifdf(true) {
//    val fib = DFUInt(8) init(0, 1)
//    fib := fib.prev + fib.prev(2)
//  }
  val sum = DFUInt(8) init 0
  ifdf(true) {
    val scope = new DFScope(Some("Meshuga")) {
      val b = DFBool()
      val temp = i.uint
      sum := sum + temp
    }
  }
  o := sum
}

object IDTopApp extends App {
  val top = new IDTopTest
  import compiler.backend.vhdl.v93
  val designDB = top.printCodeString//.printGenFiles()
//  val cmp = new Compiled(designDB, designDB.top)
//  println(cmp.entity)

}
//object IDTopApp extends DFApp.VHDLCompiler[IDTop] //The IDTop compilation program entry-point