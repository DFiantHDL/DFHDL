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
  val x = DFUInt(16) <> IN  //The input port is a signed 16-bit integer
  val y = DFUInt(16) <> OUT	//The output port is a signed 16-bit integer
  val tempX = DFUInt(16)
  val tempY = DFUInt(16)
  tempX <> x
  tempY := tempX
  tempY <> y
//  y <> x //Trivial direct input-to-output connection
}

trait IDTop extends DFDesign { //This our `IDTop` dataflow design
  val x = DFUInt(16) <> IN  //The input port is a signed 16-bit integer
  val y = DFUInt(16) <> OUT	//The output port is a signed 16-bit integer
  val id1 = new ID {} //First instance of the `ID` design
  val id2 = new ID {} //Second instance of the `ID` design
  id1.x <> x      //Connecting parent input port to child input port
  id1.y <> id2.x  //Connecting sibling instance ports
  id2.y <> y      //Connecting parent output port to child output port
}

object IDTopApp extends App {
  val top = new IDTop {}
  import DFCompiler._
  top.db.flatten(top.id1).printCodeString()

}
//object IDTopApp extends DFApp.VHDLCompiler[IDTop] //The IDTop compilation program entry-point