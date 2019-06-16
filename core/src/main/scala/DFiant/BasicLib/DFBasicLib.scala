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

package DFiant.BasicLib

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFSIntOps : DFBasicLib.DFSIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps
  val DFBoolOps : DFBasicLib.DFBoolOps

}


object DFBasicLib {
  implicit val default : DFBasicLib = Xilinx.FPGAs.`XC7VX485T-2FFG1761C`.basicLib
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    import DFiant.BasicLib.DFUIntOps._
    implicit val `Comp+`  : `Comp+` => Unit
    implicit val `Comp-`  : `Comp-` => Unit
    implicit val `Comp*`  : `Comp*` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
    implicit val `Comp<`  : `Comp<` => Unit
    implicit val `Comp>`  : `Comp>` => Unit
    implicit val `Comp<=` : `Comp<=` => Unit
    implicit val `Comp>=` : `Comp>=` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFSInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFSIntOps {
    import DFiant.BasicLib.DFSIntOps._
    implicit val `Comp+`  : `Comp+` => Unit
    implicit val `Comp-`  : `Comp-` => Unit
    implicit val `Comp*`  : `Comp*` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
    implicit val `Comp<`  : `Comp<` => Unit
    implicit val `Comp>`  : `Comp>` => Unit
    implicit val `Comp<=` : `Comp<=` => Unit
    implicit val `Comp>=` : `Comp>=` => Unit

    implicit val `Comp<<` : `Comp<<` => Unit
    implicit val `Comp>>` : `Comp>>` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBitsOps {
    import DFiant.BasicLib.DFBitsOps._
    implicit val `Comp|`  : `Comp|` => Unit
    implicit val `Comp&`  : `Comp&` => Unit
    implicit val `Comp^`  : `Comp^` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit

    implicit val `Comp<<` : `Comp<<` => Unit
    implicit val `Comp>>` : `Comp>>` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBool
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBoolOps {
    import DFiant.BasicLib.DFBoolOps._
    implicit val `Comp||` : `Comp||` => Unit
    implicit val `Comp&&` : `Comp&&` => Unit
    implicit val `Comp^`  : `Comp^` => Unit
    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}