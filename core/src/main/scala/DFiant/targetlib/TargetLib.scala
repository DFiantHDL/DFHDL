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

package DFiant.targetlib

trait TargetLib {

  val DFUIntOps : TargetLib.DFUIntOps
  val DFSIntOps : TargetLib.DFSIntOps
  val DFBitsOps : TargetLib.DFBitsOps
  val DFBoolOps : TargetLib.DFBoolOps

}


object TargetLib {
  implicit val default : TargetLib = Xilinx.FPGAs.`XC7VX485T-2FFG1761C`.targetLib
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    import DFiant.targetlib.DFUIntOps._
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
    import DFiant.targetlib.DFSIntOps._
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
    import DFiant.targetlib.DFBitsOps._
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
    import DFiant.targetlib.DFBoolOps._
    implicit val `Comp||` : `Comp||` => Unit
    implicit val `Comp&&` : `Comp&&` => Unit
    implicit val `Comp^`  : `Comp^` => Unit
    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}