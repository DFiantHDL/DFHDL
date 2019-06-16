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

//package DFiant
//
//import org.scalacheck._
//import shapeless.test.illTyped
//import psuedoVendor.family.device._
//
//class DFComponentTest extends Properties("DFComponentTest") {
//  trait Adder[Left <: DFAny, Right <: DFAny, Result <: DFAny] extends DFComponent[Adder[Left, Right, Result]] {
//    val left : Left <> IN
//    val right : Right <> IN
//    val result : Result <> OUT
//  }
//
//  object Adder {
//    type DFU[LW, RW, OW] = Adder[DFUInt[LW], DFUInt[RW], DFUInt[OW]]
//    implicit def fro[LW, RW, OW] : DFComponent.Implementation[Adder.DFU[LW, RW, OW]] = ifc => {
//      import ifc._
//      result := left + right
//    }
//  }
//
//  new DFDesign() {
//    val a = DFUInt(8)
//    new Adder.DFU[8, 8, 8] {
//      val left = a
//      val right = 7
//      val result = OPEN
//    }
//  }
//
//}
//
//
