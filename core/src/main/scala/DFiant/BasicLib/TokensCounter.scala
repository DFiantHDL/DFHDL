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

//package DFiant.BasicLib

//import DFiant._

//object TokensCounter {
//  def apply(in : DFAny, supremLimit : Int) : DFUInt = {
//    val cnt = DFUInt.rangeUntil(supremLimit-1).initPrev(0)
//
//    ifdf (in.isNotEmpty) {
//      ifdf (cnt == supremLimit-1) {
//        cnt := 0
//      } elsedf {
//        cnt := cnt + 1
//      }
//    } elsedf {
//      in.dontConsume()
//      cnt.dontProduce() //cnt.prev preserves latest valid value
//    }
//    cnt
//  }
//
//}
