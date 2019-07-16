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

//package DFiant.internals
//
//import DFiant.DFAny.Token
//
//trait AlmanacID
//
//case class AlmanacIDConst(token : Token) extends AlmanacID {
//  override def toString: String = s"$token"
//}
//
//
//trait AlmanacIDUnique extends AlmanacID {
//  protected val unique : Int
//
//  override def toString: String = s"V$unique"
//}
//
//object AlmanacID {
//  protected var idsNum : Int = 0
//
//  def apply() : AlmanacID = {
//    val ret = new AlmanacIDUnique {protected val unique : Int = idsNum}
//    idsNum += 1
//    ret
//  }
//}
//
//
