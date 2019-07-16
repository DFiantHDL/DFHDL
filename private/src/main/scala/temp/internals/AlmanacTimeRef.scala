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
////Time reference of the address
////Negative step - Prev(Past time)
////Zero (0) step - Current(Present time)
////Positive step - Next(Future time)
//class AlmanacTimeRef private (val step : Int) {
//  override def toString: String = if(step < 0) s"P$step" else if (step == 0) "C" else s"N$step"
//  def stepBy(deltaStep : Int) = if (deltaStep == 0) this else AlmanacTimeRef(step + deltaStep)
//}
//
//object AlmanacTimeRef {
//  case object Current extends AlmanacTimeRef(0)
//
//  def apply(step : Int) = new AlmanacTimeRef(step)
//}
//
//
