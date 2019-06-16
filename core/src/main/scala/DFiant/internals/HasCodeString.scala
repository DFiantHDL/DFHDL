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

package DFiant.internals

trait HasCodeString {
  def codeString : String
}

trait CodeStringOf[T] {
  def apply(t : T) : String
}
object CodeStringOf {
  implicit def ev[T <: HasCodeString] : CodeStringOf[T] = t => t.codeString
  implicit def evBoolean : CodeStringOf[Boolean] = t => t.toString
  implicit def evInt : CodeStringOf[Int] = t => t.toString
  implicit def evLong : CodeStringOf[Long] = t => t.toString
  implicit def evBigInt : CodeStringOf[BigInt] = t => t.codeString
}
