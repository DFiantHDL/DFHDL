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

package DFiant
package compiler.printer

trait HasCodeString {
  def codeString(implicit printer: Printer) : String
}

trait CodeStringOf[T] {
  def apply(t : T)(implicit printer: Printer) : String
}
object CodeStringOf {
  implicit def ev[T <: HasCodeString](implicit printer: Printer) : CodeStringOf[T] = new CodeStringOf[T] {
    override def apply(t : T)(implicit printer: Printer) : String = t.codeString
  }
  implicit def evBoolean : CodeStringOf[Boolean] = new CodeStringOf[Boolean] {
    override def apply(t : Boolean)(implicit printer: Printer) : String = t.toString
  }
  implicit def evInt : CodeStringOf[Int] = new CodeStringOf[Int] {
    override def apply(t : Int)(implicit printer: Printer) : String = t.toString
  }
  implicit def evLong : CodeStringOf[Long] = new CodeStringOf[Long] {
    override def apply(t : Long)(implicit printer: Printer) : String = t.toString
  }
  implicit def evBigInt : CodeStringOf[BigInt] = new CodeStringOf[BigInt] {
    override def apply(t : BigInt)(implicit printer: Printer) : String = t.codeString
  }
}
