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
package csprinter
import internals.BitVector

trait HasCodeString {
  def codeString(implicit printer: CSPrinter) : String
}

trait CodeStringOf[T] {
  def apply(t : T)(implicit printer: CSPrinter) : String
}
object CodeStringOf {
  implicit def ev[T <: HasCodeString](implicit printer: CSPrinter) : CodeStringOf[T] = new CodeStringOf[T] {
    override def apply(t : T)(implicit printer: CSPrinter) : String = t.codeString
  }
  implicit def evBoolean : CodeStringOf[Boolean] = new CodeStringOf[Boolean] {
    override def apply(t : Boolean)(implicit printer: CSPrinter) : String = t.toString
  }
  implicit def evInt : CodeStringOf[Int] = new CodeStringOf[Int] {
    override def apply(t : Int)(implicit printer: CSPrinter) : String = t.toString
  }
  implicit def evLong : CodeStringOf[Long] = new CodeStringOf[Long] {
    override def apply(t : Long)(implicit printer: CSPrinter) : String = t.toString
  }
  implicit def evBigInt : CodeStringOf[BigInt] = new CodeStringOf[BigInt] {
    override def apply(t : BigInt)(implicit printer: CSPrinter) : String = t.codeString
  }
  implicit def csoIntervalBigInt : CodeStringOf[Interval[BigInt]] = new CodeStringOf[Interval[BigInt]] {
    def apply(t : Interval[BigInt])(implicit printer: CSPrinter) : String = {
      import continuum.bound._
      val lower = t.lower.bound match {
        case Closed(v) => v
        case Open(v) => v+1
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      val upper = t.upper.bound match {
        case Closed(v) => v
        case Open(v) => v-1
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      if (lower == upper) lower.codeString
      else s"${lower.codeString} to ${upper.codeString}"
    }
  }
  implicit def csoBitVector : CodeStringOf[BitVector] = new CodeStringOf[BitVector] {
    def apply(vec : BitVector)(implicit printer: CSPrinter) : String =
      if (vec.length % 4 == 0) s"""h"${vec.toHex}""""
      else s"""b"${vec.toBin}""""
  }
  implicit class CodeStringExtension[T](t : T)(implicit codeStringOf: CodeStringOf[T]) {
    def codeString(implicit printer: CSPrinter) : String = codeStringOf(t)
  }

  implicit class BigIntCodeString(value : BigInt) {
    def codeString : String = {
      if (value.isValidInt) s"$value"
      else if (value.isValidLong) s"${value}L"
      else s"""BigInt("$value")"""
    }
  }

}
