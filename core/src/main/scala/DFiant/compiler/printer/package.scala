package DFiant
package compiler

package object printer {
  implicit def csoIntervalBigInt : CodeStringOf[Interval[BigInt]] = new CodeStringOf[Interval[BigInt]] {
    def apply(t : Interval[BigInt])(implicit printer: Printer) : String = {
      import continuum.bound._
      val lower = t.lower.bound match {
        case Closed(v) => v
        case Open(v) => v-1
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      val upper = t.upper.bound match {
        case Closed(v) => v
        case Open(v) => v+1
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      if (lower == upper) lower.codeString
      else s"${lower.codeString} to ${upper.codeString}"
    }
  }
  implicit def csoBitVector : CodeStringOf[BitVector] = new CodeStringOf[BitVector] {
    def apply(vec : BitVector)(implicit printer: Printer) : String =
      if (vec.length % 4 == 0) s"""h"${vec.toHex}""""
      else s"""b"${vec.toBin}""""
  }

  implicit class CodeStringExtension[T](t : T)(implicit codeStringOf: CodeStringOf[T]) {
    def codeString(implicit printer: Printer) : String = codeStringOf(t)
  }

  implicit class BigIntCodeString(value : BigInt) {
    def codeString : String = {
      if (value.isValidInt) s"$value"
      else if (value.isValidLong) s"${value}L"
      else s"""BigInt("$value")"""
    }
  }
}
