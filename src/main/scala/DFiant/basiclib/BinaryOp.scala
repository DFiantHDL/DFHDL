package DFiant.basiclib

import DFiant.core._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._

trait BinaryOpRight[L <: DFAny, R] {
  val width : Int
  val currentEntry : AlmanacEntry
}

abstract class BinaryOpRightConstInt[L <: DFAny, R <: Int](val right : R)(implicit bitsWidthOf: BitsWidthOf.Int[Int]) extends BinaryOpRight[L, R] {
  val width : Int = bitsWidthOf(right)
  val currentEntry : AlmanacEntry = AlmanacEntryConst(right)
}
//abstract class BinaryOpRightConstLong[L <: DFAny, R <: Long](right : R) extends BinaryOpRight[L, R] {
//  def getCurrentEntry : AlmanacEntry = AlmanacEntryConst(right)
//}
//abstract class BinaryOpRightConstBigInt[L <: DFAny, R <: BigInt](right : R) extends BinaryOpRight[L, R] {
//  def getCurrentEntry : AlmanacEntry = AlmanacEntryConst(right)
//}

abstract class BinaryOpRightDFVar[L <: DFAny, R <: DFAny](val right : R) extends BinaryOpRight[L, R] {
  val width : Int = right.width
  val currentEntry : AlmanacEntry = right.getCurrentEntry
}