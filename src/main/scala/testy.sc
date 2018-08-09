import java.nio.ByteOrder
import DFiant.internals._
import scodec.bits._

implicit class BigIntExtras(value : BigInt) {
  //get the maximum BigInt given a bits width
  private def bitsWidthToMaxBigIntBits(width : Int) : BigInt = {
    var mask: BigInt = 2
    mask = mask.pow(width) - 1
    mask
  }

  def bitsWidth : Int = {
    if (value > 0) value.bitLength
    else if (value == 0) 1
    else (-value).bitLength + 1
  }
  def asUnsigned(ofWidth : Int) : BigInt = {
    if (value >= 0) value
    else {
      BigInt(2).pow(ofWidth) + value
    }
  }
  def asUnsigned : BigInt = asUnsigned(bitsWidth)
  def codeString : String = {
    if (value.isValidInt) s"$value"
    else if (value.isValidLong) s"${value}L"
    else s"""BigInt("$value")"""
  }
  def toBitVector(width : Int) : BitVector = BitVector(value.toByteArray).toLength(width)
}

implicit class BitVectorExtras(vec : BitVector) {
  def lzc : Long = {
    val l = for (i <- vec.length-1 to 0L by -1L if vec(i)) yield i
    if (l.isEmpty) vec.length else l.head
  }
  def lengthOfValue : Long = if (lzc == vec.length) 1L else vec.length - lzc
  def toLength(newLength : Long) : BitVector = {
    if (newLength > vec.length) vec.padLeft(newLength)
    else if (newLength < vec.length) vec.drop(vec.length - newLength)
    else vec
  }
  def bit(idx : Long) : Boolean = vec(idx)
  def bits(hiIdx : Long, loIdx : Long) : BitVector = {
    vec.slice(loIdx, hiIdx + 1)
  }
  def padToMulsOf(bitsNum : Int) : BitVector = {
    val paddedVecLength = ((vec.length + bitsNum - 1) / bitsNum) * bitsNum
    vec.padLeft(paddedVecLength)
  }
  def toShortString : String = {
    val nibble = 4
    val lov = lengthOfValue
    //narrowing the vector by removing all the leftest zeros
    val narrowVec = vec.takeRight(lov)
    //default printing of bitvectors is padding-right in `toHex`.
    //padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    s"0x${narrowVec.padToMulsOf(nibble).toHex}"
  }
  def toBigInt : BigInt = {
    val len = vec.length
    val ext = vec.padLeft(len + 1)
    BigInt(ext.padToMulsOf(8).toByteArray)
  }
}

implicit class IntExtras(value : Int) {
  def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
}

implicit class LongExtras(value : Long) {
  def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
}

implicit class BooleanExtras(value : Boolean) {
  def toBitVector(width : Int) : BitVector = BitVector.fill(width)(value)
}


val a = bin"11000".reverseBitOrder
a.bits(3,2).reverseBitOrder.toShortString
a.lzc

