import scodec.bits._

def asUnsigned(num : BigInt) = {
  val pre : Array[Byte] = Array(0)
  BigInt(pre ++ num.toByteArray)
}

val x: BitVector = BitVector(BigInt(0xF234).toByteArray)
x.drop(8)

implicit class BigIntExtras(value : BigInt) {
  //get the maximum BigInt given a bits width
  def bitsWidthToMaxBigIntBits(width : Int) : BigInt = {
    var mask: BigInt = 2
    mask = mask.pow(width) - 1
    mask
  }
  def bitsWidth : Int = {
    if (value > 0) value.bitLength
    else if (value == 0) 1
    else (-value).bitLength + 1
  }
  def asUnsigned : BigInt = {
    if (value >= 0) value
    else {
      val mask = bitsWidthToMaxBigIntBits(bitsWidth)
      BigInt(Array(0.toByte) ++ value.toByteArray) & mask
    }
  }
}

BigInt(0).asUnsigned.toString(16)

