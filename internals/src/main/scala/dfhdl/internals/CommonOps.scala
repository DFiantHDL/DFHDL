package dfhdl.internals
import annotation.tailrec
object CommonOps:
  extension (value: BigInt)
    def bitsWidth(signed: Boolean): Int =
      if (value > 0)
        if (signed) value.bitLength + 1 else value.bitLength
      else if (value == 0)
        if (signed) 2 else 1
      else if (value == -1) 2
      else value.bitLength + 1 // value < 0
    def toBitVector(width: Int): BitVector =
      val vec = BitVector(value.toByteArray)
      if (value < 0 && vec.length < width)
        BitVector.fill(width - vec.length)(true) ++ vec
      else vec.resize(width)
    def asUnsigned(ofWidth: Int): BigInt =
      if (value >= 0) value
      else (BigInt(1) << ofWidth) + value
    def asSigned(ofWidth: Int): BigInt =
      val isNegative = value.testBit(ofWidth - 1)
      if (isNegative) value - (BigInt(1) << ofWidth)
      else value
    def asUnsigned: BigInt = asUnsigned(bitsWidth(false))
    def truncateAsUnsigned(width: Int): BigInt =
      val mask = (BigInt(1) << width) - 1
      value & mask
  end extension

  extension (value: Int) def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

  extension [T](list: List[T])
    def reduceTree(f: (T, T) => T): T =
      require(list.nonEmpty, "Cannot reduce an empty list")

      def reduceRecursive(list: List[T]): T = list match
        case head :: Nil => head // Base case: if the list has one element, return it.
        case _           =>
          // Split the sequence into two halves
          val (left, right) = list.splitAt(list.length / 2)
          // Reduce the left and right halves and combine them using the function `f`
          f(reduceRecursive(left), reduceRecursive(right))

      reduceRecursive(list)
  end extension

  extension (value: BigInt.type)
    // get the maximum BigInt given a bits width
    def maxUnsignedFromWidth(width: Int): BigInt = BigInt(2).pow(width) - 1
    def maxSignedFromWidth(width: Int): BigInt = BigInt(2).pow(width - 1) - 1
    def minSignedFromWidth(width: Int): BigInt = -BigInt(2).pow(width - 1)

  extension (value: Int)
    def toBitVector(width: Int): BitVector = BigInt(value).toBitVector(width)
    def toPaddedString(maxValue: Int, padWithZeros: Boolean): String =
      val pad = if (padWithZeros) "0" else ""
      s"%$pad${maxValue.toString.length}d".format(value)
    def toPaddedString(maxValue: Int): String =
      toPaddedString(maxValue, true)

  extension (value: Boolean) def toBitVector(width: Int): BitVector = BitVector.fill(width)(value)

end CommonOps

export CommonOps.*
