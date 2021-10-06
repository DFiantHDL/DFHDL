package DFiant.internals
import annotation.tailrec
object CommonOps:
  extension (value: BigInt)
    def bitsWidth(signed: Boolean): Int =
      if (value > 0)
        if (signed) value.bitLength + 1 else value.bitLength
      else if (value == 0)
        if (signed) 2 else 1
      else if (value == -1) 2
      else value.bitLength + 1 //value < 0
    def toBitVector(width: Int): BitVector =
      val vec = BitVector(value.toByteArray)
      if (value < 0 && vec.length < width)
        BitVector.fill(width - vec.length)(true) ++ vec
      else vec.resize(width)
    def asUnsigned(ofWidth: Int): BigInt =
      if (value >= 0) value
      else
        BigInt(2).pow(ofWidth) + value
    def asUnsigned: BigInt = asUnsigned(bitsWidth(false))
  end extension

  extension (value: Int)
    def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

  extension [T](list: Iterable[T])
    @tailrec private def reduceTreeRecur(
        recurList: Iterable[T],
        f: (T, T) => T
    ): T =
      if (recurList.size <= 1) list.head
      else
        reduceTreeRecur(
          recurList
            .grouped(2)
            .map {
              case l :: r :: Nil => f(l, r)
              case l :: Nil      => l
            }
            .to(Iterable),
          f
        )
    def reduceTree(f: (T, T) => T): T = reduceTreeRecur(list, f)
  end extension

  extension (value: BigInt.type)
    //get the maximum BigInt given a bits width
    def maxUnsignedFromWidth(width: Int): BigInt = BigInt(2).pow(width) - 1
    def maxSignedFromWidth(width: Int): BigInt = BigInt(2).pow(width - 1) - 1
    def minSignedFromWidth(width: Int): BigInt = -BigInt(2).pow(width - 1)

  extension (value: Int)
    def toBitVector(width: Int): BitVector = BigInt(value).toBitVector(width)
    def toPaddedString(maxValue: Int): String =
      s"%0${maxValue.toString.length}d".format(value)

  extension (value: Boolean)
    def toBitVector(width: Int): BitVector = BitVector.fill(width)(value)

end CommonOps

export CommonOps.*
