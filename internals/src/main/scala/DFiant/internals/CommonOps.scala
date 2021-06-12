package DFiant.internals
import annotation.tailrec
object CommonOps:
  extension (value: BigInt)
    def bitsWidth(signed: Boolean): Int =
      if (value > 0)
        if (signed) value.bitLength + 1 else value.bitLength
      else if (value == 0)
        if (signed) 2 else 1
      else (-value).bitLength + 1

  extension (value: Int)
    def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

  extension [T](list: Iterable[T]) {
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
  }

export CommonOps.*
