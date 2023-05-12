package dfhdl.internals

import scala.collection.immutable.SortedMap

case class RangeValue[+T](range: Range, value: T)

case class RangeMap[+T](private val internalMap: SortedMap[Int, RangeValue[T]] = SortedMap.empty):

  def isEmpty: Boolean = internalMap.isEmpty

  def insert[U >: T](range: Range, value: U): RangeMap[U] =
    if (range.isEmpty || range.step != 1)
      throw new IllegalArgumentException("Only non-empty ranges with a step of 1 are allowed")

    if (contains(range)) throw new IllegalArgumentException("Overlapping range")

    RangeMap(internalMap + (range.start -> RangeValue(range, value)))

  def get(key: Int): Option[T] =
    val rangeValueOpt = internalMap.rangeUntil(key + 1).lastOption
    rangeValueOpt.flatMap { case (_, rangeValue) =>
      if (rangeValue.range.contains(key)) Some(rangeValue.value) else None
    }

  def contains(range: Range): Boolean =
    val previous = internalMap.get(range.start - 1)
    val next = internalMap.get(range.start)

    (previous.isDefined && previous.get.range.end >= range.start) ||
    (next.isDefined && range.end >= next.get.range.start)
end RangeMap

object RangeMap:
  def empty[T]: RangeMap[T] = RangeMap[T]()
