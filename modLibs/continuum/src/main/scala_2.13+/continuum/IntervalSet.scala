package scala.collection.immutable

import scala.collection.immutable.{RedBlackTree => RB}
import scala.collection.mutable

import continuum.Interval

object IntervalSet extends {
  def empty[T](implicit conv: T => Ordered[T]): IntervalSet[T] =
    new IntervalSet()

  def apply[T](intervals: Interval[T]*)(implicit
      conv: T => Ordered[T]
  ): IntervalSet[T] = create(intervals)

  def create[T](
      intervalSeq: Seq[Interval[T]]
  )(implicit conv: T => Ordered[T]): IntervalSet[T] =
    intervalSeq.foldLeft(empty[T])(_ + _)
}

/**
  * A set containing 0 or more intervals. Intervals which may be unioned together are automatically
  * coalesced, so at all times an interval set contains the minimum number of necessary intervals.
  * Interval sets are immutable and persistent.
  */
class IntervalSet[T](tree: RB.Tree[Interval[T], Unit])(implicit
    conv: T => Ordered[T]
) extends SortedSet[Interval[T]]
    with SortedSetOps[Interval[T], SortedSet, IntervalSet[T]]
    with Serializable {

  override protected def fromSpecific(
      coll: IterableOnce[Interval[T]]
  ): IntervalSet[T] = IntervalSet.create[T](coll.iterator.to(Seq))
  override protected def newSpecificBuilder
      : mutable.Builder[Interval[T], IntervalSet[T]] = ???

  def this()(implicit conv: T => Ordered[T]) = this(null)

  override def ordering: Ordering[Interval[T]] = Ordering.ordered

  override def className = "IntervalSet"

  override def size = RB.count(tree)

  override def head       = RB.smallest(tree).key
  override def headOption = if (RB.isEmpty(tree)) None else Some(head)
  override def last       = RB.greatest(tree).key
  override def lastOption = if (RB.isEmpty(tree)) None else Some(last)

  override def tail = new IntervalSet(RB.delete(tree, firstKey))
  override def init = new IntervalSet(RB.delete(tree, lastKey))

  override def drop(n: Int) = {
    if (n <= 0) this
    else if (n >= size) empty
    else newSet(RB.drop(tree, n))
  }

  override def take(n: Int) = {
    if (n <= 0) empty
    else if (n >= size) this
    else newSet(RB.take(tree, n))
  }

  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else newSet(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int) = take(size - n)
  override def takeRight(n: Int) = drop(size - n)
  override def splitAt(n: Int)   = (take(n), drop(n))

  private[this] def countWhile(p: Interval[T] => Boolean): Int = {
    var result = 0
    val it     = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }
  override def dropWhile(p: Interval[T] => Boolean) = drop(countWhile(p))
  override def takeWhile(p: Interval[T] => Boolean) = take(countWhile(p))
  override def span(p: Interval[T] => Boolean)      = splitAt(countWhile(p))

  private def newSet(t: RB.Tree[Interval[T], Unit]) = new IntervalSet(t)

  override def empty: IntervalSet[T] = IntervalSet.empty

  override def excl(elem: Interval[T]): IntervalSet[T] = {
    val intersectings = intersecting(elem)
    val differences   = intersectings.flatMap(_ difference elem)
    val diff          = intersectings.foldLeft(tree)(RB.delete(_, _))
    newSet(differences.foldLeft(diff)(RB.update(_, _, (), false)))
  }
  override def incl(elem: Interval[T]): IntervalSet[T] = {
    val unionables: IntervalSet[T] = unioning(elem)
    val union                      = unionables.foldLeft(elem)((a, b) => (a union b).get)
    val diff                       = unionables.foldLeft(tree)(RB.delete(_, _))
    newSet(RB.update(diff, union, (), false))
  }

  override def iteratorFrom(start: Interval[T]): Iterator[Interval[T]] =
    ??? //super.iteratorFrom(start)

  override def contains(interval: Interval[T]): Boolean = {
    val intersectings = intersecting(interval)
    intersectings.size == 1 && intersectings.head.encloses(interval)
  }

  def containsPoint(point: T): Boolean = contains(Interval.point(point))

  override def iterator: Iterator[Interval[T]] = RB.keysIterator(tree)

  override def keysIteratorFrom(start: Interval[T]): Iterator[Interval[T]] = {
    val keys = RB.keysIterator(tree)
    keys.drop(keys.indexOf(start))
  }

  override def foreach[U](f: Interval[T] => U) = RB.foreachKey(tree, f)

  override def rangeImpl(
      from: Option[Interval[T]],
      until: Option[Interval[T]]
  ): IntervalSet[T] = newSet(RB.rangeImpl(tree, from, until))
  override def range(from: Interval[T], until: Interval[T]): IntervalSet[T] =
    newSet(RB.range(tree, from, until))
  override def rangeFrom(from: Interval[T]): IntervalSet[T] =
    newSet(RB.from(tree, from))
  override def rangeTo(to: Interval[T]): IntervalSet[T] =
    newSet(RB.to(tree, to))
  override def rangeUntil(until: Interval[T]): IntervalSet[T] =
    newSet(RB.until(tree, until))

  override def firstKey = head
  override def lastKey  = last

  /**
    * Returns the subset of intervals which intersect with the given interval.
    */
  def intersecting(interval: Interval[T]): IntervalSet[T] = {
    val buf = mutable.ArrayBuffer[Interval[T]]()
    def loop(t: RB.Tree[Interval[T], Unit]): Unit = {
      if (!RB.isEmpty(t)) {
        if (t.key intersects interval) buf += t.key
        if (
          !RB.isEmpty(t.left) && (RB
            .greatest(t.left)
            .key
            .upper intersects interval.lower)
        )
          loop(t.left)
        if (
          !RB.isEmpty(t.right) && (RB
            .smallest(t.right)
            .key
            .lower intersects interval.upper)
        )
          loop(t.right)
      }
    }
    loop(tree)
    IntervalSet.create(buf.toIndexedSeq)
  }

  /**
    * Tests if the provided interval intersects with any of the intervals in this set.
    */
  def intersects(interval: Interval[T]): Boolean =
    rangeFrom(interval).head intersects interval

  /**
    * Returns the the result of the intervals in this set intersected with the given interval.
    */
  def intersect(interval: Interval[T]): IntervalSet[T] =
    IntervalSet(intersecting(interval).toList.flatMap(_ intersect interval): _*)

  override def intersect(
      other: scala.collection.Set[Interval[T]]
  ): IntervalSet[T] =
    other.foldLeft(empty)(_ ++ intersect(_))

  /**
    * Alias for `intersect`.
    */
  def &(interval: Interval[T]): IntervalSet[T] = intersect(interval)

  /**
    * Returns the subset of intervals which union with the given interval.
    */
  def unioning(interval: Interval[T]): IntervalSet[T] = {
    val buf = mutable.ArrayBuffer[Interval[T]]()
    def loop(tree: RB.Tree[Interval[T], Unit]): Unit = {
      if (!RB.isEmpty(tree)) {
        if (tree.key unions interval) buf += tree.key
        if (
          !RB.isEmpty(tree.left) && (RB
            .greatest(tree.left)
            .key
            .upper connects interval.lower)
        )
          loop(tree.left)
        if (
          !RB.isEmpty(tree.right) && (RB
            .smallest(tree.right)
            .key
            .lower connects interval.upper)
        )
          loop(tree.right)
      }
    }
    loop(tree)
    IntervalSet.create(buf.toIndexedSeq)
  }

  def span: Option[Interval[T]] =
    if (!RB.isEmpty(tree)) Some(head span last) else None

  def complement: IntervalSet[T] = IntervalSet(Interval.all[T]) -- this
}
