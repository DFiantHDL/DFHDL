import DFiant._

trait SortedNetwork extends DFDesign {
  val size : Int
  val width : Int

  final val ingress = List.tabulate(size)(i => DFSInt(width) <> IN  setName(s"ingress$i"))
  final val egress  = List.tabulate(size)(i => DFSInt(width) <> OUT setName(s"egress$i"))
  implicit class TupleCaS(tuple : (DFSInt[Int], DFSInt[Int])) {
    private def maxWidth : Int = math.max(tuple._1.width, tuple._2.width)
    def sortdf(lowToHigh : Boolean = true) : (DFSInt[Int], DFSInt[Int]) = {
      val swap = if (lowToHigh) tuple._1 > tuple._2 else tuple._2 > tuple._1
      Tuple2(
        DFSInt(maxWidth).selectdf(swap)(tuple._2, tuple._1),
        DFSInt(maxWidth).selectdf(swap)(tuple._1, tuple._2)
      )
    }
  }
}

trait BitonicSortNetwork extends SortedNetwork {
  implicit class ListSort(list : List[DFSInt[Int]]) {
    private val dist = list.length / 2
    def sortdf(lowToHigh : Boolean) : List[DFSInt[Int]] = {
      if (list.length <= 1) list
      else {
        val split = list.splitAt(dist)
        val sorted = split._1.sortdf(lowToHigh) ++ split._2.sortdf(!lowToHigh)
        sorted.merge(lowToHigh)
      }
    }
    private[ListSort] def merge(lowToHigh : Boolean) : List[DFSInt[Int]] = {
      if (list.length <= 1) list
      else {
        val swapped = list.swap(lowToHigh)
        val split = swapped.splitAt(dist)
        split._1.merge(lowToHigh) ++ split._2.merge(!lowToHigh)
      }
    }
    private[ListSort] def swap(lowToHigh : Boolean) : List[DFSInt[Int]] = {
      val split = list.splitAt(dist)
      val swapped = for (i <- 0 until dist)
        yield (list(i), list(i + dist)).sortdf(lowToHigh)
      swapped.toList.flatMap(e => List(e._1, e._2))
    }
  }
}

trait UDCounter extends DFDesign {
  val enable    = DFBool() <> IN
  val upDown_n  = DFBool() <> IN
  val cnt       = DFUInt(32) <> OUT init 0
  ifdf (enable) {
    ifdf (upDown_n) {
      cnt := cnt + 1
    }.elsedf {
      cnt := cnt - 1
    }
  }
}

