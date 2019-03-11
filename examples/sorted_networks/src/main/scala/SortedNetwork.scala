import DFiant._
import singleton.ops._

object bitonic_sort {
  implicit class BitonicSort(list : List[DFSInt[Int]])(implicit ctx : DFBlock.Context) {
    private val dist = list.length / 2
    private val maxWidth : Int = list.map(e => e.width.getValue).max
    def sortdf(lowToHigh : Boolean) : List[DFSInt[Int]] =
      if (list.length <= 1) list
      else {
        val split = list.splitAt(dist)
        val sorted = split._1.sortdf(lowToHigh) ++ split._2.sortdf(!lowToHigh)
        sorted.merge(lowToHigh)
      }
    private[BitonicSort] def merge(lowToHigh : Boolean) : List[DFSInt[Int]] =
      if (list.length <= 1) list
      else {
        val swapped = list.cas(lowToHigh)
        val split = swapped.splitAt(dist)
        split._1.merge(lowToHigh) ++ split._2.merge(!lowToHigh)
      }
    private[BitonicSort] def cas(lowToHigh : Boolean) : List[DFSInt[Int]] =
      list.splitAt(dist).zipped.toList.flatMap(e => cas(lowToHigh, e))
    private def cas(lowToHigh : Boolean, tuple : (DFSInt[Int], DFSInt[Int])) : List[DFSInt[Int]] = {
      val swap = if (lowToHigh) tuple._1 > tuple._2 else tuple._2 > tuple._1
      List(
        DFSInt(maxWidth).selectdf(swap)(tuple._2, tuple._1),
        DFSInt(maxWidth).selectdf(swap)(tuple._1, tuple._2)
      )
    }
  }
}

trait UDCounter extends DFDesign {
  val enable    = DFBool() <> IN
  val upDown_n  = DFBool() <> IN
  val cnt       = DFUInt(32) <> OUT   init 0
  ifdf (enable) {
    ifdf (upDown_n) {
      cnt := cnt + 1
    }.elsedf {
      cnt := cnt - 1
    }
  }
}

