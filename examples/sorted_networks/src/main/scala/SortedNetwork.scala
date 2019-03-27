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
  val enable    = DFBool()   <> IN
  val upDown_n  = DFBool()   <> IN
  val cnt       = DFUInt(32) <> OUT init 0
  ifdf (enable) {
    ifdf (upDown_n) {
      cnt := cnt + 1
    }.elsedf {
      cnt := cnt - 1
    }
  }
}

trait SampleFilterAccumulator extends DFDesign {
  val max_stdv  = 1000
  val sample    = DFSInt(16) <> IN
  val acc       = DFSInt(32) <> OUT init 0
  val delta1    = (sample-sample.prev).wc
  val delta2    = (sample-sample.prev(2)).wc
  val usable1   = ((delta1 < max_stdv) && (delta1 > -max_stdv))
  val usable2   = ((delta2 < max_stdv) && (delta2 > -max_stdv))
  ifdf (usable1 && usable2) {
    acc := acc + sample
  }
}

class SlidingAvg(sample : DFSInt[16]) extends DFDesign {
  val avg       = DFSInt(16) <> OUT
  val acc       = DFSInt(18) init 0
  acc := acc + sample - sample.prev(4)
  avg := (acc >> 2).bits(15, 0).sint
}

trait SlidingAvg4x4 extends DFDesign {
  val a    = DFSInt(16) <> IN init 0
  val b    = DFSInt(16) <> IN init 0
  val c    = DFSInt(16) <> IN init 0
  val d    = DFSInt(16) <> IN init 0
  val avg  = DFSInt(16) <> OUT

  def sa(src : DFSInt[16]) : DFSInt[16] = {
    val acc = DFSInt(18) init 0
    acc := acc + src - src.prev(4)
    (acc >> 2).bits(15, 0).sint
  }
  avg := ((sa(a) + sa(b)).wc + (sa(c) + sa(d)).wc).wc.bits(15, 0).sint
}

