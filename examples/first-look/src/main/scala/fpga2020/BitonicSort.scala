package fpga2020

import DFiant._

object BitonicSort {
  implicit class BitonicSortImpl(list : List[DFSInt[8]])(implicit ctx : DFBlock.Context) {
    private val dist = list.length / 2
    def sortdf(lowToHigh : Boolean) : List[DFSInt[8]] =
      if (list.length <= 1) list
      else {
        val split = list.splitAt(dist)
        val sorted = split._1.sortdf(lowToHigh) ++ split._2.sortdf(!lowToHigh)
        sorted.merge(lowToHigh)
      }
    private[BitonicSort] def merge(lowToHigh : Boolean) : List[DFSInt[8]] =
      if (list.length <= 1) list
      else {
        val swapped = list.cas(lowToHigh)
        val split = swapped.splitAt(dist)
        split._1.merge(lowToHigh) ++ split._2.merge(!lowToHigh)
      }
    private[BitonicSort] def cas(lowToHigh : Boolean) : List[DFSInt[8]] =
      list.splitAt(dist).zipped.toList.flatMap(e => cas(lowToHigh, e))
    private def cas(lowToHigh : Boolean, tuple : (DFSInt[8], DFSInt[8])) : List[DFSInt[8]] = {
      val swap = if (lowToHigh) tuple._1 > tuple._2 else tuple._2 > tuple._1
      val sel1 = DFSInt[8].ifdf(swap){tuple._2}.elsedf{tuple._1}
      val sel2 = DFSInt[8].ifdf(swap){tuple._1}.elsedf{tuple._2}
      List(sel1, sel2)
    }
  }

}

trait BitSort extends DFDesign {
  import BitonicSort._

  val inList : List[DFSInt[8]] = (for (i <- 0 until 16) yield DFSInt[8] <> IN setName(s"i$i") pipe()).toList
  val outList = (for (i <- 0 until 16) yield DFSInt[8] <> OUT setName(s"o$i")).toList
  val res = inList.sortdf(true)

  for (i <- 0 until 16) outList(i) := res(i).setName(s"res$i").pipe()
}

object BitSortApp extends DFApp.VHDLCompiler[BitSort]