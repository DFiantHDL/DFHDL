import DFiant._

trait SortedNetwork extends DFDesign {
  val size : Int
  val width : Int
  final val ingress = List.tabulate(size)(i => DFSInt(width) <> IN  setName(s"ingress$i"))
  final val egress  = List.tabulate(size)(i => DFSInt(width) <> OUT setName(s"egress$i"))
  implicit class CaS(tuple : (DFSInt[Int], DFSInt[Int])) {
    private def maxWidth : Int = math.max(tuple._1.width, tuple._2.width)
    def sortdf() : (DFSInt[Int], DFSInt[Int]) = {
      val swap = tuple._1 > tuple._2
      Tuple2(
        DFSInt(maxWidth).selectdf(swap)(tuple._2, tuple._1),
        DFSInt(maxWidth).selectdf(swap)(tuple._1, tuple._2)
      )
    }
  }
}

//A basic compare-and-swap sort network
trait CaS extends SortedNetwork {
  lazy val size : Int = 2
  ifdf(ingress(0) <= ingress(1)) {
    egress(0) := ingress(0)
    egress(1) := ingress(1)
  }.elsedf {
    egress(0) := ingress(1)
    egress(1) := ingress(0)
  }
}
object CaS {
//  def apply[W0, W1](ingress0 : DFSInt[W0], ingress1 : DFSInt[W1])(implicit ctx : DFAny.Op.Context)
//  : ()
}

object SortedNetwork {
//  def sort[W](tuple2 : Tuple2[DFSInt[W], DFSInt[W]])(implicit ctx : DFAny.Op.Context) : Tuple2[DFSInt[W], DFSInt[W]] = {
//  }
}