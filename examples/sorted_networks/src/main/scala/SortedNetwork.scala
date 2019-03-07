import DFiant._
import singleton.ops._
import singleton.twoface._

trait SortedNetwork[W] extends DFDesign {
  val size : Int
  val width : TwoFace.Int[W]
  final val ingress = List.tabulate(size)(i => DFSInt(width) <> IN  setName(s"ingress$i"))
  final val egress  = List.tabulate(size)(i => DFSInt(width) <> OUT setName(s"egress$i"))
}

//A basic compare-and-swap sort network
trait CaS[W] extends SortedNetwork[W] {
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
//  def apply[W0, W1](ingress0 : DFSInt[W0], ingress1 : DFSInt[W1])(
//    implicit
//    ctx : DFAny.Op.Context,
//    w : singleton.ops.math.Max[W0, W1]
}

object SortedNetwork {
//  def sort[W](tuple2 : Tuple2[DFSInt[W], DFSInt[W]])(implicit ctx : DFAny.Op.Context) : Tuple2[DFSInt[W], DFSInt[W]] = {
//  }
}