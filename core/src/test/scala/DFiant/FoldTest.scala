import DFiant._

import internals._

class FoldRTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
//  setInitFunc(O)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(I), getInit(I)))
}

trait FoldComp extends DFComponent[FoldComp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  final protected val foldedDiscoveryDependencyList = (o -> (i :: Nil)) :: Nil
}
object FoldComp {
  implicit val ev : FoldComp => Unit = ifc => {
    import ifc._
    if (i.isConstant) o := 0
    else {
      val rt = new FoldRTx2(8)
      rt.I <> i
      rt.O <> o
    }
  }
}

trait FoldTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val io = new FoldComp {}
  io.i <> 0
//  i <> io.i
  o <> io.o
}

object FoldApp extends App {
  val foldtest = new FoldTest {}.printVHDLString
  println("------------------------------")
  foldtest.io.unfold
  foldtest.printCodeString
  println("------------------------------")
  foldtest.io.fold
  foldtest.printCodeString
}
