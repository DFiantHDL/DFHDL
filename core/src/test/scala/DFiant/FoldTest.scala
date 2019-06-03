import DFiant._

class FoldRTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
//  setInitFunc(O)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(I), getInit(I)))
}

abstract class FoldComp(val ii : DFUInt[8] <> IN)(implicit ctx : DFComponent.Context[FoldComp]) extends DFComponent[FoldComp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  final protected val foldedDiscoveryDependencyList = (o -> (i :: Nil)) :: Nil
}
object FoldComp {
  implicit val ev : FoldComp => Unit = ifc => {
    import ifc._
    if (i.isConstant) o := 0
    else {
      RTOp2.+(o, i, ii)
//      val rt = new FoldRTx2(8)
//      rt.I <> i
//      rt.O <> o
    }
  }
}

trait FoldTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val io = new FoldComp(i) {}
//  io.i <> 0
  i <> io.i
  o <> io.o
}

class Trans(val ti : DFUInt[8] <> IN, val to : DFUInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[Trans]) extends DFDesign {
//  import internals._
//  println(this.transparentPorts)
  to := ti
}

object FoldApp extends App {
  val foldtest = new FoldTest {}
//  println(foldtest.io.externals.named)
  println("------------------------------")
  foldtest.io.unfold
  foldtest.printCodeString
  println("------------------------------")
  foldtest.io.fold
  foldtest.printCodeString
}


trait TransTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val io = new Trans(i, o) {}
}

object TransApp extends App {
  val transTest = new TransTest {}.printCodeString
}
