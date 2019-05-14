package DFiant

import internals._

class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
  setInitFunc(O)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(I), getInit(I)))
}

trait Comp extends DFComponent[Comp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  final protected val foldedDiscoveryDependencyList = (o -> (i :: Nil)) :: Nil
}
object Comp {
  implicit val ev : Comp => Unit = ifc => {
    import ifc._
    val rt = new RTx2(8)
    rt.I <> i
    rt.O <> o
  }
}

trait FoldTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val io = new Comp {}
  i <> io.i
  o <> io.o
}

object FoldApp extends App {
//  implicit val cfg = DFAnyConfiguration.unfolded
  val foldtest = new FoldTest {}.printCodeString//.compileToVHDL.print().toFile("tour.vhd")
  println("------------------------------")
  foldtest.io.unfold
  foldtest.printCodeString
  println("------------------------------")
  foldtest.io.fold
  foldtest.printCodeString
}
