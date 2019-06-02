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
      RTOp2.+(o, i, i)
//      val rt = new FoldRTx2(8)
//      rt.I <> i
//      rt.O <> o
    }
  }
}

trait FoldTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val io = new FoldComp(o) {}
//  io.i <> 0
  i <> io.i
  o <> io.o
}

object FoldApp extends App {
  trait BBB {
    type Func[A <: DFAny]
  }
  trait IN extends BBB {
    type Func[A <: DFAny] = A#In
  }
  trait OUT extends BBB {
    type Func[A <: DFAny] = A#Out
  }
  type <~>[DF <: DFAny, Dir <: BBB] = Dir#Func[DF]

//  val a : DFiant.DFUInt[8] <> DFiant.IN = null.asInstanceOf[DFiant.DFAny.Port[DFiant.DFUInt[8],DFiant.IN.type] with DFiant.DFUInt[8]]
  val foldtest = new FoldTest {}
  println(foldtest.io.externals.named)
  println("------------------------------")
  foldtest.io.unfold
  foldtest.printCodeString
  println("------------------------------")
  foldtest.io.fold
  foldtest.printCodeString
}
