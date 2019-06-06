import DFiant._

class PortArgDesign(val ti : DFUInt[8] <> IN, val to : DFUInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesign]) extends DFDesign {
  val padInt = new PortArgDesignInt(ti, to)
}

class PortArgDesignInt(val ti : DFUInt[8] <> IN, val to : DFUInt[8] <> OUT)(implicit ctx : DFDesign.ContextOf[PortArgDesignInt]) extends DFDesign {
  to <> ti + ti//.bits.uint
}

trait PortArgTest extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT

  val temp = DFUInt(8)
  val io1 = new PortArgDesign(i, temp) {}
  val io2 = new PortArgDesign(temp, o) {}
}

object PortArgApp extends App {
  val paTest = new PortArgTest {}.printCodeString
}
