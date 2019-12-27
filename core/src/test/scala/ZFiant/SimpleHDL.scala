package ZFiant
import scala.language.postfixOps

trait SimpleHDL extends DFDesign {
  final val max = DFBits(32) <> IN
  final val count = DFBits(32) <> OUT
  final val hold_count = DFBits(32) <> IN

  final val counter = DFUInt(32) init 0
  final val hold_counter = DFUInt(3) init 0
}

object SimpleHDLApp extends App {
  val simple_hdl = new SimpleHDL {}
  import DFCompiler._
  simple_hdl.db.printCodeString()
}