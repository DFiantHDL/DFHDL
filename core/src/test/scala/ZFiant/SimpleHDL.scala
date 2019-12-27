package ZFiant
import scala.language.postfixOps

object ModeType extends Enum.Auto {
  val COUNTING_UP, COUNTING_DOWN, HOLD = Entry()
}
trait SimpleHDL extends DFDesign {
  final val max = DFBits(32) <> IN
  final val count = DFBits(32) <> OUT
  final val hold_count = DFBits(32) <> IN

  final val counter = DFUInt(32) init 0 keep
  final val hold_counter = DFUInt(3) init 0
  final val mode = DFEnum(ModeType) init ModeType.COUNTING_UP keep
}

object SimpleHDLApp extends App {
  val simple_hdl = new SimpleHDL {}
  import DFCompiler._
  simple_hdl.db.printCodeString()
}