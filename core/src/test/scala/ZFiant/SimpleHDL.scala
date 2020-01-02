package ZFiant
import scala.language.postfixOps

object ModeType extends Enum.Auto {
  val COUNTING_UP, COUNTING_DOWN, HOLD = Entry()
}
trait SimpleHDL extends DFDesign {
  import ModeType._
  final val max = DFBits(32) <> IN
  final val count = DFBits(32) <> OUT
  final val hold_count = DFBits(32) <> IN

  final val counter = DFUInt(32) init 0
  final val hold_counter = DFUInt(32) init 0
  final val mode = DFEnum(ModeType) init COUNTING_UP
  matchdf(mode)
    .casedf(COUNTING_UP) {
      ifdf(counter >= max.uint){
        counter := max.uint
        mode := HOLD
      }.elsedf {
        counter := counter + 1
      }
    }
    .casedf(COUNTING_DOWN) {
      ifdf(counter === 0) {
        counter := counter + 1
        mode := COUNTING_UP
      }.elsedf {
        counter := counter - 1
      }
    }
    .casedf(HOLD) {
      ifdf (hold_counter === hold_count.uint) {
        hold_counter := 0
        mode := COUNTING_DOWN
      }.elsedf {
        hold_counter := hold_counter + 1
      }
    }

  count := counter.bits
}

object SimpleHDLApp extends App {
  val simple_hdl = new SimpleHDL {}
  import DFCompiler._
  val hack = new DFDesign() {
    val x = DFUInt(8) init 0
    x := x + 1
  }
  simple_hdl.db.patch(List(simple_hdl.mode -> DFDesign.DB.Patch.AddBefore(hack.db.fixNames))).printCodeString()
}