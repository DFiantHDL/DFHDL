package DFiant
package lib.maxeler
import compiler.sync._
import scala.language.postfixOps

object ModeType extends EnumType.Auto {
  val COUNTING_UP, COUNTING_DOWN, HOLD = Entry()
}
@df class ScalarHDL extends DFDesign {
  import ModeType._
  final val max = DFBits(32) <> IN
  final val count = DFBits(32) <> OUT init b0s
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

  count := counter.prev.bits
}

object ScalarHDLApp extends App {
  val scalar_hdl = new ScalarHDL {
    max !! Maxeler.StreamIOPull
    count !! Maxeler.StreamIOPush
    hold_count !! Maxeler.ScalarIO
    this !! ClockParams("clk", ClockParams.Edge.Rising)
    this !! ResetParams("rst", ResetParams.Mode.Async, ResetParams.Active.High)
  }
  scalar_hdl.compile.printCodeString().printGenFiles()
}