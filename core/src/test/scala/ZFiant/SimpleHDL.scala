package ZFiant
import scala.language.postfixOps

object ModeType extends Enum.Auto {
  val COUNTING_UP, COUNTING_DOWN, HOLD = Entry()
}
trait SimpleHDL extends DFDesign {
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

  count := counter.bits
}

object SimpleHDLApp extends App {
  val simple_hdl = new SimpleHDL {}
  import DFCompiler._
  import DFDesign.DB.Patch
  implicit class MaxelerExtras(designDB : DFDesign.DB) {
    def maxelerStreamIn(port : DFAny.PortInOf[_ <: DFAny.Type]) : DFDesign.DB = {
      val extras = new DFDesign() {
        val empty = DFBool() <> IN setNamePrefix(s"${port.name}_")
        val almost_empty = DFBool() <> IN setNamePrefix(s"${port.name}_")
        val read = DFBool() <> OUT setNamePrefix(s"${port.name}_")
      }
      import designDB.getset
      designDB.patch(List(port -> DFDesign.DB.Patch.Add(extras.db, before = false))).patch(List(port -> Patch.Replace(port.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
    }
    def maxelerStreamOut(port : DFAny.PortOutOf[_ <: DFAny.Type]) : DFDesign.DB = {
      val extras = new DFDesign() {
        val stall = DFBool() <> IN setNamePrefix(s"${port.name}_")
        val valid = DFBool() <> OUT init 0 setNamePrefix(s"${port.name}_")
      }
      import designDB.getset
      designDB.patch(List(port -> DFDesign.DB.Patch.Add(extras.db, before = false))).patch(List(port -> Patch.Replace(port.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
    }
    def maxelerScalarIn(port : DFAny.PortInOf[_ <: DFAny.Type]) : DFDesign.DB = {
      val extras = new DFDesign() {
        val reg = port.prev() setNamePrefix(s"${port.name}_")
      }
      import designDB.getset
      designDB.patch(List(port -> Patch.Replace(extras.reg, Patch.Replace.Config.ChangeRefOnly))).patch(List(port -> DFDesign.DB.Patch.Add(extras.db, before = false)))
    }

  }
  simple_hdl.db.maxelerStreamIn(simple_hdl.max).maxelerStreamOut(simple_hdl.count).maxelerScalarIn(simple_hdl.hold_count).printCodeString()
}