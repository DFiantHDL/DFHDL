package course.Lab2
import DFiant._

trait MulticycleRightShifter extends RightShifter {
  final val valid = DFBool() <> OUT init false
  private val temp = DFBits(w)
  private val step = DFUInt(k) init 0
  private val done = step == 0
  res := Mux(w)(done)(vec, res.prev >> 1)
  step := Mux(k)(done)(shift.bits, (step - 1).bits).uint
  valid := done
}

trait MulticycleRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new MulticycleRightShifter {}
  override def check(): Unit = {
  }
}


