package course.Lab2
import DFiant._

trait MulticycleRightShifter extends RightShifter {
  private val step = DFUInt(k) init 0
  private val shifted_vec = DFBits(w)
  private val done = step == 0
  shifted_vec := Mux(w)(done)(vec, shifted_vec.prev >> 1)
  step := Mux(k)(done)(shift.bits, (step - 1).bits).uint
  res := shifted_vec
}

trait MulticycleRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new MulticycleRightShifter {}

//  private val step = DFUInt(k) init 0
//  ifdf(step == 0) {
//    vec := vec.prev(testNum)
//    shift := shift.prev(testNum)
//    sim.report(msg"Entered put in new pair $vec, $shift")
//    step := shift
//  } elsedf {
//    step := step - 1
//    ifdf (step == 0) {
//      expected := expected.prev(testNum)
//      sim.assert(rightShifter.res == expected, msg"expected $vec >> $shift = $expected, but got ${rightShifter.res}")
//    }
//  }
//
//  rightShifter.vec <> vec
//  rightShifter.shift <> shift
}


