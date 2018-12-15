package course.Lab2
import DFiant._

trait SimpleRightShifter extends RightShifter {
  res := vec >> shift
}

trait SimpleRightShifterTester extends RightShifterTester {
  lazy val rightShifter : RightShifter = new SimpleRightShifter {}

  rightShifter.vec <> vec
  rightShifter.shift <> shift

  sim.assert(rightShifter.res == expected, msg"expected $vec >> $shift = $expected, but got ${rightShifter.res}")
}


