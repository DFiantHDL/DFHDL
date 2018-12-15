package course.Lab2
import DFiant._

trait SimpleRightShifter extends RightShifter {
  res := vec >> shift
}

trait SimpleRightShifterTester extends RightShifterTester {
  lazy val rightShifter : RightShifter = new SimpleRightShifter {}

  //Cyclic rotation through the test cases
  final val vec = DFBits(w) init vecSeq
  final val shift = DFUInt.rangeUntil(w) init shiftSeq
  final val expected = DFBits(w) init expectedSeq

  vec := vec.prev(testNum)
  shift := shift.prev(testNum)
  expected := expected.prev(testNum)

  rightShifter.vec <> vec
  rightShifter.shift <> shift

  sim.assert(rightShifter.res == expected, msg"expected $vec >> $shift = $expected, but got ${rightShifter.res}")
}


