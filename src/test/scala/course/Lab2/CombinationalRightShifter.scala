package course.Lab2

import DFiant._
trait CombinationalRightShifter extends RightShifter {
  private val temp = DFBits(w)
  temp := vec
  for (i <- k-1 to 0 by -1) {
    temp := Mux(w)(shift.bit(i))(temp >> (1 << i), temp)
  }
  res := temp
}

trait CombinationalRightShifterTester extends RightShifterTester { //No need to modify the test
  final lazy val rightShifter = new CombinationalRightShifter {}
}


