package course.Lab2
import DFiant._

trait MulticycleRightShifter extends RightShifter {
  private val temp = DFBits(w)
  temp := vec
  for (i <- k-1 to 0 by -1) {
    temp := Mux(w)(shift.bit(i))(temp >> (1 << i), temp)
  }
  res := temp
}

trait MulticycleRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new MulticycleRightShifter {}
  override def check(): Unit = super.check() //change this if check should be modified for the test to succeed
}


