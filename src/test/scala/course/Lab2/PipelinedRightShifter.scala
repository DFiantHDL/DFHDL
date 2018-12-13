package course.Lab2
import DFiant._

trait PipelinedRightShifter extends RightShifter {
  private val temp = DFBits(w)
  temp := vec
  for (i <- k-1 to 0 by -1) {
    temp := Mux(w)(shift.bit(i).pipe(k-i-1))(temp >> (1 << i), temp).pipe()
  }
  res := temp
}

trait PipelinedRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new PipelinedRightShifter {}
  override def check(): Unit = super.check() //change this if check should be modified for the test to succeed
}


