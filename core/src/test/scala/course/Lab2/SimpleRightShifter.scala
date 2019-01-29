package course.Lab2
import DFiant._

trait SimpleRightShifter extends RightShifter {
  res := vec >> shift
}

trait SimpleRightShifterTester extends RightShifterTester {
  lazy val rightShifter : RightShifter = new SimpleRightShifter {}

}


