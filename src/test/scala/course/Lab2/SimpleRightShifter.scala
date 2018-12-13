package course.Lab2

trait SimpleRightShifter extends RightShifter {
  res := vec >> shift
}

trait SimpleRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new SimpleRightShifter {}
}


