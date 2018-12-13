package course.Lab2

trait MulticycleRightShifter extends RightShifter {
}

trait MulticycleRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new MulticycleRightShifter {}
  override def check(): Unit = super.check() //change this if check should be modified for the test to succeed
}


