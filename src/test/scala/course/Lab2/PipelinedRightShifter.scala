package course.Lab2

trait PipelinedRightShifter extends RightShifter {

}

trait PipelinedRightShifterTester extends RightShifterTester {
  final lazy val rightShifter = new PipelinedRightShifter {}
  override def check(): Unit = super.check() //change this if check should be modified for the test to succeed
}


