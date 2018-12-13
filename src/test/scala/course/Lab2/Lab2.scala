package course.Lab2

object Lab2 extends App {
  println("Hello world! I'm Lab #2")
  //run this program to generate the proper lab2.vhd
  val msg =
    """
      |Param #1: Type of Right Shifter
      |s = Simple Right Shifter (given) [Default = s]
      |c = Combination Right Shifter (your own)
      |m = Multi-cycle Right Shifter (your own)
      |p = Pipelined Right Shifter (your own)
      |
      |Param #2: Test, or Synthesis, or DFiant Code printout
      |t = Test (only works with a 32-bit width shifter) [Default = t]
      |s = Synthesis
      |c = Code printout
      |
      |Param #3: Width of the shifter [Default = 32]
    """.stripMargin


  def error() = throw new IllegalArgumentException("Error in program arguments\n" + msg)

  val shifterType = if (args.length < 1) "s" else args(0)
  val cmdType = if (args.length < 2) "t" else args(1)
  val width = if (args.length < 3) "32" else args(2)

  RightShifter.requestedWidth = if (cmdType == "t") 32 else width.toInt

  val top = (shifterType, cmdType) match {
    case ("s", "s") => new SimpleRightShifter {}.setName("rightShifter")
    case ("c", "s") => new CombinationalRightShifter {}.setName("rightShifter")
    case ("m", "s") => new MulticycleRightShifter {}.setName("rightShifter")
    case ("p", "s") => new PipelinedRightShifter {}.setName("rightShifter")
    case ("s", "c") => new SimpleRightShifter {}.setName("rightShifter")
    case ("c", "c") => new CombinationalRightShifter {}.setName("rightShifter")
    case ("m", "c") => new MulticycleRightShifter {}.setName("rightShifter")
    case ("p", "c") => new PipelinedRightShifter {}.setName("rightShifter")
    case ("s", "t") => new SimpleRightShifterTester {}.setName("rightShifter_tb")
    case ("c", "t") => new CombinationalRightShifterTester {}.setName("rightShifter_tb")
    case ("m", "t") => new MulticycleRightShifterTester {}.setName("rightShifter_tb")
    case ("p", "t") => new PipelinedRightShifterTester {}.setName("rightShifter_tb")
    case _ => error()
  }
  if (cmdType == "c") top.printCodeString
  else top.compileToVHDL.print().toFile("lab2.vhd")
}

