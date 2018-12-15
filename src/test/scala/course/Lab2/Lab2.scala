package course.Lab2
import sys.process._
import scala.language.postfixOps

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
  def ghdlTestFail() : Boolean = {
    val result = {"ghdl --version" !!}
    !result.startsWith("GHDL")
  }
  def simRTLCompile() : Unit = println(
    "ghdl -a --std=08 lab2.vhd" !!
  )
  def simRTLRun(timeNS : Int) : Unit = println(
    s"ghdl -r --std=08 top --ieee-asserts=disable-at-0 --stop-time=${timeNS}ns" !!
  )

  val shifterType = if (args.length < 1) "s" else args(0)
  val cmdType = if (args.length < 2) "t" else args(1)
  val width = if (args.length < 3) "32" else args(2)

  RightShifter.requestedWidth = if (cmdType == "t") 32 else width.toInt

  val top = (shifterType, cmdType) match {
    case ("s", "s") => new SimpleRightShifter {}
    case ("c", "s") => new CombinationalRightShifter {}
    case ("m", "s") => new MulticycleRightShifter {}
    case ("p", "s") => new PipelinedRightShifter {}
    case ("s", "c") => new SimpleRightShifter {}
    case ("c", "c") => new CombinationalRightShifter {}
    case ("m", "c") => new MulticycleRightShifter {}
    case ("p", "c") => new PipelinedRightShifter {}
    case ("s", "t") => new SimpleRightShifterTester {}
    case ("c", "t") => new CombinationalRightShifterTester {}
    case ("m", "t") => new MulticycleRightShifterTester {}
    case ("p", "t") => new PipelinedRightShifterTester {}
    case _ => error()
  }


  if (cmdType == "c") top.printCodeString
  else top.compileToVHDL.print().toFile("lab2.vhd")
  if (cmdType == "t") {
    if (ghdlTestFail)
      println("Test run of GHDL did not succeed :(\nMake sure it is available in your path for automatic simulation run.")
    else {
      println("Attempting to compile and run simulation RTL files...")
      simRTLCompile()
      simRTLRun(500)
    }
  }
}

