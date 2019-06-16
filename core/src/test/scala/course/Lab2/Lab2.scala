/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package course.Lab2
import sys.process._
import scala.language.postfixOps

object Lab2 extends App {
  val ghdl = "ghdl"
  val vivado = "/opt/Xilinx/Vivado/2018.2/bin/vivado"

  println("Hello world! I'm Lab #2")
  //run this program to generate the proper lab2.vhd
  val msg =
    """
      |Param #1: Type of Right Shifter
      |s = Simple Right Shifter (given) [Default = s]
      |c = Combination Right Shifter (your own)
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
    val result = {s"$ghdl --version" !!}
    !result.startsWith("GHDL")
  }
  def simRTLCompile() : Unit = println(
    s"$ghdl -a --std=08 lab2.vhd" !!
  )
  def simRTLRun(timeNS : Int) : Unit = println(
    s"$ghdl -r --std=08 lab2 --ieee-asserts=disable-at-0 --stop-time=${timeNS}ns" !!
  )
  def synthesize() : Unit = {
    s"$vivado -mode batch -source lab2.tcl -log lab2.log" !!
  }

  val shifterType = if (args.length < 1) "s" else args(0)
  val cmdType = if (args.length < 2) "t" else args(1)
  val width = if (args.length < 3) "32" else args(2)

  RightShifter.requestedWidth = if (cmdType == "t") 32 else width.toInt

  val lab2 = (shifterType, cmdType) match {
    case ("s", "s") => new SimpleRightShifter {}
    case ("c", "s") => new CombinationalRightShifter {}
    case ("p", "s") => new PipelinedRightShifter {}
    case ("s", "c") => new SimpleRightShifter {}
    case ("c", "c") => new CombinationalRightShifter {}
    case ("p", "c") => new PipelinedRightShifter {}
    case ("s", "t") => new SimpleRightShifterTester {}
    case ("c", "t") => new CombinationalRightShifterTester {}
    case ("p", "t") => new PipelinedRightShifterTester {}
    case _ => error()
  }

  cmdType match {
    case "c" => lab2.printCodeString
    case "t" =>
      lab2.compileToVHDL.print().toFile("lab2.vhd")
      if (ghdlTestFail())
        println("Test run of GHDL did not succeed :(\nMake sure it is available in your path for automatic simulation run.")
      else {
        println("Attempting to compile and run simulation RTL files...")
        simRTLCompile()
        simRTLRun(500)
      }
    case "s" =>
      lab2.compileToVHDL.print().toFile("lab2.vhd")
      println("Attempting to synthesize the RTL files...")
      synthesize()
      println("Synthesis done!\nSee lab2.log, lab2_xxx.rpt files")
  }
}

