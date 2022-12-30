import dfhdl.*
import dfhdl.compiler.stages.verilog.*
import scala.sys.process.*

class Example extends EDDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x

@main def hello: Unit =
  System.setProperty("user.dir", "c:\\Users\\oronpo\\IdeaProjects\\dfhdl")
  val top = new Example
  top.commitVerilogCode()
  val output = Process("verilator_bin --lint-only -Wall ./../../sandbox/Example.v").!
  println("done")
