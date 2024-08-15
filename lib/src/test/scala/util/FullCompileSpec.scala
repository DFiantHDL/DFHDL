package util
import munit.*

import dfhdl.*
import dfhdl.compiler.stages.getCompiledCodeString

abstract class FullCompileSpec extends FunSuite:
  def dut: core.Design
  def expectedVerilogCS: String
  def expectedVHDLCS: String
  given options.OnError = options.OnError.Exception
  test("verilog compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog
    val cs = dut.compile.lint.getCompiledCodeString
    assertNoDiff(cs, expectedVerilogCS)

  test("vhdl compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl
    val cs = dut.compile.lint.getCompiledCodeString
    assertNoDiff(cs, expectedVHDLCS)
end FullCompileSpec
