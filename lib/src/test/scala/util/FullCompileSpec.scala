package util
import munit.*

import dfhdl.*
import dfhdl.compiler.stages.getCompiledCodeString

abstract class FullCompileSpec extends FunSuite:
  def dut: core.Design
  def expectedVerilogCS: String
  def expectedVHDLCS: String
  given options.OnError = options.OnError.Exception
  test("verilog[default = sv2009] compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog
    val cs = dut.compile.lint.getCompiledCodeString
    assertNoDiff(cs, expectedVerilogCS)

  test("verilog.v2001 compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog.v2001
    dut.compile.lint

  test("verilog.v95 compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog.v95
    dut.compile.lint

  test("vhdl[default = v2008] compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl
    val compiled = dut.compile
    locally:
      given options.LinterOptions.VHDLLinter = tools.linters.ghdl
      compiled.lint
    locally:
      given options.LinterOptions.VHDLLinter = tools.linters.nvc
      compiled.lint
    assertNoDiff(compiled.getCompiledCodeString, expectedVHDLCS)
    val cs = dut.compile.lint.getCompiledCodeString
    assertNoDiff(cs, expectedVHDLCS)
end FullCompileSpec
