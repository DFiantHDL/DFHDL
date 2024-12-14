package util
import munit.*

import dfhdl.*
import dfhdl.compiler.stages.{getCompiledCodeString, CompiledDesign}
import dfhdl.options.{CompilerOptions, LinterOptions}
import tools.linters.*

abstract class FullCompileSpec extends FunSuite:
  def dut: core.Design
  def expectedVerilogCS: String
  def expectedVHDLCS: String
  given options.OnError = options.OnError.Exception
  given options.LinterOptions.FatalWarnings = true
  def verilogLinters: List[LinterOptions.VerilogLinter] =
    List(verilator, iverilog, vlog, xvlog)
  def vhdlLinters: List[LinterOptions.VHDLLinter] =
    List(ghdl, nvc, vcom, xvhdl)
  extension [D <: core.Design](cd: CompiledDesign[D])
    def lintVerilog(using CompilerOptions): CompiledDesign[D] =
      verilogLinters.foreach { linter =>
        if (linter.isAvailable)
          given LinterOptions.VerilogLinter = linter
          cd.lint
      }
      cd
    def lintVHDL(using CompilerOptions): CompiledDesign[D] =
      vhdlLinters.foreach { linter =>
        if (linter.isAvailable)
          given LinterOptions.VHDLLinter = linter
          cd.lint
      }
      cd
  end extension

  test("verilog[default = sv2009] compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog
    val compiled = dut.compile.lintVerilog
    if (expectedVerilogCS.nonEmpty)
      assertNoDiff(compiled.getCompiledCodeString, expectedVerilogCS)

  test("verilog.v2001 compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog.v2001
    dut.compile.lintVerilog

  test("verilog.v95 compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog.v95
    dut.compile.lintVerilog

  test("vhdl[default = v2008] compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl
    val compiled = dut.compile.lintVerilog
    if (expectedVHDLCS.nonEmpty)
      assertNoDiff(compiled.getCompiledCodeString, expectedVHDLCS)

  test("vhdl.v93 compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    dut.compile.lintVHDL
end FullCompileSpec
