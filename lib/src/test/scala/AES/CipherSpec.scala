package AES
import munit.*
import dfhdl.*
import tools.linters.iverilog
import dfhdl.options.LinterOptions.VerilogLinter

//TODO: need to fix Cipher verilog compilation errors
class CipherSpec extends util.FullCompileSpec:
  def dut: core.Design = Cipher()
  def expectedVerilogCS: String = ""
  def expectedVHDLCS: String = ""
  // iverilog does not support unpacked array parameters
  // TODO: change when https://github.com/steveicarus/iverilog/issues/846 is fixed
  override def verilogLinters: List[VerilogLinter] = Nil
  // super.verilogLinters.filterNot(_ equals iverilog)

  // test("dropped opaques verilog compilation with no error"):
  //   given options.CompilerOptions.Backend = backends.verilog
  //   given options.CompilerOptions.DropUserOpaques = true
  //   dut.compile.lintVerilog

  test("dropped opaques vhdl compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl.v2008
    given options.CompilerOptions.DropUserOpaques = true
    dut.compile.lintVHDL
end CipherSpec
