package AES
import munit.*
import dfhdl.*
import tools.linters.iverilog
import dfhdl.options.LinterOptions.VerilogLinter

class CipherSpecWithOpaques extends util.FullCompileSpec:
  def dut: core.Design = Cipher()
  // TODO: need to fix Cipher verilog compilation errors
  override def verilogLinters: List[VerilogLinter] = Nil
end CipherSpecWithOpaques

class CipherSpecNoOpaques extends CipherSpecWithOpaques:
  // force a different top folder name to run tests concurrently with Cipher()
  @top(false) class CipherNoOpaques extends Cipher
  override def dut: core.Design = CipherNoOpaques()
  given options.CompilerOptions.DropUserOpaques = true
  // iverilog does not support unpacked array parameters
  // TODO: change when https://github.com/steveicarus/iverilog/issues/846 is fixed
  override def verilogLinters: List[VerilogLinter] =
    super.verilogLinters.filterNot(_ equals iverilog)
end CipherSpecNoOpaques
