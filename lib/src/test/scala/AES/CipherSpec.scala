package AES
import munit.*
import dfhdl.*
import tools.linters.iverilog
import dfhdl.options.LinterOptions.VerilogLinter
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.verilog.VerilogDialect

class CipherSpecWithOpaques extends util.FullCompileSpec:
  def dut: core.Design = Cipher()
  // iverilog does not support unpacked array parameters
  // TODO: change when https://github.com/steveicarus/iverilog/issues/846 is fixed
  // In v95 and v2001, the unpacked array parameters are removed, so we can use iverilog
  override def verilogLinters(using co: CompilerOptions): List[VerilogLinter] =
    val dialect = co.backend.asInstanceOf[backends.verilog].dialect
    if (dialect == VerilogDialect.v95 || dialect == VerilogDialect.v2001)
      super.verilogLinters
    else
      super.verilogLinters.filterNot(_ equals iverilog)
end CipherSpecWithOpaques

class CipherSpecNoOpaques extends CipherSpecWithOpaques:
  // force a different top folder name to run tests concurrently with Cipher()
  @top(false) class CipherNoOpaques extends Cipher
  override def dut: core.Design = CipherNoOpaques()
  given options.CompilerOptions.DropUserOpaques = true
end CipherSpecNoOpaques
