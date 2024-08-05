package AES
import munit.*
import dfhdl.*

class CipherSpec extends FunSuite:
  def dut: core.Design = Cipher()
  given options.OnError = options.OnError.Exception
  test("verilog compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog
    val cs = dut.compile.lint

  test("vhdl compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl
    val cs = dut.compile.lint

  test("dropped opaques verilog compilation with no error"):
    given options.CompilerOptions.Backend = backends.verilog
    given options.CompilerOptions.DropUserOpaques = true
    val cs = dut.compile.lint

  test("dropped opaques vhdl compilation with no error"):
    given options.CompilerOptions.Backend = backends.vhdl
    given options.CompilerOptions.DropUserOpaques = true
    val cs = dut.compile.lint
end CipherSpec
