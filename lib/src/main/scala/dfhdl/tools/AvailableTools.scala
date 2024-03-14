package dfhdl.tools
import toolsCore.*

object linters:
  val verilator: VerilogLinter = Verilator
  val ghdl: VHDLLinter = GHDL

object builders:
  val vivado: Builder = Vivado
