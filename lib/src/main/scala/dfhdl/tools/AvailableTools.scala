package dfhdl.tools
import toolsCore.*

object linters:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val ghdl = GHDL
  val nvc = NVC

object builders:
  val vivado: Builder = Vivado
