package dfhdl.tools
import toolsCore.*

object linters:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val vlog = QuestaSimVerilog
  val ghdl = GHDL
  val nvc = NVC
  val vcom = QuestaSimVHDL

object builders:
  val vivado: Builder = Vivado
