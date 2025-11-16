package dfhdl.tools
import toolsCore.*

object linters:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val vlog = QuestaSimVerilog
  val xvlog = VivadoSimVerilog
  val ghdl = GHDL
  val nvc = NVC
  val vcom = QuestaSimVHDL
  val xvhdl = VivadoSimVHDL
  object questa
  final val vsim = questa
  object vivado
  final val xsim = vivado

object simulators:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val vlog = QuestaSimVerilog
  val xvlog = VivadoSimVerilog
  val ghdl = GHDL
  val nvc = NVC
  val vcom = QuestaSimVHDL
  val xvhdl = VivadoSimVHDL
  object questa
  final val vsim = questa
  object vivado
  final val xsim = vivado

enum builders derives CanEqual:
  case foss, vendor

enum programmers derives CanEqual:
  case foss, vendor
