package dfhdl.tools
import toolsCore.*
import dfhdl.internals.StableEnum

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

enum builders extends StableEnum derives CanEqual:
  case foss, vendor

enum programmers extends StableEnum derives CanEqual:
  case foss, vendor
