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
  object verilogLinters:
    val verilator = linters.verilator
    val iverilog = linters.iverilog
    val vlog = linters.vlog
    val xvlog = linters.xvlog
  object vhdlLinters:
    val ghdl = linters.ghdl
    val nvc = linters.nvc
    val vcom = linters.vcom
    val xvhdl = linters.xvhdl
end linters

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
  object verilogSimulators:
    export simulators.{verilator, iverilog, vlog, xvlog, questa, vsim, vivado, xsim}
  object vhdlSimulators:
    export simulators.{ghdl, nvc, vcom, xvhdl, questa, vsim, vivado, xsim}
end simulators

enum builders derives CanEqual:
  case foss, vendor

enum programmers derives CanEqual:
  case foss, vendor
