package dfhdl.tools
import toolsCore.*

object linters:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val vlog = QuestaSimVerilog
  val xvlog = VivadoSimVerilog
  val ghdl = GHDL
  val vcom = QuestaSimVHDL
  val xvhdl = VivadoSimVHDL
  object questa
  final val vsim = questa
  object vivado
  final val xsim = vivado
  // NVC is a single binary serving both languages, so the bare `nvc` is a marker (like `questa`)
  // that resolves per language, while each language scope below holds its actual front-end.
  object nvc
  object verilogLinters:
    val verilator = linters.verilator
    val iverilog = linters.iverilog
    val vlog = linters.vlog
    val xvlog = linters.xvlog
    val nvc = NVCVerilog
  object vhdlLinters:
    val ghdl = linters.ghdl
    val nvc = NVCVHDL
    val vcom = linters.vcom
    val xvhdl = linters.xvhdl
end linters

object simulators:
  val verilator = Verilator
  val iverilog = IcarusVerilog
  val vlog = QuestaSimVerilog
  val xvlog = VivadoSimVerilog
  val ghdl = GHDL
  val vcom = QuestaSimVHDL
  val xvhdl = VivadoSimVHDL
  object questa
  final val vsim = questa
  object vivado
  final val xsim = vivado
  // NVC is a single binary serving both languages, so the bare `nvc` is a marker (like `questa`)
  // that resolves per language, while each language scope below holds its actual front-end.
  object nvc
  object verilogSimulators:
    export simulators.{verilator, iverilog, vlog, xvlog, questa, vsim, vivado, xsim}
    val nvc = NVCVerilog
  object vhdlSimulators:
    export simulators.{ghdl, vcom, xvhdl, questa, vsim, vivado, xsim}
    val nvc = NVCVHDL
end simulators

enum builders derives CanEqual:
  case foss, vendor

enum programmers derives CanEqual:
  case foss, vendor
