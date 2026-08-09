package dfhdl.app
import dfhdl.options.SimulatorOptions
import dfhdl.tools.toolsCore.{VerilogSimulator, VHDLSimulator}
import dfhdl.tools.simulators

final case class SimulateToolSelection(
    verilogSimulator: VerilogSimulator,
    vhdlSimulator: VHDLSimulator
) derives CanEqual:
  override def toString(): String = s"$verilogSimulator/$vhdlSimulator"
object SimulateToolSelection:
  given (using
      so: SimulatorOptions
  ): SingleValueConverter[SimulateToolSelection] with
    def parse(
        arg: String
    ): Either[String, Option[SimulateToolSelection]] =
      // `nvc` serves both languages, so in the two-tool `/` syntax it resolves by its slot
      // (first is the Verilog side, second is the VHDL side); the bare single `nvc` is handled
      // as a both-languages selection below, like `questa` and `vivado`.
      def parseTool(
          toolName: String,
          verilogSlot: Boolean
      ): Option[dfhdl.tools.toolsCore.Simulator] =
        toolName match
          case "verilator" => Some(simulators.verilator)
          case "iverilog"  => Some(simulators.iverilog)
          case "vlog"      => Some(simulators.vlog)
          case "xvlog"     => Some(simulators.xvlog)
          case "ghdl"      => Some(simulators.ghdl)
          case "nvc"       =>
            if (verilogSlot) Some(simulators.verilogSimulators.nvc)
            else Some(simulators.vhdlSimulators.nvc)
          case "vcom"  => Some(simulators.vcom)
          case "xvhdl" => Some(simulators.xvhdl)
          case _       => None
      val toolNames = arg.split("\\/").toList
      val parsedTools = arg match
        case "questa" | "vsim" => List(Some(simulators.vlog), Some(simulators.vcom))
        case "vivado" | "xsim" => List(Some(simulators.xvlog), Some(simulators.xvhdl))
        case "nvc"             =>
          List(Some(simulators.verilogSimulators.nvc), Some(simulators.vhdlSimulators.nvc))
        case _ =>
          toolNames.zipWithIndex.map((name, idx) => parseTool(name, verilogSlot = idx == 0))
      parsedTools match
        case Some(tool: VerilogSimulator) :: Nil =>
          Right(Some(SimulateToolSelection(tool, so.vhdlSimulator)))
        case Some(tool: VHDLSimulator) :: Nil =>
          Right(Some(SimulateToolSelection(so.verilogSimulator, tool)))
        case None :: _ =>
          Left(s"Invalid tool name: ${toolNames(0)}")
        case _ :: None :: _ =>
          Left(s"Invalid tool name: ${toolNames(1)}")
        case Some(tool1: VerilogSimulator) :: Some(tool2: VerilogSimulator) :: Nil =>
          Left(
            s"Invalid tool selection. Both tools ($tool1 and $tool2) are Verilog simulators. When specifying two simulators they must be exclusive for different backends."
          )
        case Some(tool1: VHDLSimulator) :: Some(tool2: VHDLSimulator) :: Nil =>
          Left(
            s"Invalid tool selection. Both tools ($tool1 and $tool2) are VHDL simulators. When specifying two simulators they must be exclusive for different backends."
          )
        case Some(verilogSimulator: VerilogSimulator) :: Some(
              vhdlSimulator: VHDLSimulator
            ) :: Nil =>
          Right(Some(SimulateToolSelection(verilogSimulator, vhdlSimulator)))
        case Some(vhdlSimulator: VHDLSimulator) :: Some(
              verilogSimulator: VerilogSimulator
            ) :: Nil =>
          Right(Some(SimulateToolSelection(verilogSimulator, vhdlSimulator)))
        case _ => Left("Invalid tool syntax.")
      end match
    end parse
  end given
end SimulateToolSelection
