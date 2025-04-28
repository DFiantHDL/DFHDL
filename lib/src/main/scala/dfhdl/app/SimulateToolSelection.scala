package dfhdl.app
import dfhdl.options.SimulatorOptions
import dfhdl.tools.toolsCore.{VerilogSimulator, VHDLSimulator}

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
      def parseTool(toolName: String): Option[dfhdl.tools.toolsCore.Simulator] =
        toolName match
          case "verilator" => Some(dfhdl.tools.simulators.verilator)
          case "iverilog"  => Some(dfhdl.tools.simulators.iverilog)
          case "vlog"      => Some(dfhdl.tools.simulators.vlog)
          case "xvlog"     => Some(dfhdl.tools.simulators.xvlog)
          case "ghdl"      => Some(dfhdl.tools.simulators.ghdl)
          case "nvc"       => Some(dfhdl.tools.simulators.nvc)
          case "vcom"      => Some(dfhdl.tools.simulators.vcom)
          case "xvhdl"     => Some(dfhdl.tools.simulators.xvhdl)
          case _           => None
      val toolNames = arg.split("\\/").toList
      toolNames.map(parseTool) match
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
