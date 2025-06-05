package dfhdl.app
import dfhdl.options.LinterOptions
import dfhdl.tools.toolsCore.{VerilogLinter, VHDLLinter}
import dfhdl.tools.linters

final case class LintToolSelection(verilogLinter: VerilogLinter, vhdlLinter: VHDLLinter)
    derives CanEqual:
  override def toString(): String = s"$verilogLinter/$vhdlLinter"
object LintToolSelection:
  given (using
      lo: LinterOptions
  ): SingleValueConverter[LintToolSelection] with
    def parse(
        arg: String
    ): Either[String, Option[LintToolSelection]] =
      def parseTool(toolName: String): Option[dfhdl.tools.toolsCore.Simulator] =
        toolName match
          case "verilator" => Some(linters.verilator)
          case "iverilog"  => Some(linters.iverilog)
          case "vlog"      => Some(linters.vlog)
          case "xvlog"     => Some(linters.xvlog)
          case "ghdl"      => Some(linters.ghdl)
          case "nvc"       => Some(linters.nvc)
          case "vcom"      => Some(linters.vcom)
          case "xvhdl"     => Some(linters.xvhdl)
          case _           => None
      val toolNames = arg.split("\\/").toList
      val parsedTools = arg match
        case "questa" | "vsim" => List(Some(linters.vlog), Some(linters.vcom))
        case "vivado" | "xsim" => List(Some(linters.xvlog), Some(linters.xvhdl))
        case _                 => toolNames.map(parseTool)
      parsedTools match
        case Some(tool: VerilogLinter) :: Nil =>
          Right(Some(LintToolSelection(tool, lo.vhdlLinter)))
        case Some(tool: VHDLLinter) :: Nil =>
          Right(Some(LintToolSelection(lo.verilogLinter, tool)))
        case None :: _ =>
          Left(s"Invalid tool name: ${toolNames(0)}")
        case _ :: None :: _ =>
          Left(s"Invalid tool name: ${toolNames(1)}")
        case Some(tool1: VerilogLinter) :: Some(tool2: VerilogLinter) :: Nil =>
          Left(
            s"Invalid tool selection. Both tools ($tool1 and $tool2) are Verilog linters. When specifying two linters they must be exclusive for different backends."
          )
        case Some(tool1: VHDLLinter) :: Some(tool2: VHDLLinter) :: Nil =>
          Left(
            s"Invalid tool selection. Both tools ($tool1 and $tool2) are VHDL linters. When specifying two linters they must be exclusive for different backends."
          )
        case Some(verilogLinter: VerilogLinter) :: Some(vhdlLinter: VHDLLinter) :: Nil =>
          Right(Some(LintToolSelection(verilogLinter, vhdlLinter)))
        case Some(vhdlLinter: VHDLLinter) :: Some(verilogLinter: VerilogLinter) :: Nil =>
          Right(Some(LintToolSelection(verilogLinter, vhdlLinter)))
        case _ => Left("Invalid tool syntax.")
      end match
    end parse
  end given
end LintToolSelection
