package dfhdl.app
import dfhdl.options.LinterOptions
import dfhdl.tools.toolsCore.{VerilogLinter, VHDLLinter}

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
      def parseTool(toolName: String): Option[dfhdl.tools.toolsCore.Linter] =
        toolName match
          case "verilator" => Some(dfhdl.tools.linters.verilator)
          case "iverilog"  => Some(dfhdl.tools.linters.iverilog)
          case "vlog"      => Some(dfhdl.tools.linters.vlog)
          case "xvlog"     => Some(dfhdl.tools.linters.xvlog)
          case "ghdl"      => Some(dfhdl.tools.linters.ghdl)
          case "nvc"       => Some(dfhdl.tools.linters.nvc)
          case "vcom"      => Some(dfhdl.tools.linters.vcom)
          case "xvhdl"     => Some(dfhdl.tools.linters.xvhdl)
          case _           => None
      val toolNames = arg.split("\\/").toList
      toolNames.map(parseTool) match
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
