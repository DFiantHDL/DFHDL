package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import scala.sys.process.*

trait VivadoSimCommon extends Linter, Simulator:
  final val toolName: String = s"Vivado Simulator $binExec"
  final protected def versionCmd: String = "-version"
  final override protected def windowsBinExec: String = s"$binExec.bat"
  final protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Vivado Simulator\s+v(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))
  protected def suppressLine(line: String): Boolean = line.startsWith("INFO:")
  final override protected def lintLogger(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = Some(
    Tool.ProcessLogger(
      lineIsWarning = (line: String) => line.startsWith("WARNING:"),
      lineIsSuppressed = (line: String) => suppressLine(line)
    )
  )
end VivadoSimCommon

object VivadoSimVerilog extends VivadoSimCommon, VerilogLinter, VerilogSimulator:
  protected def binExec: String = "xvlog"
  protected def includeFolderFlag: String = "-i "
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => ""
      case _                                         => "--sv"
  // suppress info messages and initial value omission
  override protected def suppressLine(line: String): Boolean =
    super.suppressLine(line) || line.matches("WARNING: \\[VRFC 10\\-3467\\].*")
end VivadoSimVerilog

object VivadoSimVHDL extends VivadoSimCommon, VHDLLinter, VHDLSimulator:
  protected def binExec: String = "xvhdl"
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    dialect match
      case VHDLDialect.v93   => "--93_mode"
      case VHDLDialect.v2008 => "--2008"
      case VHDLDialect.v2019 => "--2019"
  // suppress info messages and shared variable warnings
  override protected def suppressLine(line: String): Boolean =
    super.suppressLine(line) || line.matches("WARNING: \\[VRFC 10\\-2115\\].*")
end VivadoSimVHDL
