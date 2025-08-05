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

object IcarusVerilog extends VerilogLinter, VerilogSimulator:
  override val simRunsLint: Boolean = true
  val toolName: String = "Icarus Verilog"
  protected def binExec: String = "iverilog"
  override protected def simRunExec(using MemberGetSet): String =
    if (osIsWindows) "vvp.exe" else "vvp"
  protected def versionCmd: String = "-V"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Icarus Verilog version\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  protected def includeFolderFlag: String = "-I"

  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    val generation = dialect match
      case VerilogDialect.v95    => "1995"
      case VerilogDialect.v2001  => "2001"
      case VerilogDialect.sv2005 => "2005"
      case VerilogDialect.sv2009 => "2009"
      case VerilogDialect.sv2012 => "2012"
      case _                     =>
        throw new java.lang.IllegalArgumentException(
          "Current dialect is not supported for Icarus Verilog linting."
        )
    s"-g$generation"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    s"-s $topName",
    s"-o $topName"
  )

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-Wall"
  )

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = Some(
    Tool.ProcessLogger(
      lineIsWarning = (line: String) => line.contains("warning: "),
      lineIsSuppressed = (line: String) =>
        // suppress the "cannot be synthesized" warning when in simulation
        if (line.contains("cannot be synthesized") && getSet.designDB.inSimulation)
          true
        else
          false
    )
  )

  override protected[dfhdl] def producedFiles(using
      MemberGetSet,
      CompilerOptions,
      SimulatorOptions
  ): List[String] = List(s"${topName}")

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    topName
  )

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      new Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("WARNING:"),
        lineIsSuppressed = (line: String) => false,
        lineIsErrorOpt =
          Some((line: String) => line.startsWith("ERROR:") || line.startsWith("FATAL:"))
      )
    )
  end simulateLogger

  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    ""
end IcarusVerilog
