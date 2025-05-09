package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import scala.sys.process.*

object NVC extends VHDLLinter, VHDLSimulator:
  override val simRunsLint: Boolean = true
  val toolName: String = "NVC"
  protected def binExec: String = "nvc"
  protected def versionCmd: String = s"--version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """nvc\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    val std = dialect match
      case VHDLDialect.v93   => "93"
      case VHDLDialect.v2008 => "08"
      case VHDLDialect.v2019 => "19"
    s"--std=$std"

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    var insideWarning = false
    // Create a process logger to suppress the shared variable warning
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("** Warning:"),
        lineIsSuppressed = (line: String) =>
          if (line.matches("\\*\\* Warning: shared variable .* must have protected type"))
            // Start suppressing lines
            insideWarning = true
            true
          else if (insideWarning)
            // hit the end of the warning
            if (line.trim.endsWith("^")) insideWarning = false
            true
          // this is expected when mixing multiple simulators/linters all using "work" folder
          else if (line == "** Warning: directory work already exists and is not an NVC library")
            true
          else false
      )
    )
  end lintLogger

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-a",
    "--relaxed"
  )

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "-e",
    topName,
    "-r",
    "--ieee-warnings=off"
  )

  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    lintCmdLanguageFlag(dialect)

end NVC
