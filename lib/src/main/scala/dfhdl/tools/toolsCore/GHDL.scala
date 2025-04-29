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
import dfhdl.compiler.stages.vhdl.VHDLBackend

object GHDL extends VHDLLinter, VHDLSimulator:
  override val simRunsLint: Boolean = true
  val toolName: String = "GHDL"
  protected def binExec: String = "ghdl"
  protected def versionCmd: String = s"version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """GHDL\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    val std = dialect match
      case VHDLDialect.v93   => "93"
      case VHDLDialect.v2008 => "08"
      case VHDLDialect.v2019 => "19"
    s"--std=$std"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-a",
    summon[ToolOptions].Werror.toBoolean.toFlag("--warn-error")
  )

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-frelaxed",
    "-Wno-shared"
  )

  override protected def simulateCmdPreLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "-r"
  )

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    topName,
    "--ieee-asserts=disable-at-0"
  )

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    val inVHDL93 =
      summon[CompilerOptions].backend.asInstanceOf[backends.vhdl].dialect == VHDLDialect.v93
    var finishedSuccessfully = false
    Some(
      new Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.contains(":(report warning):"),
        lineIsSuppressed = (line: String) =>
          // VHDL'93 does not have a standard finish, so we detect the DFHDL generated
          // fatal report and convert it to the same behavior as in VHDL'2008 and later in GHDL
          if (inVHDL93)
            if (line.endsWith(":(report failure): Finished successfully (not an error)"))
              // Extract the time and message from the line
              val timePattern = """@(\d+ns)""".r
              val time = timePattern.findFirstMatchIn(line).map(_.group(1)).get
              finishedSuccessfully = true
              println(s"simulation finished @$time")
              true
            else if (finishedSuccessfully)
              line == "ghdl:error: report failed" || line == "ghdl:error: simulation failed"
            else false
          else false,
        // GHDL does not report error codes for runtime errors, so we need to detect errors manually
        // even when using VHDL'2008 and later
        lineIsErrorOpt = Some((line: String) =>
          line.startsWith("ghdl:error:") || line.contains(":(report failure):") ||
            line.contains(":(report error):")
        )
      )
    )
  end simulateLogger

  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    lintCmdLanguageFlag(dialect)

end GHDL
