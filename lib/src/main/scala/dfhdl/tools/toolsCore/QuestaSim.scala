package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import java.io.FileWriter
import java.io.File.separatorChar
import scala.sys.process.*

trait QuestaSimCommon extends Linter, Simulator:
  final val toolName: String = s"QuestaSim $binExec"
  final protected def versionCmd: String = "-version"
  final protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = s""".*$binExec\\s+(\\d+\\.\\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-quiet",
    summon[ToolOptions].Werror.toBoolean.toFlag("-warning error")
  )

  // creating a questa sim work lib if the work/_info file is missing
  final override protected def lintPrepare()(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Unit =
    val work = new java.io.File(s"${execPath}${separatorChar}work${separatorChar}_info")
    if (!work.exists())
      Process("vlib work", new java.io.File(execPath)).!

  override protected def simRunExec(using MemberGetSet): String =
    if (osIsWindows) "vsim.exe" else "vsim"

  // we do not cache the work directory because it is too complex,
  // so we override simulate and always call lint to recompile sources to work
  override def simulate(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    val ret = lint(cd)
    given MemberGetSet = ret.stagedDB.getSet
    val doCmd = "set NumericStdNoWarnings 1; run -all; quit"
    val cmd = constructCommand(
      "-quiet",
      "-batch",
      "-do",
      s"\"${doCmd}\"",
      topName
    )
    exec(cmd = cmd, loggerOpt = simulateLogger, runExec = simRunExec)
    ret
  end simulate
end QuestaSimCommon

object QuestaSimVerilog extends QuestaSimCommon, VerilogLinter, VerilogSimulator:
  protected def binExec: String = "vlog"
  protected def includeFolderFlag: String = "+incdir+"
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    dialect match
      case VerilogDialect.v95    => "-vlog95compat"
      case VerilogDialect.v2001  => "-vlog01compat"
      case VerilogDialect.sv2005 => "-sv05compat"
      case VerilogDialect.sv2009 => "-sv09compat"
      case VerilogDialect.sv2012 => "-sv12compat"
      case VerilogDialect.sv2017 => "-sv17compat"
  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    // suppressing "Design unit ... already exists and will be overwritten. Overwriting a VHDL entity with a Verilog module"
    "-suppress 13233"
  )
  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    ""
  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("# ** Warning:"),
        lineIsSuppressed = (line: String) => false,
        lineIsErrorOpt =
          // "Error" can be followed by "(suppressible), so we don't wait for colons too"
          Some((line: String) => line.startsWith("# ** Error") || line.startsWith("# ** Fatal:"))
      )
    )
end QuestaSimVerilog

object QuestaSimVHDL extends QuestaSimCommon, VHDLLinter, VHDLSimulator:
  protected def binExec: String = "vcom"
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    dialect match
      case VHDLDialect.v93   => "-93"
      case VHDLDialect.v2008 => "-2008"
      case VHDLDialect.v2019 => "-2019"
  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    // suppressing shared variable warnings
    "-suppress 1236",
    // suppressing "Type of expression ... is ambiguous"
    "-suppress 1320"
  )
  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    ""
  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    val inVHDL93 =
      summon[CompilerOptions].backend.asInstanceOf[backends.vhdl].dialect == VHDLDialect.v93
    var suppressCnt = 0
    var finishedFalseError = false
    Some(
      new Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("# ** Warning:"),
        lineIsSuppressed = (line: String) =>
          // VHDL'93 does not have a standard finish, so we detect the DFHDL generated
          // fatal report and convert it to the same behavior as in VHDL'2008 and later in QuestaSim
          if (inVHDL93)
            if (line.endsWith(": Finished successfully (not an error)"))
              suppressCnt = 4
              finishedFalseError = true
              true
            else if (suppressCnt > 0)
              suppressCnt -= 1
              true
            else if (finishedFalseError && line.startsWith("# Errors:"))
              // Extract error count and decrease by 1 for the false error
              val errorPattern = """# Errors: (\d+), Warnings: (\d+)""".r
              line match
                case errorPattern(errors, warnings) =>
                  val correctedErrors = errors.toInt - 1
                  println(s"# Errors: $correctedErrors, Warnings: $warnings")
              true
            else false
          else false,
        lineIsErrorOpt =
          Some((line: String) =>
            // "Error" can be followed by "(suppressible), so we don't wait for colons too"
            line.startsWith("# ** Error") || line.startsWith("# ** Failure:")
          )
      )
    )
  end simulateLogger
end QuestaSimVHDL
