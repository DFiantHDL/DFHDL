package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import dfhdl.compiler.stages.verilog.VerilogDialect

/** Xezim SystemVerilog simulator (https://github.com/aionhw/xezim). Not distributed for local
  * installation through the usual EDA channels, so it typically runs from its DFTools image
  * (`sim-xezim`), which the `auto` tools-location falls back to when no local `xezim` is found. A
  * single invocation parses, elaborates, and (for simulation) runs; there is no work-library or
  * build-artifact step, so linting maps to `--compile` and simulating to `--simulate`.
  *
  * Known upstream limitations: a design port whose type is an unpacked-array typedef (how DFHDL
  * prints vector and opaque ports) trips a false "Implicit net under `default_nettype none"
  * elaboration error (https://github.com/aionhw/xezim/issues/106), so such designs (e.g. the AES
  * suite) cannot run under xezim until that is fixed; and a DPI-C import inside a parameterized
  * child module is silently no-op'd (https://github.com/aionhw/xezim/issues/108), which blocks
  * every DPI foreign IP (the wrappers are parameterized child modules), so the ips sim specs do not
  * list xezim yet.
  */
object Xezim extends VerilogLinter, VerilogSimulator:
  val toolName: String = "Xezim"
  protected def binExec: String = "xezim"
  protected def versionCmd: String = "-V"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """xezim version\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  protected def includeFolderFlag: String = "-I"

  // xezim is SystemVerilog-only: IEEE 1800-2023 grammar by default, and `--sv2017` opts back to
  // the 1800-2017 edition, which covers every DFHDL sv dialect. There is no IEEE 1364 mode, so
  // the plain-Verilog dialects are unsupported.
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 =>
        throw new java.lang.IllegalArgumentException(
          "Current dialect is not supported for Xezim linting."
        )
      case _ => "--sv2017"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "--compile",
    s"-s $topName"
  )

  // xezim exits 0 even when it reports errors (https://github.com/aionhw/xezim/issues/107), so
  // the loggers own error detection: compile diagnostics read `[file] line:col: error: ...`,
  // elaboration failures read `Simulation error: ...`, and the runtime severity tasks print
  // Questa-style `** Error:`/`** Fatal:`.
  private def xezimLogger: Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) =>
          line.startsWith("** Warning") || line.contains(": warning:"),
        lineIsSuppressed = (line: String) => false,
        lineIsErrorOpt = Some((line: String) =>
          line.startsWith("** Error") || line.startsWith("** Fatal") ||
            line.startsWith("Simulation error:") || line.contains(": error:")
        )
      )
    )

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = xezimLogger

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = xezimLogger

  override protected def simulateCmdPreLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "--simulate",
    s"-s $topName",
    // xezim caps simulated time at 100us by default; DFHDL testbenches terminate themselves
    // with $finish, so push the cap far away to match the other simulators' unbounded default.
    "--max-time 1000s",
    // Foreign IP DPI integration: load each IP's DPI shared library at run time.
    constructCommand(
      foreignSources.filter(_.dpiLib.nonEmpty).map { f =>
        s"--dpi-lib ${foreignLibDir(f)}/${foreignSharedLibFile(f.dpiLib)}"
      }*
    )
  )

  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    lintCmdLanguageFlag(dialect)

end Xezim
