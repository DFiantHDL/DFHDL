package dfhdl.app
import dfhdl.*
import dfhdl.compiler.ir
import wvlet.log.{Logger, LogFormatter}
import scala.collection.mutable
import dfhdl.options.CompilerOptions
import org.rogach.scallop.*
import dfhdl.internals.sbtShellIsRunning
import scala.util.chaining.scalaUtilChainingOps

trait DFApp:
  private val logger = Logger("DFHDL App")
  logger.setFormatter(LogFormatter.BareFormatter)
  private var designName: String = ""
  private var topScalaPath: String = ""
  // this context is just for enabling `getConstData` to work.
  // the internal global context inside `value` will be actually at play here.
  val dfc: DFC = DFC.emptyNoEO

  private var designArgs: DesignArgs = DesignArgs.empty
  private var elaborationOptions: options.ElaborationOptions = compiletime.uninitialized
  private var compilerOptions: options.CompilerOptions = compiletime.uninitialized
  private var printerOptions: options.PrinterOptions = compiletime.uninitialized
  private var linterOptions: options.LinterOptions = compiletime.uninitialized
  private var simulatorOptions: options.SimulatorOptions = compiletime.uninitialized
  private var appOptions: options.AppOptions = compiletime.uninitialized
  inline given options.ElaborationOptions = elaborationOptions
  inline given options.CompilerOptions = compilerOptions
  inline given options.PrinterOptions = printerOptions
  inline given options.LinterOptions = linterOptions
  inline given options.SimulatorOptions = simulatorOptions
  inline given options.AppOptions = appOptions
  private var dsn: () => core.Design = compiletime.uninitialized
  // used by the plugin to get the updated design arguments that could be changed by the
  // command-line options
  final protected def getDsnArg(name: String): Any =
    designArgs(name).value
  // used by the plugin to get the updated elaboration options that could be changed by the
  // command-line options
  final protected def getElaborationOptions: options.ElaborationOptions = elaborationOptions
  final protected def setInitials(
      _designName: String,
      _topScalaPath: String,
      top: dfhdl.top,
      argNames: List[String],
      argValues: List[Any],
      argDescs: List[String]
  ): Unit =
    designName = _designName
    topScalaPath = _topScalaPath
    elaborationOptions = top.elaborationOptions
    compilerOptions = top.compilerOptions
    printerOptions = top.printerOptions
    linterOptions = top.linterOptions
    simulatorOptions = top.simulatorOptions
    appOptions = top.appOptions
    designArgs = DesignArgs(argNames, argValues, argDescs)
  end setInitials
  final protected def setDsn(d: => core.Design): Unit = dsn = () => d
  private def elaborate: core.Design =
    logger.info("Elaborating design...")
    // the elaboration options are set in the compiler plugin using getElaborationOptions
    val elaborated = dsn()
    if (elaborationOptions.printDFHDLCode)
      println(
        """|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |The design code after elaboration:
           |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin
      )
      elaborated.printCodeString
    elaborated

  private inline def compile =
    elaborate.tap(_ => logger.info("Compiling design...")).compile

  private inline def commit =
    compile.tap(_ => logger.info("Committing backend files to disk...")).commit

  private inline def lint =
    commit.tap(_ => logger.info("Running external linter...")).lint

  private inline def simulate =
    commit.tap(_ => logger.info("Running external simulator...")).simulate

  private def listBackends: Unit =
    println(
      s"""|Backend option pattern: -b <lang>[.<dialect>]
          |<lang>    - the required backend language
          |<dialect> - the optional language dialect (each language has a default dialect)
          |Examples: 
          |-b vhdl          - VHDL v2008 (default dialect) 
          |-b verilog.v2001 - Verilog 2001
          |
          |Available languages (see their dialects below):
          |verilog - Verilog or SystemVerilog
          |vhdl    - VHDL
          |
          |Available Verilog/SystemVerilog dialects:
          |v95     - Verilog 1995
          |v2001   - Verilog 2001
          |sv2005  - SystemVerilog 2005
          |sv2009  - SystemVerilog 2009 [default]
          |sv2012  - SystemVerilog 2012
          |sv2017  - SystemVerilog 2017
          |
          |Available VHDL dialects:
          |v93     - VHDL 1993
          |v2008   - VHDL 2008 [default]
          |v2019   - VHDL 2019""".stripMargin
    )
  end listBackends

  private def listLintTools(scan: Boolean): Unit =
    def scanned(tool: dfhdl.tools.toolsCore.Tool): String =
      if (scan)
        tool.installedVersion match
          case Some(version) => s"Found version $version"
          case None          => "Not found on your system"
      else ""
    println(
      s"""|Linter tool option pattern: -t [<verilogLinter>][/][<vhdlLinter>]
          |<verilogLinter> - the selected Verilog/SystemVerilog linter
          |<vhdlLinter>    - the selected VHDL linter
          |You may specify both linters by separating with `/` or just the
          |one you intend to run according to your chosen backend.
          |Examples:
          |-t verilator     - Set the Verilog linter to Verilator (VHDL linter remains default)
          |-t nvc           - Set the VHDL linter to NVC (Verilog linter remains default)
          |-t iverilog/ghdl - Set both Verilog and VHDL linters
          |
          |Selectable Verilog/SystemVerilog linting tools:
          |verilator - Verilator (default) ${scanned(dfhdl.tools.linters.verilator)}
          |iverilog  - Icarus Verilog      ${scanned(dfhdl.tools.linters.iverilog)}
          |vlog      - QuestaSim/ModelSim  ${scanned(dfhdl.tools.linters.vlog)}
          |xvlog     - Vivado Simulator    ${scanned(dfhdl.tools.linters.xvlog)}
          |
          |Selectable VHDL linting tools:
          |ghdl      - GHDL (default)      ${scanned(dfhdl.tools.linters.ghdl)}
          |nvc       - NVC                 ${scanned(dfhdl.tools.linters.nvc)}
          |vcom      - QuestaSim/ModelSim  ${scanned(dfhdl.tools.linters.vcom)}
          |xvhdl     - Vivado Simulator    ${scanned(dfhdl.tools.linters.xvhdl)}""".stripMargin
    )
  end listLintTools
  private def listSimulateTools(scan: Boolean): Unit =
    def scanned(tool: dfhdl.tools.toolsCore.Tool): String =
      if (scan)
        tool.installedVersion match
          case Some(version) => s"Found version $version"
          case None          => "Not found on your system"
      else ""
    println(
      s"""|Simulator tool option pattern: -t [<verilogSimulator>][/][<vhdlSimulator>]
          |<verilogSimulator> - the selected Verilog/SystemVerilog simulator
          |<vhdlSimulator>    - the selected VHDL simulator
          |You may specify both simulators by separating with `/` or just the
          |one you intend to run according to your chosen backend.
          |Examples:
          |-t verilator     - Set the Verilog simulator to Verilator (VHDL simulator remains default)
          |-t nvc           - Set the VHDL simulator to NVC (Verilog simulator remains default)
          |-t iverilog/ghdl - Set both Verilog and VHDL simulators
          |
          |Selectable Verilog/SystemVerilog simulation tools:
          |verilator - Verilator (default) ${scanned(dfhdl.tools.simulators.verilator)}
          |iverilog  - Icarus Verilog      ${scanned(dfhdl.tools.simulators.iverilog)}
          |vlog      - QuestaSim/ModelSim  ${scanned(dfhdl.tools.simulators.vlog)}
          |xvlog     - Vivado Simulator    ${scanned(dfhdl.tools.simulators.xvlog)}
          |
          |Selectable VHDL simulation tools:
          |ghdl      - GHDL (default)      ${scanned(dfhdl.tools.simulators.ghdl)}
          |nvc       - NVC                 ${scanned(dfhdl.tools.simulators.nvc)}
          |vcom      - QuestaSim/ModelSim  ${scanned(dfhdl.tools.simulators.vcom)}
          |xvhdl     - Vivado Simulator    ${scanned(dfhdl.tools.simulators.xvhdl)}""".stripMargin
    )
  end listSimulateTools

  def main(commandArgs: Array[String]): Unit =
    if (appOptions.clearConsole) print("\u001bc")
    logger.info(s"Welcome to DFiant HDL (DFHDL) v$dfhdlVersion !!!")
    val parsedCommandLine = ParsedCommandLine(designName, topScalaPath, designArgs, commandArgs)
    import parsedCommandLine.{Mode, HelpMode}
    if (commandArgs.isEmpty && parsedCommandLine.mode != Mode.help)
      logger.info(
        "No command-line given; using defaults. Run with `help` argument to get usage text."
      )
    parsedCommandLine.getExitCodeOption match
      case Some(code) =>
        if (!sbtShellIsRunning) sys.exit(code)
      case None =>
        given CanEqual[ScallopConfBase, ScallopConfBase] = CanEqual.derived
        // update design args from command line
        designArgs = parsedCommandLine.updatedDesignArgs
        // update elaboration options from command line
        parsedCommandLine.mode match
          case mode: Mode.ElaborateMode =>
            elaborationOptions = elaborationOptions.copy(
              printDFHDLCode = mode.`print-elaborate`.toOption.get
            )
          case _ =>
        // update compiler options from command line
        parsedCommandLine.mode match
          case mode: Mode.CompileMode =>
            compilerOptions = compilerOptions.copy(
              backend = mode.backend.toOption.get,
              printDFHDLCode = mode.`print-compile`.toOption.get,
              printBackendCode = mode.`print-backend`.toOption.get
            )
          case _ =>
        // update linter options from command line
        parsedCommandLine.mode match
          case mode: Mode.LintMode =>
            val toolSelection = mode.tool.toOption.get
            linterOptions = linterOptions.copy(
              verilogLinter = toolSelection.verilogLinter,
              vhdlLinter = toolSelection.vhdlLinter,
              fatalWarnings = mode.fatalWarnings.toOption.get
            )
          case mode: Mode.SimulateMode =>
            val toolSelection = mode.tool.toOption.get
            simulatorOptions = simulatorOptions.copy(
              verilogSimulator = toolSelection.verilogSimulator,
              vhdlSimulator = toolSelection.vhdlSimulator,
              fatalWarnings = mode.fatalWarnings.toOption.get
            )
          case _ =>
        end match
        // execute command
        parsedCommandLine.mode match
          case help @ Mode.help =>
            help.subcommand match
              case Some(HelpMode.backend) => listBackends
              case Some(lintTool: HelpMode.`lint-tool`.type) =>
                listLintTools(lintTool.scan.toOption.get)
              case Some(simulateTool: HelpMode.`simulate-tool`.type) =>
                listSimulateTools(simulateTool.scan.toOption.get)
              case _ => println(parsedCommandLine.getFullHelpString())
          case Mode.elaborate => elaborate
          case Mode.compile   => compile
          case Mode.commit    => commit
          case Mode.lint      => lint
          case Mode.simulate  => simulate
    end match
  end main
end DFApp
