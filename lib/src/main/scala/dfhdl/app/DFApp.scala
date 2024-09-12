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
  private var elaborationOptions: options.ElaborationOptions = null
  private var compilerOptions: options.CompilerOptions = null
  private var printerOptions: options.PrinterOptions = null
  private var linterOptions: options.LinterOptions = null
  private var appOptions: options.AppOptions = null
  inline given options.ElaborationOptions = elaborationOptions
  inline given options.CompilerOptions = compilerOptions
  inline given options.PrinterOptions = printerOptions
  inline given options.AppOptions = appOptions
  private var dsn: () => core.Design = null
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
      argTypes: List[String],
      argValues: List[Any],
      argDescs: List[String]
  ): Unit =
    designName = _designName
    topScalaPath = _topScalaPath
    elaborationOptions = top.elaborationOptions
    compilerOptions = top.compilerOptions
    printerOptions = top.printerOptions
    appOptions = top.appOptions
    designArgs = DesignArgs(argNames, argTypes, argValues, argDescs)
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
        // execute command
        parsedCommandLine.mode match
          case help @ Mode.help =>
            help.subcommand match
              case Some(HelpMode.backend) => listBackends
              case _                      => println(parsedCommandLine.getFullHelpString())
          case Mode.elaborate => elaborate
          case Mode.compile   => compile
          case Mode.commit    => commit
          case Mode.lint      => lint
    end match
  end main
end DFApp
