package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.options.{CompilerOptions, ToolOptions, LinterOptions, BuilderOptions, SimulatorOptions}
import dfhdl.options.OnError
import java.io.IOException
import scala.sys.process.*
import dfhdl.internals.*
import java.nio.file.Paths
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect

trait Tool:
  val toolName: String
  final protected def runExec: String =
    val osName: String = sys.props("os.name").toLowerCase
    if (osName.contains("windows")) windowsBinExec else binExec
  protected def binExec: String
  protected def windowsBinExec: String = s"$binExec.exe"
  final protected def addSourceFiles(
      cd: CompiledDesign,
      sourceFiles: List[SourceFile]
  )(using CompilerOptions): CompiledDesign =
    val stagedDB = cd.stagedDB
    cd.newStage(stagedDB.copy(srcFiles = stagedDB.srcFiles ++ sourceFiles)).commit

  protected def versionCmd: String
  protected def extractVersion(cmdRetStr: String): Option[String]

  private[dfhdl] lazy val installedVersion: Option[String] =
    val getVersionFullCmd =
      Process(s"$runExec $versionCmd", new java.io.File(System.getProperty("java.io.tmpdir")))
    try extractVersion(getVersionFullCmd.!!)
    catch case e: IOException => None
  final def isAvailable: Boolean = installedVersion.nonEmpty
  protected def getInstalledVersion(using to: ToolOptions): String =
    preCheck()
    installedVersion.get
  private var preCheckDone: Boolean = false
  final protected def error(msg: String)(using to: ToolOptions): Unit =
    // TODO: there is a false exhaustivity warning here in 3.4.2 or later
    (to.onError: @unchecked) match
      case OnError.Exit =>
        println(msg)
        sys.exit(1)
      case OnError.Exception => sys.error(msg)

  final protected def preCheck()(using to: ToolOptions): Unit =
    if (preCheckDone) {}
    else
      installedVersion.getOrElse {
        error(s"${toolName} could not be found.")
      }
      preCheckDone = true

  final protected def topName(using MemberGetSet): String =
    getSet.designDB.top.dclName

  final protected def execPath(using co: CompilerOptions, getSet: MemberGetSet): String =
    co.topCommitPath(getSet.designDB)

  protected val convertWindowsToLinuxPaths: Boolean = false
  extension (path: String)
    protected def convertWindowsToLinuxPaths: String =
      if (this.convertWindowsToLinuxPaths) path.forceWindowsToLinuxPath else path

  protected def designFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design | SourceType.BlackBox,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def toolFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(SourceOrigin.Committed, SourceType.Tool(tn, _), path, _) if tn == toolName =>
        path.convertWindowsToLinuxPaths
    }

  protected def designDefFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.DFHDLDef | SourceType.GlobalDef,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def designDefFolders(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.DFHDLDef | SourceType.GlobalDef,
            path,
            _
          ) =>
        Paths.get(path).getParent.toString.convertWindowsToLinuxPaths
    }.distinct

  protected def constructCommand(args: String*): String =
    args.filter(_.nonEmpty).mkString(" ")

  final protected def exec(
      cmd: String,
      prepare: => Unit = (),
      loggerOpt: Option[Tool.ProcessLogger] = None,
      runExec: String = this.runExec
  )(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    preCheck()
    prepare
    val fullExec = s"$runExec $cmd"
    var process: Option[scala.sys.process.Process] = None
    val pb = new java.lang.ProcessBuilder(fullExec.split(" ")*)
    pb.directory(new java.io.File(execPath))
    pb.redirectErrorStream(true)
    val processBuilder = Process(pb)
    var hasWarnings: Boolean = false

    val handler = new sun.misc.SignalHandler:
      def handle(sig: sun.misc.Signal): Unit =
        process.foreach(p =>
          p.destroy()
          p.exitValue()
        )
        println(s"\n${toolName} interrupted by user")
    sun.misc.Signal.handle(new sun.misc.Signal("INT"), handler)

    val errCode = loggerOpt.map(logger =>
      val p = processBuilder.run(logger)
      process = Some(p)
      val errCode = p.exitValue()
      hasWarnings = logger.hasWarnings
      if (logger.lineIsErrorOpt.nonEmpty)
        if (logger.hasErrors) 1 else 0
      else errCode
    ).getOrElse({
      val p = processBuilder.run()
      process = Some(p)
      p.exitValue()
    })

    if (errCode != 0 || hasWarnings && summon[ToolOptions].Werror.toBoolean)
      val msg =
        if (errCode != 0) s"${toolName} exited with the error code ${errCode}."
        else s"${toolName} exited with warnings while `Werror-tool` is turned on."
      error(
        s"""|$msg
            |Path: ${Paths.get(execPath).toAbsolutePath()}
            |Command: $fullExec""".stripMargin
      )
  end exec
  override def toString(): String = binExec
end Tool
object Tool:
  class ProcessLogger(
      lineIsWarning: String => Boolean,
      lineIsSuppressed: String => Boolean,
      // set to override error detection
      val lineIsErrorOpt: Option[String => Boolean] = None
  ) extends scala.sys.process.ProcessLogger:
    private var _hasWarnings: Boolean = false
    private var _hasErrors: Boolean = false
    final def hasWarnings: Boolean = _hasWarnings
    final def hasErrors: Boolean = _hasErrors
    private def useLine(line: String): Unit =
      if (!lineIsSuppressed(line))
        if (!_hasWarnings && lineIsWarning(line)) _hasWarnings = true
        if (!_hasErrors && lineIsErrorOpt.map(_(line)).getOrElse(false)) _hasErrors = true
        println(line)
    final def out(s: => String): Unit = useLine(s)
    final def err(s: => String): Unit = useLine(s)
    final def buffer[T](f: => T): T = f
  end ProcessLogger
end Tool

trait VerilogTool extends Tool:
  // The include flag to be attached before each included folder
  protected def includeFolderFlag: String

trait VHDLTool extends Tool

trait Linter extends Tool:
  protected[dfhdl] def lintPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign = cd
  final def lint(
      cd: CompiledDesign
  )(using CompilerOptions, ToolOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(lintCmdFlags, lintPrepare(), lintLogger)
    cd
  protected def lintPrepare()(using CompilerOptions, ToolOptions, MemberGetSet): Unit = {}
  protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = None
  protected def lintCmdLanguageFlag(using co: CompilerOptions): String
  protected def lintCmdSources(using CompilerOptions, ToolOptions, MemberGetSet): String
  protected def lintCmdPreLangFlags(using CompilerOptions, ToolOptions, MemberGetSet): String = ""
  protected def lintCmdPostLangFlags(using CompilerOptions, ToolOptions, MemberGetSet): String =
    ""
  final protected def lintCmdFlags(using CompilerOptions, ToolOptions, MemberGetSet): String =
    constructCommand(lintCmdPreLangFlags, lintCmdLanguageFlag, lintCmdPostLangFlags, lintCmdSources)
end Linter

trait VerilogLinter extends Linter, VerilogTool:
  // Converts the selected compiler verilog dialect to the relevant lint flag
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String
  final protected def lintCmdLanguageFlag(using co: CompilerOptions): String =
    lintCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.verilog].dialect)
  final protected def lintCmdSources(using CompilerOptions, ToolOptions, MemberGetSet): String =
    (designDefFolders.map(includeFolderFlag + _) ++ toolFiles ++ designFiles).mkString(" ")

trait VHDLLinter extends Linter, VHDLTool:
  // Converts the selected compiler vhdl dialect to the relevant lint flag
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String
  final protected def lintCmdSources(using CompilerOptions, ToolOptions, MemberGetSet): String =
    (designDefFiles ++ toolFiles ++ designFiles).mkString(" ")
  final protected def lintCmdLanguageFlag(using co: CompilerOptions): String =
    lintCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.vhdl].dialect)

trait Simulator extends Tool:
  protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign = cd
  val simRunsLint: Boolean = false
  protected def simRunExec: String = this.runExec
  def simulate(
      cd: CompiledDesign
  )(using CompilerOptions, SimulatorOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    if (simRunsLint) this.asInstanceOf[Linter].lint(cd)
    exec(simulateCmdFlags, simulatePrepare(), simulateLogger, simRunExec)
    cd
  protected def simulatePrepare()(using CompilerOptions, SimulatorOptions, MemberGetSet): Unit = {}
  protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = None
  protected def simulateCmdLanguageFlag(using co: CompilerOptions): String
  protected def simulateCmdSources(using CompilerOptions, SimulatorOptions, MemberGetSet): String
  protected def simulateCmdPreLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = ""
  protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    ""
  final protected def simulateCmdFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    constructCommand(
      simulateCmdPreLangFlags,
      simulateCmdLanguageFlag,
      simulateCmdPostLangFlags,
      simulateCmdSources
    )
end Simulator

trait VerilogSimulator extends Simulator, VerilogTool:
  // Converts the selected compiler verilog dialect to the relevant lint flag
  protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String = ???
  final protected def simulateCmdLanguageFlag(using co: CompilerOptions): String =
    simulateCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.verilog].dialect)
  final protected def simulateCmdSources(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    if (simRunsLint) ""
    else (designDefFolders.map(includeFolderFlag + _) ++ toolFiles ++ designFiles).mkString(" ")

trait VHDLSimulator extends Simulator, VHDLTool:
  // Converts the selected compiler vhdl dialect to the relevant lint flag
  protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String = ???
  final protected def simulateCmdSources(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    if (simRunsLint) ""
    else (designDefFiles ++ toolFiles ++ designFiles).mkString(" ")
  final protected def simulateCmdLanguageFlag(using co: CompilerOptions): String =
    simulateCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.vhdl].dialect)

trait Builder extends Tool:
  protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign = cd
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign
object Builder:
  // default linter will be vivado
  given Builder = dfhdl.tools.builders.vivado
