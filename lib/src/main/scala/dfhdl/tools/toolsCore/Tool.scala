package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.options.*
import java.io.IOException
import scala.sys.process.*
import dfhdl.internals.*
import java.nio.file.Paths
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect
import java.io.File.separatorChar

trait Tool:
  type TOptions <: ToolOptions
  val toolName: String
  final protected def runExec: String =
    if (osIsWindows) windowsBinExec else binExec
  protected def binExec: String
  protected def windowsBinExec: String = s"$binExec.exe"
  final protected def addSourceFiles(
      cd: CompiledDesign,
      sourceFiles: List[SourceFile]
  )(using CompilerOptions): CompiledDesign =
    val stagedDB = cd.stagedDB
    cd.newStage(stagedDB.update(srcFiles = stagedDB.srcFiles ++ sourceFiles)).commit

  protected def versionCmd: String
  protected def extractVersion(cmdRetStr: String): Option[String]
  protected[dfhdl] def producedFiles(using MemberGetSet, CompilerOptions, TOptions): List[String] =
    Nil
  protected[dfhdl] def cleanUpBeforeFileRestore()(using MemberGetSet, CompilerOptions): Unit = {}

  private[dfhdl] lazy val (runExecFullPath, installedVersion): (String, Option[String]) =
    var runExecFullPathRet: String = ""
    val installedVersionRet = programFullPaths(runExec).view.flatMap { runExecFullPath =>
      runExecFullPathRet = runExecFullPath
      val versionText =
        if (versionCmd.nonEmpty)
          val getVersionFullCmd =
            Process(
              s"$runExecFullPath $versionCmd",
              new java.io.File(System.getProperty("java.io.tmpdir"))
            )
          getVersionFullCmd.lazyLines_!.mkString("\n")
        else runExecFullPath
      // since the command is not guaranteed to return 0, we need to use lazyLines_! and avoid
      // exception handling (e.g., vivado returns 1 for version check)
      try extractVersion(versionText)
      catch case e: Exception => None
    }.headOption
    (runExecFullPathRet, installedVersionRet)
  end val
  final def isAvailable: Boolean = installedVersion.nonEmpty
  protected def getInstalledVersion(using to: ToolOptions): String =
    preCheck()
    installedVersion.get
  private var preCheckDone: Boolean = false
  final protected def error(msg: String)(using to: ToolOptions): Unit =
    // TODO: there is a false exhaustivity warning here in 3.4.2 or later
    to.onError.runtimeChecked match
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

  final protected def topName(using MemberGetSet): String = getSet.topName

  final protected def execPath(using co: CompilerOptions, getSet: MemberGetSet): String =
    co.topCommitPath(getSet.designDB)

  protected val convertWindowsToLinuxPaths: Boolean = false
  extension (path: String)
    protected def convertWindowsToLinuxPaths: String =
      if (this.convertWindowsToLinuxPaths) path.forceWindowsToLinuxPath else path

  // Extra environment variables to set for the spawned tool process, merged over the inherited
  // environment. Defaults to the Windows DLL-search guard below; tools that override this should
  // fold in `winDllPathEnv` so they keep that protection.
  protected def execEnv: Map[String, String] = winDllPathEnv

  // Windows DLL-hell guard.
  // Many bundled tools (oss-cad-suite's iverilog/ivl, ghdl, nvc, verilator, ...) dynamically link
  // the MinGW runtime `libstdc++-6.dll` / `libgcc_s_seh-1.dll`. For Icarus Verilog these live in
  // `<root>\lib` (a sibling of the `bin` that holds the launcher, and two levels above `ivl.exe`
  // itself) rather than next to the executable, so the loader resolves them through PATH. If an
  // unrelated toolchain on PATH (Git's mingw64, Quartus, Intel FPGA, ...) ships an incompatible
  // copy earlier than oss-cad-suite, the tool loads the wrong DLL and is killed at load time with
  // STATUS_ENTRYPOINT_NOT_FOUND (0xC0000139) or an access violation, producing no output. We
  // prepend the executable's own directory plus its sibling `lib` and `lib\ivl` so the tool
  // always finds its own bundled runtime DLLs first. No-op off Windows.
  protected final def winDllPathEnv: Map[String, String] =
    if (!osIsWindows || runExecFullPath.isEmpty) Map.empty
    else
      Option(Paths.get(runExecFullPath).getParent) match
        case None         => Map.empty
        case Some(exeDir) =>
          val root = Option(exeDir.getParent)
          val dllDirs =
            (exeDir ::
              root.toList.flatMap(r => List(r.resolve("lib"), r.resolve("lib").resolve("ivl"))))
              .map(_.toString)
          val pathSep = java.io.File.pathSeparator
          Map("PATH" -> (dllDirs :+ sys.env.getOrElse("PATH", "")).mkString(pathSep))

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
      runExec: String = this.runExecFullPath
  )(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    preCheck()
    prepare
    val fullExec =
      // absolute path
      if (Paths.get(runExec).isAbsolute()) s"$runExec $cmd"
      // relative path with separator char, so we assume this is a product of the execution,
      // and therefore should be resolved against the exec path
      else if (runExec.contains(separatorChar))
        s"${Paths.get(execPath).toAbsolutePath().resolve(runExec)} $cmd"
      // for just executable name, we assume this is just another executable of the same tools,
      // so we use the full tool path and resolve the executable
      else s"${Paths.get(this.runExecFullPath).getParent().resolve(runExec)} $cmd"

    // process the output.
    // note that reading the output line-by-line may affect the program behavior, since it is
    // disengaged from the TTY.
    // when no logger is set we would like to inherit the parent's stdout/stderr so the tool keeps
    // its TTY (colors, live progress). however, os.Inherit writes to the JVM's real file
    // descriptors, which under `sbtn` belong to the detached build server rather than the client
    // terminal, so the tool's output becomes invisible. when there is no real console (the `sbtn`
    // case, and CI), fall back to reading the tool's lines and re-emitting them through
    // System.out, which sbt forwards to the client.
    val processOutput = loggerOpt.map(logger =>
      os.ProcessOutput.Readlines(line => logger.out(line))
    ).getOrElse(
      if (System.console() != null) os.Inherit
      else os.ProcessOutput.Readlines(line => println(line))
    )
    // spawn the process
    val process = os.proc(os.Shellable(fullExec.split(" ").toSeq)).spawn(
      cwd = os.Path(execPath, os.pwd),
      env = execEnv,
      stdin = os.Inherit,
      stdout = processOutput,
      mergeErrIntoOut = true
    )
    // setup an interrupt handler to destroy the process.
    // this covers the `sbt` case, where Ctrl+C is delivered to the JVM as a POSIX/Windows
    // SIGINT signal. we keep the previously installed handler so we can restore it afterwards,
    // which matters under `sbtn` where the same long-lived server JVM is reused across runs.
    val interruptHandler = new sun.misc.SignalHandler:
      def handle(sig: sun.misc.Signal): Unit =
        process.destroy(shutdownGracePeriod = 100)
        println(s"\n${toolName} interrupted by user")
    val prevHandler = sun.misc.Signal.handle(new sun.misc.Signal("INT"), interruptHandler)
    // wait for the process to finish.
    // under `sbtn`, the app runs on an sbt background-job thread that is cancelled via
    // Thread.interrupt() rather than a SIGINT, so the signal handler above never fires and
    // waitFor() throws InterruptedException. destroy the process here as well so the spawned
    // tool never outlives the run in that case.
    val interrupted =
      try
        process.waitFor()
        false
      catch
        case _: InterruptedException =>
          process.destroy(shutdownGracePeriod = 100)
          println(s"\n${toolName} interrupted by user")
          true
      finally sun.misc.Signal.handle(new sun.misc.Signal("INT"), prevHandler)
    if (interrupted) {}
    else
      // get the error code, which may be overridden by the logger
      val errCode = loggerOpt.map { logger =>
        if (logger.lineIsErrorOpt.nonEmpty)
          if (logger.hasErrors) 1 else 0
        else process.exitCode()
      }.getOrElse(process.exitCode())
      // check if there are warnings
      val hasWarnings = loggerOpt.map(logger => logger.hasWarnings).getOrElse(false)
      // if there are errors or warnings and Werror is turned on, raise an application error
      if (errCode != 0 || hasWarnings && summon[ToolOptions].Werror.toBoolean)
        val msg =
          if (errCode != 0) s"${toolName} exited with the error code ${errCode}."
          else s"${toolName} exited with warnings while `Werror-tool` is turned on."
        error(
          s"""|$msg
              |Path: ${Paths.get(execPath).toAbsolutePath()}
              |Command: $fullExec""".stripMargin
        )
    end if
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
  type TOptions = SimulatorOptions
  val simRunsLint: Boolean = false
  protected def simRunExec(using MemberGetSet): String = this.runExec
  protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    if (simRunsLint) this.asInstanceOf[Linter].lint(cd)
    else cd
  def simulate(
      cd: CompiledDesign
  )(using CompilerOptions, SimulatorOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
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
  type TOptions = BuilderOptions
  protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign = cd
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign

trait Programmer extends Tool:
  protected[dfhdl] def programPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      ProgrammerOptions
  ): CompiledDesign = cd
  def program(
      cd: CompiledDesign
  )(using CompilerOptions, ProgrammerOptions): CompiledDesign
