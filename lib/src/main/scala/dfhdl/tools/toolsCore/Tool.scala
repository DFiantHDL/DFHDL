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

  private[dfhdl] lazy val (runExecFullPath, installedVersion) =
    var runExecFullPathRet: String = ""
    val installedVersionRet = programFullPaths(runExec).view.flatMap { runExecFullPath =>
      runExecFullPathRet = runExecFullPath
      val versionText =
        if (versionCmd.nonEmpty)
          val getVersionFullCmd =
            Process(
              s"$runExecFullPath $versionCmd",
              new java.io.File(System.getProperty("java.io.tmpdir")),
              // apply the same Windows DLL-search guard as `exec` (see `winDllPathEnv`); otherwise
              // the version probe inherits the polluted PATH and the tool's own sub-process (e.g.
              // `ivl -V`) loads the wrong runtime DLL and prints a spurious "Unable to get version"
              winDllSearchPath(runExecFullPath).map("PATH" -> _).toSeq*
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

  // Version probed from inside this tool's DFTools image (dftools mode). The tool is not on the
  // host PATH in this mode, so `installedVersion` (a PATH scan) is empty; instead we run the tool's
  // `versionCmd` inside its image and parse it with the same `extractVersion`. A `lazy val` (not a
  // var) so the probe runs once and is published safely across threads, like `installedVersion`.
  private lazy val dftoolsInstalledVersion: Option[String] =
    val exec = containerExec(this.runExec)
    // version is dialect-independent, so the vhdl flag (only relevant for yosys) doesn't matter.
    val image = DFToolsImage.imageFor(exec, vhdl = false)
    if (!DFToolsImage.isAvailable(image)) None
    else
      val probeCmd =
        if (versionCmd.nonEmpty) exec +: versionCmd.split(" ").filter(_.nonEmpty).toSeq
        else Seq(exec)
      val out =
        try DFToolsImage.probe(image, probeCmd)
        catch case _: Throwable => ""
      try extractVersion(out)
      catch case _: Exception => None

  protected def getInstalledVersion(using to: ToolOptions): String =
    if (usesDFTools)
      dftoolsInstalledVersion.getOrElse {
        error(s"${toolName} could not be found in its DFTools image.")
        "" // unreachable: `error` either exits or throws
      }
    else
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
    winDllSearchPath(runExecFullPath).map("PATH" -> _).toMap

  // Builds the PATH value used by `winDllPathEnv` (and the version probe), with the tool's own
  // install dirs (its exe dir plus the sibling `lib` and `lib\ivl`) prepended ahead of the
  // inherited PATH. Returns None off Windows or when the executable path is unknown.
  private def winDllSearchPath(exeFullPath: String): Option[String] =
    if (!osIsWindows || exeFullPath.isEmpty) None
    else
      Option(Paths.get(exeFullPath).getParent).map { exeDir =>
        val root = Option(exeDir.getParent)
        val dllDirs =
          (exeDir ::
            root.toList.flatMap(r => List(r.resolve("lib"), r.resolve("lib").resolve("ivl"))))
            .map(_.toString)
        val inheritedPath = Option(System.getenv("PATH")).getOrElse("")
        (dllDirs :+ inheritedPath).mkString(java.io.File.pathSeparator)
      }

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

  // --- DFTools (Apptainer image) execution support --------------------------
  // Whether this tool needs an X11 display forwarded into the container (GUI tools).
  protected def needsX11: Boolean = false
  // The executable name to invoke inside the Linux DFTools image, derived from the
  // (possibly Windows/host) `runExec`. Default: the file name with any `.exe` dropped.
  // Tools whose host launcher name differs from the in-image name can override.
  protected def containerExec(runExec: String): String =
    val base = Paths.get(runExec).getFileName.toString
    if (base.endsWith(".exe")) base.dropRight(4) else base
  // True when the given tool options select the DFTools image rather than local PATH tools.
  protected final def usesDFTools(using to: ToolOptions): Boolean =
    to.runLocation == dfhdl.options.ToolOptions.Location.dftools

  final protected def exec(
      cmd: String,
      prepare: => Unit = (),
      loggerOpt: Option[Tool.ProcessLogger] = None,
      // empty => resolved below per tools-location (host full path, or bare in-image name)
      runExec: String = ""
  )(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    val dftools = usesDFTools
    // the executable to run: an explicit `runExec`, else the host full path (local) or the bare
    // tool name (dftools, mapped to its in-image name by `containerExec`).
    val effRunExec =
      if (runExec.nonEmpty) runExec
      else if (dftools) this.runExec
      else this.runExecFullPath
    // a produced artifact to run in place (e.g. verilator's obj_dir/V<top>) has a path separator;
    // a plain tool name does not. The former runs by its relative path inside the mounted cwd.
    val produced = dftools && (effRunExec.contains(separatorChar) || effRunExec.contains('/'))
    val containerName =
      if (!dftools) ""
      else if (produced)
        // the container produces a Linux binary (no .exe); run it by relative ./path with /-seps
        val rel0 = effRunExec.replace('\\', '/')
        val rel = if (rel0.endsWith(".exe")) rel0.dropRight(4) else rel0
        if (rel.startsWith("./") || rel.startsWith("/")) rel else s"./$rel"
      else containerExec(effRunExec)
    // the DFTools image this command runs in. A produced artifact runs in its producing tool's
    // image; a tool name maps directly (yosys also depends on the backend dialect).
    val dftoolsImage =
      if (dftools)
        val vhdl = summon[CompilerOptions].backend match
          case _: dfhdl.backends.vhdl => true
          case _                      => false
        DFToolsImage.imageFor(if (produced) containerExec(this.runExec) else containerName, vhdl)
      else ""
    if (dftools)
      if (!DFToolsImage.isAvailable(dftoolsImage))
        error(
          s"DFTools image '$dftoolsImage' (${DFToolsImage.version}) could not be resolved for ${toolName}."
        )
    else preCheck()
    prepare
    val argv: Seq[String] =
      if (dftools)
        // run the bare tool inside its DFTools image; apptainer mounts the cwd (execPath) as $PWD,
        // so committed source/tool files are visible without an explicit bind. The tool runs on
        // Linux, so normalize Windows path separators in the args (relative source/include paths).
        val linuxCmd = cmd.replace('\\', '/')
        val containerCmd = containerName +: linuxCmd.split(" ").filter(_.nonEmpty).toSeq
        DFToolsImage.execArgv(dftoolsImage, containerCmd, needsX11)
      else
        val fullExec =
          // absolute path
          if (Paths.get(effRunExec).isAbsolute()) s"$effRunExec $cmd"
          // relative path with separator char, so we assume this is a product of the execution,
          // and therefore should be resolved against the exec path
          else if (effRunExec.contains(separatorChar))
            s"${Paths.get(execPath).toAbsolutePath().resolve(effRunExec)} $cmd"
          // for just executable name, we assume this is just another executable of the same tools,
          // so we use the full tool path and resolve the executable
          else s"${Paths.get(this.runExecFullPath).getParent().resolve(effRunExec)} $cmd"
        fullExec.split(" ").toSeq
    val displayCmd = argv.mkString(" ")

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
    val process = os.proc(os.Shellable(argv)).spawn(
      cwd = os.Path(execPath, os.pwd),
      env = if (dftools) Map.empty[String, String] else execEnv,
      stdin = os.Inherit,
      stdout = processOutput,
      mergeErrIntoOut = true
    )
    // Destroys the spawned tool together with any child processes it forked. We force-kill the
    // descendants and the process itself directly via the underlying java.lang.Process rather
    // than os-lib's `destroy`: `destroy(async = false)` joins the output-pumper thread, which can
    // be blocked on a back-pressured write when the tool floods stdout (e.g. a runaway sim print
    // loop), stalling the whole cancellation. `destroyForcibly` returns immediately; the orphaned
    // pumper hits EOF once the process dies and exits on its own. On Windows in particular,
    // killing only the direct child (`gw_sh`, `vsim` -> `vsimk`, ...) leaves the real workers
    // running, hence the descendants walk. Best-effort and idempotent: safe to call repeatedly.
    def destroyToolTree(): Unit =
      try
        process.wrapped.toHandle.descendants().forEach(p =>
          p.destroyForcibly(); ()
        )
      catch case _: Throwable => ()
      process.wrapped.destroyForcibly()

    // Ctrl+C handling. The cancellation reaches us through two different mechanisms depending on
    // the launcher:
    //  - under `sbt`/standalone, Ctrl+C is delivered to the JVM as a POSIX/Windows SIGINT and the
    //    signal handler below fires on the signal-dispatch thread.
    //  - under `sbtn`, the run executes on an sbt background-job thread that is cancelled via
    //    Thread.interrupt(); no signal is raised, so `waitFor()` throws InterruptedException.
    // We keep and restore the previous signal handler since under `sbtn` the same long-lived
    // server JVM is reused across runs.
    @volatile var interruptedBySignal = false
    val interruptHandler = new sun.misc.SignalHandler:
      def handle(sig: sun.misc.Signal): Unit =
        interruptedBySignal = true
        destroyToolTree()
    val prevHandler = sun.misc.Signal.handle(new sun.misc.Signal("INT"), interruptHandler)
    // Block on the underlying java.lang.Process rather than os-lib's `process.waitFor()`. Both
    // wait on the same handle and are interrupted promptly by a thread cancel (`sbtn`), but
    // os-lib's no-arg `waitFor()` additionally joins the output-pumper thread once the process
    // exits — and under an output flood that join blocks on the back-pressured writer, which is
    // what made the abort feel slow on the signal path. Waiting on `wrapped` skips that join, so
    // cancellation is immediate via either mechanism. (Polling with the timed `waitFor` was worse:
    // it doesn't surface the interrupt as promptly.) We drain the pumper below, only when the tool
    // finishes on its own.
    val interruptedByThread =
      try
        process.wrapped.waitFor()
        false
      catch case _: InterruptedException => true
      finally sun.misc.Signal.handle(new sun.misc.Signal("INT"), prevHandler)
    if (interruptedByThread || interruptedBySignal)
      // the tool (and its children) are now being torn down; unwind the whole run so the app
      // actually stops instead of silently continuing past the cancelled step.
      // ToolInterruptedException carries no stack trace and is caught by DFApp, so this neither
      // prints a noisy trace nor (under `sbtn`/sbt-shell) kills the reusable server JVM.
      destroyToolTree()
      println(s"\n${toolName} interrupted by user")
      throw new ToolInterruptedException(s"${toolName} interrupted by user")
    else
      // the tool finished on its own; join the output pumper (no-arg waitFor) so any buffered
      // lines are flushed before we read and report the exit code
      process.waitFor()
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
              |Command: $displayCmd""".stripMargin
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
