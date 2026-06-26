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
  // The launcher name the tool is invoked by: the Windows `.exe`/`.bat` form on a Windows host, but
  // the plain `binExec` in dftools mode where the tool runs inside the Linux image (see
  // `isToolInWindows`).
  final protected def runExec(using ToolOptions): String =
    if (isToolInWindows) windowsBinExec else binExec
  // The host-native launcher name, used only for the local PATH lookup (`installedVersion`); in
  // dftools mode the version is probed inside the image instead.
  private def hostExec: String =
    if (osIsWindows) windowsBinExec else binExec
  protected def binExec: String
  protected def windowsBinExec: String = s"$binExec.exe"
  // True only when the tool actually runs as a Windows executable: a Windows host AND not inside the
  // (Linux) DFTools image. In dftools mode the tool runs on Linux even on a Windows host, so every
  // `.exe`/backslash decision keys off this instead of the raw host `osIsWindows`.
  protected final def isToolInWindows(using ToolOptions): Boolean = osIsWindows && !usesDFTools
  // The path separator the tool sees: host-native locally, '/' inside the Linux image.
  protected final def toolSeparatorChar(using ToolOptions): Char =
    if (isToolInWindows) '\\' else '/'
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
    val installedVersionRet = programFullPaths(hostExec).view.flatMap { runExecFullPath =>
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
    // in the image the tool is the plain `binExec`. version is dialect-independent, so the vhdl
    // flag (only relevant for yosys) doesn't matter.
    val image = DFToolsImage.imageFor(binExec, vhdl = false)
    if (!DFToolsImage.isAvailable(image)) None
    else
      val probeCmd =
        if (versionCmd.nonEmpty) binExec +: versionCmd.split(" ").filter(_.nonEmpty).toSeq
        else Seq(binExec)
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
    // convert to forward slashes when the tool always wants them (e.g. Verilator under MSYS) or
    // whenever the tool runs on Linux — natively or inside the DFTools image (`!isToolInWindows`).
    protected def convertWindowsToLinuxPaths(using ToolOptions): String =
      if (this.convertWindowsToLinuxPaths || !isToolInWindows) path.forceWindowsToLinuxPath
      else path

  // The distinct foreign IP sources present in the design (see `EDBlackBox.ForeignIP`).
  protected final def foreignSources(using
      getSet: MemberGetSet
  ): List[DFDesignBlock.InstMode.BlackBox.Source.ForeignIP] =
    getSet.designDB.members
      .collect { case d: DFDesignBlock => d.foreignIPSource }
      .flatten
      .distinctBy(_.resourcePath)

  // The distinct foreign IP design blocks. Used where the IP name is needed — that name is the
  // block's `dclName` (the HDL module/entity and the `<dclName>.<ext>` wrapper basename, which sits
  // directly in the IP resource folder).
  protected final def foreignBlocks(using getSet: MemberGetSet): List[DFDesignBlock] =
    getSet.designDB.members.collect { case d: DFDesignBlock if d.isForeignIPBlackbox => d }
      .distinctBy(_.dclName)

  // The foreign IP HDL wrapper basename(s) in the IP folder that THIS tool must compile, for an IP of
  // the given name (`dclName`), selected by the tool's foreign-function interface. Overridden per
  // tool family (DPI SystemVerilog, VPI Verilog, VHPI VHDL); the base tool consumes no wrapper.
  // `ToolOptions` is in scope so an override can vary the wrapper by tool back-end (e.g. GHDL picks
  // the mcode VHPI package on its mcode JIT back-end).
  protected def foreignWrapperHdlNames(ipName: String)(using ToolOptions): List[String] = Nil

  // Full (committed) paths of the foreign IP HDL wrappers this tool compiles, ordered so a VHDL
  // package precedes the entity that uses it. Empty for designs with no foreign IP. Paths are
  // relative to the exec dir (the tool's cwd), matching `designFiles`, so they resolve both for a
  // local tool and inside the DFTools image where only the cwd is mounted.
  protected final def foreignWrapperFiles(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): List[String] =
    foreignBlocks.flatMap { b =>
      val resourcePath = b.foreignIPSource.get.resourcePath
      foreignWrapperHdlNames(b.dclName).map { name =>
        s"$resourcePath/$name".convertWindowsToLinuxPaths
      }
    }

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

  protected def designFiles(using getSet: MemberGetSet, to: ToolOptions): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design | SourceType.BlackBox,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def toolFiles(using getSet: MemberGetSet, to: ToolOptions): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(SourceOrigin.Committed, SourceType.Tool(tn, _), path, _) if tn == toolName =>
        path.convertWindowsToLinuxPaths
    }

  protected def designDefFiles(using getSet: MemberGetSet, to: ToolOptions): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.DFHDLDef | SourceType.GlobalDef,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def designDefFolders(using getSet: MemberGetSet, to: ToolOptions): List[String] =
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
  // Whether this tool can run from a DFTools image at all. Proprietary tools (Questa, Vivado,
  // Quartus, Diamond, Gowin) have no DFTools image, so they transparently fall back to a local
  // install even when the default tools-location is `dftools`. Derived from the single source of
  // truth — the `DFToolsImage.imageForOpt` mapping (the `vhdl` flag only picks between two yosys
  // images, so it never affects whether an image exists).
  protected def supportsDFTools: Boolean =
    DFToolsImage.imageForOpt(binExec, vhdl = false).isDefined

  // True when the tool options select the DFTools image AND this tool actually has one; an
  // unsupported tool always runs from the local PATH (fallback) regardless of the option.
  protected final def usesDFTools(using to: ToolOptions): Boolean =
    supportsDFTools && to.runLocation == dfhdl.options.ToolOptions.Location.dftools

  // A short identifier of the active toolchain: its install location (local vs. the DFTools image)
  // and resolved version. Tools that keep cache-unmanaged build intermediates in the sandbox use
  // this to detect a toolchain switch.
  protected final def toolFingerprint(using ToolOptions): String =
    s"dftools=${usesDFTools};version=${getInstalledVersion}"

  // Cache-unmanaged, tool-owned build intermediates in the sandbox (e.g. verilator's `obj_dir`, the
  // GHDL `work-obj*.cf` library) that become invalid when the toolchain changes. They are NOT
  // tracked by the gen-file cache, so switching tools-location/version leaves the other toolchain's
  // stale objects behind — breaking incremental rebuilds or triggering spurious duplicate-definition
  // warnings. Tools override this to list such paths (files or directories).
  protected def staleToolArtifacts(using
      MemberGetSet,
      CompilerOptions,
      ToolOptions
  ): List[os.Path] =
    Nil

  // Purge the stale tool artifacts when the toolchain fingerprint differs from the previous run,
  // then record the current fingerprint in a per-tool stamp file. Same-toolchain runs keep their
  // intermediates (incremental rebuilds); a switched toolchain starts clean. Call once before the
  // tool analyzes/builds.
  protected final def purgeStaleToolArtifactsOnSwitch()(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Unit =
    val artifacts = staleToolArtifacts
    if (artifacts.nonEmpty)
      val stamp = os.Path(execPath, os.pwd) /
        s".dfhdl-toolchain-${toolName.filter(_.isLetterOrDigit)}"
      val fingerprint = toolFingerprint
      val unchanged = os.exists(stamp) && os.read(stamp) == fingerprint
      if (!unchanged)
        artifacts.foreach(p => if (os.exists(p)) os.remove.all(p))
        os.write.over(stamp, fingerprint)
  end purgeStaleToolArtifactsOnSwitch

  final protected def exec(
      cmd: String,
      prepare: => Unit = (),
      loggerOpt: Option[Tool.ProcessLogger] = None,
      // empty => resolved below per tools-location (host full path, or bare in-image name)
      runExec: String = "",
      // extra environment variables merged over `execEnv` for the spawned process (e.g. foreign IP
      // runtime config such as a viewer rendezvous address). Ignored in dftools (container) mode.
      extraEnv: Map[String, String] = Map.empty
  )(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    val dftools = usesDFTools
    // the executable to run: an explicit `runExec`, else the bare launcher name (dftools, run inside
    // the image) or the resolved host full path (local).
    val effRunExec =
      if (runExec.nonEmpty) runExec
      else if (dftools) this.runExec
      else this.runExecFullPath
    // a produced artifact to run in place (e.g. verilator's obj_dir/V<top>) carries a path
    // separator; a bare tool name does not. `effRunExec` is already in the tool's own form
    // (`toolSeparatorChar`, no `.exe`) since runExec/simRunExec/verilatedBinary key off
    // `isToolInWindows`, so no container-specific rewriting is needed here.
    val produced = effRunExec.contains(toolSeparatorChar)
    // the DFTools image this command runs in. A produced artifact runs in its producing tool's
    // image; a bare name maps directly (yosys also depends on the backend dialect).
    val dftoolsImage =
      if (dftools)
        val vhdl = summon[CompilerOptions].backend match
          case _: dfhdl.backends.vhdl => true
          case _                      => false
        DFToolsImage.imageFor(if (produced) this.runExec else effRunExec, vhdl)
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
        // run the tool inside its DFTools image; apptainer mounts the cwd (execPath) as $PWD, so
        // committed source/tool files are visible without an explicit bind. A produced artifact is
        // run by its relative ./path.
        val command =
          if (produced && !effRunExec.startsWith("./") && !effRunExec.startsWith("/"))
            s"./$effRunExec"
          else effRunExec
        val containerCmd = command +: cmd.split(" ").filter(_.nonEmpty).toSeq
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
    // Set once cancellation begins so the output pumper stops forwarding the tool's backlog: a killed
    // tool can leave a large buffered backlog that would otherwise keep trickling to the console
    // (paced by the throttle below), making the run look slow to stop. We drain it silently instead.
    @volatile var aborted = false
    val processOutput = loggerOpt.map(logger =>
      os.ProcessOutput.Readlines(line => if (!aborted) logger.out(line))
    ).getOrElse(
      if (System.console() != null) os.Inherit
      else os.ProcessOutput.Readlines(line =>
        if (!aborted)
          Tool.outputThrottle.gate()
          println(line)
      )
    )
    // spawn the process
    val process = os.proc(os.Shellable(argv)).spawn(
      cwd = os.Path(execPath, os.pwd),
      env =
        if (dftools) Map.empty[String, String]
        else
          // merge extraEnv over execEnv; for path-like vars prepend (don't clobber the tool's own
          // DLL-search guard or an inherited search path) by appending the existing value after.
          val pathKeys = Set("PATH", "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH")
          extraEnv.foldLeft(execEnv) { case (acc, (k, v)) =>
            if (pathKeys(k))
              val tail = acc.get(k).orElse(Option(System.getenv(k))).filter(_.nonEmpty)
              acc.updated(k, (v +: tail.toSeq).mkString(java.io.File.pathSeparator))
            else acc.updated(k, v)
          }
      ,
      stdin = os.Inherit,
      stdout = processOutput,
      mergeErrIntoOut = true
    )
    @volatile var interruptedBySignal = false
    // Force-kills the spawned launcher together with any child processes it forked, directly via the
    // underlying java.lang.Process rather than os-lib's `destroy`: `destroy(async = false)` joins the
    // output pumper, which can be blocked on a back-pressured write under an output flood, stalling
    // the cancellation. `destroyForcibly` returns immediately; the orphaned pumper hits EOF once the
    // process dies. On Windows the direct child is often a launcher (`gw_sh`, `vsim` -> `vsimk`, ...)
    // rather than the real worker, hence the descendants walk. Best-effort and idempotent.
    def destroyToolTree(): Unit =
      // stop forwarding the output backlog so cancellation is prompt (the pumper drains silently).
      aborted = true
      try
        process.wrapped.toHandle.descendants().forEach(p =>
          p.destroyForcibly()
          ()
        )
      catch case _: Throwable => ()
      process.wrapped.destroyForcibly()
    end destroyToolTree

    // dftools-on-Windows cancellation marker. There the tool runs behind a `wsl.exe` launcher, and a
    // console Ctrl+C reaches the in-VM signal wrapper but not this JVM (e.g. scala-cli). The wrapper
    // kills our host `wsl.exe` itself via WSL->Windows interop — the only thing that stops the flood,
    // since killing the in-VM tool alone leaves `wsl.exe` draining its output buffer — and drops a
    // `.dfhdl-cancel` marker in the (apptainer-mounted) cwd. This JVM watches for the marker to (a)
    // tear the launcher down from here too, as a fallback should the in-VM interop kill not land, and
    // (b) recognise the run as interrupted rather than read the killed launcher's exit code as an
    // error. We clear any stale marker before the run, and below after it.
    val cancelMarker: Option[os.Path] =
      if (dftools) Some(os.Path(execPath, os.pwd) / ".dfhdl-cancel") else None
    cancelMarker.foreach(m =>
      try os.remove(m)
      catch case _: Throwable => ()
    )
    cancelMarker.foreach { marker =>
      val t = new Thread(
        () =>
          try
            while (process.wrapped.isAlive && !interruptedBySignal)
              if (os.exists(marker))
                interruptedBySignal = true
                destroyToolTree()
              else Thread.sleep(50)
          catch case _: Throwable => (),
        s"$toolName cancel-marker watcher"
      )
      t.setDaemon(true)
      t.start()
    }

    // Ctrl+C reaches us through three mechanisms depending on the launcher:
    //  - `sbt`/standalone: a POSIX/Windows SIGINT delivered to this JVM fires the handler below.
    //  - `sbtn`: the run is on an sbt background-job thread cancelled via Thread.interrupt(), so
    //    `waitFor()` throws InterruptedException (no signal is raised).
    //  - dftools under a launcher that hides Ctrl+C from the JVM (scala-cli): neither fires; the
    //    in-VM wrapper kills our `wsl.exe` and drops the marker watched above.
    // We keep and restore the previous signal handler since under `sbtn` the long-lived server JVM is
    // reused across runs.
    val interruptHandler = new sun.misc.SignalHandler:
      def handle(sig: sun.misc.Signal): Unit =
        interruptedBySignal = true
        destroyToolTree()
    val prevHandler = sun.misc.Signal.handle(new sun.misc.Signal("INT"), interruptHandler)
    // Block on the underlying java.lang.Process rather than os-lib's `process.waitFor()`: the latter
    // also joins the output pumper once the process exits, and under an output flood that join blocks
    // on the back-pressured writer. Waiting on `wrapped` skips the join, so cancellation is immediate.
    val interruptedByThread =
      try
        process.wrapped.waitFor()
        false
      catch case _: InterruptedException => true
      finally sun.misc.Signal.handle(new sun.misc.Signal("INT"), prevHandler)
    // A dropped marker also means interrupted (the scala-cli path): the wrapper writes it before
    // killing our `wsl.exe`, so by the time `waitFor` returns it is visible. The single check keeps
    // normal runs free of latency (no marker -> instant false).
    val interrupted = interruptedByThread || interruptedBySignal || cancelMarker.exists(os.exists)
    cancelMarker.foreach(m =>
      try os.remove(m)
      catch case _: Throwable => ()
    )
    if (interrupted)
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
  // Rate-limits how fast tool output is forwarded to the console. This serves two purposes under an
  // `sbtn` output flood:
  //  1. It keeps the client<->server channel idle enough to deliver a Ctrl+C: the cancel arrives as a
  //     Thread.interrupt from sbt (not something we can read and prioritise), and a saturated channel
  //     never delivers it. Any full-speed burst re-saturates instantly, so the rate must be capped
  //     *continuously* (adaptive/burst variants failed).
  //  2. By sleeping out the rest of each window on the reader thread it back-pressures the tool, so a
  //     flood's backlog stays *upstream* (in `wsl.exe`, where killing the launcher drops it) instead
  //     of being slurped into JVM / sbt-server memory at full speed (observed: multi-GB), where
  //     killing the launcher no longer stops it.
  // Output below the cap is unaffected; nothing is dropped. The cap is a one-line tunable.
  private[toolsCore] object outputThrottle:
    private val windowNanos = 100_000_000L // 100ms
    private val maxLinesPerWindow = 50 // ~500 lines/s, leaving most of each window idle
    private var windowStartNanos = System.nanoTime()
    private var linesThisWindow = 0
    def gate(): Unit = synchronized {
      if (linesThisWindow == 0) windowStartNanos = System.nanoTime()
      linesThisWindow += 1
      if (linesThisWindow >= maxLinesPerWindow)
        val remaining = windowNanos - (System.nanoTime() - windowStartNanos)
        if (remaining > 0)
          try Thread.sleep(remaining / 1_000_000L, (remaining % 1_000_000L).toInt)
          catch case _: InterruptedException => Thread.currentThread().interrupt()
        linesThisWindow = 0
    }
  end outputThrottle

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
        outputThrottle.gate()
        println(line)
    final def out(s: => String): Unit = useLine(s)
    final def err(s: => String): Unit = useLine(s)
    final def buffer[T](f: => T): T = f
  end ProcessLogger
end Tool

trait VerilogTool extends Tool:
  // The include flag to be attached before each included folder
  protected def includeFolderFlag: String
  // DPI-C tools (Verilator, Questa, Vivado XSim) compile the SystemVerilog wrapper; the VPI tool
  // (Icarus) overrides this to the Verilog wrapper.
  override protected def foreignWrapperHdlNames(ipName: String)(using ToolOptions): List[String] =
    List(s"$ipName.sv")

trait VHDLTool extends Tool:
  // VHPIDIRECT tools (GHDL, NVC) compile the VHDL package then the entity that uses it.
  override protected def foreignWrapperHdlNames(ipName: String)(using ToolOptions): List[String] =
    List(s"${ipName}_pkg.vhdl", s"$ipName.vhdl")

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
    (designDefFolders.map(includeFolderFlag + _) ++ foreignWrapperFiles ++ toolFiles ++ designFiles)
      .mkString(" ")

trait VHDLLinter extends Linter, VHDLTool:
  // Converts the selected compiler vhdl dialect to the relevant lint flag
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String
  final protected def lintCmdSources(using CompilerOptions, ToolOptions, MemberGetSet): String =
    // foreign VHDL wrappers (package then entity) must precede the design that instantiates them
    (designDefFiles ++ foreignWrapperFiles ++ toolFiles ++ designFiles).mkString(" ")
  final protected def lintCmdLanguageFlag(using co: CompilerOptions): String =
    lintCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.vhdl].dialect)

trait Simulator extends Tool:
  type TOptions = SimulatorOptions
  val simRunsLint: Boolean = false
  protected def simRunExec(using MemberGetSet, ToolOptions): String = this.runExec
  protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    purgeStaleToolArtifactsOnSwitch()
    if (simRunsLint) this.asInstanceOf[Linter].lint(cd)
    else cd
  // Runs the foreign-IP simulation hooks (the viewer/streamer) around a tool's own run command, and
  // hands the run the environment the hooks need (e.g. `VGA_MONITOR_STREAM`) merged over `baseEnv`.
  // Factored out so tools that override `simulate` (Verilator, Questa) keep the hook lifecycle and
  // injected env instead of silently dropping them. `baseEnv` is the tool's own extra env (typically
  // the foreign runtime lib path). The hooks' `simEnv` is queried AFTER `onSimStart` so a hook can
  // report state it only knows once started (e.g. the TCP port it is now listening on).
  protected final def withForeignSimHooks(baseEnv: Map[String, String])(
      run: Map[String, String] => Unit
  )(using CompilerOptions, SimulatorOptions, MemberGetSet): Unit =
    val hooks = foreignSimHooks
    hooks.foreach(_.onSimStart())
    val env = baseEnv ++ hooks.flatMap(_.simEnv()).toMap
    try run(env)
    finally hooks.foreach(_.onSimEnd())

  def simulate(
      cd: CompiledDesign
  )(using CompilerOptions, SimulatorOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    withForeignSimHooks(foreignRuntimeLibPathEnv()) { env =>
      exec(simulateCmdFlags, simulatePrepare(), simulateLogger, simRunExec, env)
    }
    cd

  // The per-system subfolder of a foreign IP's bundled binaries, keyed off the platform the tool
  // actually runs on. Mirrors the vga-monitor release per-platform archive suffixes (`linux-x86_64`,
  // `linux-arm64`, `macos-x86_64`, `macos-arm64`, `windows-x86_64`, `windows-x86_64-mingw`). In
  // dftools the tool runs inside a native-arch Linux container, so the Linux binaries are selected
  // regardless of the host OS. On Windows two C runtimes coexist: MSVC (Questa) and MinGW (the
  // rest), selected via `windowsUsesMSVC`.
  protected final def hostPlatformTag(windowsUsesMSVC: Boolean = false)(using ToolOptions): String =
    val arch = sys.props.getOrElse("os.arch", "").toLowerCase
    val isArm = arch.contains("aarch64") || arch.contains("arm64")
    if (usesDFTools) if (isArm) "linux-arm64" else "linux-x86_64"
    else if (osIsWindows) if (windowsUsesMSVC) "windows-x86_64" else "windows-x86_64-mingw"
    else if (osIsLinux) if (isArm) "linux-arm64" else "linux-x86_64"
    else if (isArm) "macos-arm64"
    else "macos-x86_64"

  // The per-system library directory of a foreign IP (committed under `dfhdl-ips/<ip>/<platform>`).
  protected final def foreignLibDir(
      f: DFDesignBlock.InstMode.BlackBox.Source.ForeignIP,
      windowsUsesMSVC: Boolean = false
  )(using co: CompilerOptions, to: SimulatorOptions, getSet: MemberGetSet): String =
    // relative to the tool's cwd (the exec dir), so it works locally and in the mounted-cwd image;
    // `resourcePath` is the IP's `dfhdl-ips/<dclName>` folder
    s"${f.resourcePath}/${hostPlatformTag(windowsUsesMSVC)}".convertWindowsToLinuxPaths

  // Shared-library file name for a base name, keyed off the OS the tool actually runs on (no `lib`
  // prefix on Windows; `lib*.so` on Linux/in-container; `lib*.dylib` on macOS), matching how the IP
  // binaries are bundled per platform.
  protected final def foreignSharedLibFile(base: String)(using ToolOptions): String =
    if (isToolInWindows) s"$base.dll"
    else if (usesDFTools || osIsLinux) s"lib$base.so"
    else s"lib$base.dylib"

  // Link-time library file name for a foreign IP base name. On Unix the linker links the shared
  // object directly (`lib<base>.so`/`.dylib`). On Windows we also link the `<base>.dll` directly:
  // MinGW ld records a proper import from it. We deliberately do NOT use the bundled `<base>.a` here
  // — that archive is an xsim-specific shim (a tiny `*_xsim.o` that LoadLibrary's the DLL at call
  // time), not a generic import library, so linking it into a verilated binary never reaches the
  // backend.
  protected final def foreignLinkLibFile(base: String)(using ToolOptions): String =
    if (isToolInWindows) s"$base.dll"
    else foreignSharedLibFile(base)

  // Runtime dynamic-library search path for the simulator process: prepend every foreign IP's
  // selected lib dir to the OS loader variable so a shim linked without an rpath is still found.
  protected final def foreignRuntimeLibPathEnv(windowsUsesMSVC: Boolean = false)(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Map[String, String] =
    val dirs = foreignSources.map(f => foreignLibDir(f, windowsUsesMSVC)).distinct
    if (dirs.isEmpty) Map.empty
    else
      // keyed off the OS the tool runs on: in dftools it's the Linux container, not the host
      val varName =
        if (isToolInWindows) "PATH"
        else if (usesDFTools || osIsLinux) "LD_LIBRARY_PATH"
        else "DYLD_LIBRARY_PATH"
      // only the foreign dirs here; `exec` prepends these ahead of the existing path value
      Map(varName -> dirs.mkString(java.io.File.pathSeparator))
  end foreignRuntimeLibPathEnv

  // Foreign IP simulation hooks present in the design, each bound to its (IP-specific) context.
  // Loaded reflectively from the `simHookClass` FQN each foreign IP relays through its IR source (a
  // Scala `object` extending `ForeignSimHook`); the hook builds its own context from the generic
  // one we provide. Missing/unloadable hooks are silently skipped.
  protected final def foreignSimHooks(using
      co: CompilerOptions,
      to: SimulatorOptions,
      getSet: MemberGetSet
  ): List[dfhdl.tools.ForeignSimHook.Bound] =
    foreignBlocks
      .filter(_.foreignIPSource.exists(_.simHookClass.nonEmpty))
      .flatMap { b =>
        val fsrc = b.foreignIPSource.get
        loadForeignSimHook(fsrc.simHookClass).map { hook =>
          val ipDir = os.Path(execPath, os.pwd) / os.RelPath(fsrc.resourcePath)
          val base = new dfhdl.tools.ForeignSimContext(b.dclName, ipDir, topName)
          dfhdl.tools.ForeignSimHook.bind(hook, base)
        }
      }

  private def loadForeignSimHook(fqn: String): Option[dfhdl.tools.ForeignSimHook[?]] =
    // a Scala `object`'s runtime class carries a trailing `$` and exposes its singleton via the
    // static `MODULE$` field; accept the FQN with or without the `$`.
    val candidates = if (fqn.endsWith("$")) List(fqn) else List(fqn + "$", fqn)
    candidates.iterator.flatMap { name =>
      try
        Some(
          Class.forName(name).getField("MODULE$").get(null)
            .asInstanceOf[dfhdl.tools.ForeignSimHook[?]]
        )
      catch case _: Throwable => None
    }.nextOption()
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
    else
      (designDefFolders.map(includeFolderFlag + _) ++ foreignWrapperFiles ++ toolFiles ++
        designFiles)
        .mkString(" ")
end VerilogSimulator

trait VHDLSimulator extends Simulator, VHDLTool:
  // Converts the selected compiler vhdl dialect to the relevant lint flag
  protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String = ???
  final protected def simulateCmdSources(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    if (simRunsLint) ""
    else (designDefFiles ++ foreignWrapperFiles ++ toolFiles ++ designFiles).mkString(" ")
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
