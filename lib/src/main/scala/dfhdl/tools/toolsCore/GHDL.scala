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
import dfhdl.compiler.stages.vhdl.VHDLBackend

object GHDL extends VHDLLinter, VHDLSimulator:
  override val simRunsLint: Boolean = true
  val toolName: String = "GHDL"
  protected def binExec: String = "ghdl"
  protected def versionCmd: String = s"version"
  // GHDL has three code generators, reported on the `ghdl version` banner: the mcode JIT (the default
  // in oss-cad-suite) and the LLVM / GCC back-ends. They differ in how a foreign VHPIDIRECT shared
  // library is wired (see the simulate flags), so we tag the parsed version with an `_mcode` suffix
  // when the banner names that back-end. This rides the existing (memoized, thread-safe) version probe
  // for both local and dftools modes — no extra invocation, no shared mutable state — and folds into
  // `toolFingerprint`, so swapping an mcode GHDL for an LLVM/GCC one at the same version invalidates
  // the sandbox. GHDL's version is never parsed numerically, so the suffix is safe.
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """GHDL\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map { m =>
      if (cmdRetStr.toLowerCase.contains("mcode")) s"${m.group(1)}_mcode" else m.group(1)
    }
  // Whether the resolved GHDL uses the mcode back-end (for the active run mode).
  private def isMcode(using ToolOptions): Boolean = getInstalledVersion.endsWith("_mcode")

  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    val std = dialect match
      case VHDLDialect.v93   => "93c" // using relaxed rules, because the pure VHDL-93 is too strict
      case VHDLDialect.v2008 => "08"
      case VHDLDialect.v2019 => "19"
    s"--std=$std"

  // GHDL stores its entire analyzed design library in a single `work-obj*.cf` file, recording the
  // *source path* of each analyzed unit. The path differs between a local install and the DFTools
  // image (`hdl\Foo.vhd` vs `hdl/Foo.vhd`), so re-analyzing into a `.cf` left over from the other
  // toolchain makes GHDL see each unit as defined twice (`-Wlibrary` duplicate warnings). The
  // library isn't cache-managed across toolchains, so it's purged on a toolchain switch.
  private def workLibFile(using co: CompilerOptions): String =
    co.backend.asInstanceOf[backends.vhdl].dialect match
      case VHDLDialect.v93   => "work-obj93.cf"
      case VHDLDialect.v2008 => "work-obj08.cf"
      case VHDLDialect.v2019 => "work-obj19.cf"

  override protected def staleToolArtifacts(using
      MemberGetSet,
      CompilerOptions,
      ToolOptions
  ): List[os.Path] = List(os.Path(execPath, os.pwd) / workLibFile)

  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      so: SimulatorOptions
  ): List[String] = List(workLibFile)

  // On the mcode back-end compile the mcode VHPI package (whose foreign attributes embed the shared
  // library name for runtime dlopen); the LLVM/GCC back-ends compile the plain package whose symbols
  // are resolved by the elaboration-time link. This is the bundle's SHARED package (one per
  // `resourcePath`, before any entity); the entity wrappers come from `VHDLTool`.
  override protected def foreignSharedHdlNames(familyName: String)(using
      ToolOptions
  ): List[String] =
    if (isMcode) List(s"${familyName}_pkg_mcode.vhdl")
    else List(s"${familyName}_pkg.vhdl")

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-a",
    summon[ToolOptions].Werror.toBoolean.toFlag("--warn-error")
  )

  // mcode's `_pkg_mcode.vhdl` wrapper embeds a fixed Linux soname (`lib<vhpiLib>.so`) in its
  // VHPIDIRECT foreign attributes. Rewrite it to the platform-correct shared-library filename so
  // GHDL's runtime loader finds the bundled binary (located via the loader path the base `simulate`
  // prepends). Done before lint analyzes the wrapper. No-op on Linux/in-container, where the soname
  // already matches. (The LLVM/GCC back-ends use the plain `_pkg.vhdl`, untouched here.)
  override protected def lintPrepare()(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Unit =
    if (isMcode)
      // the mcode package is shared once per bundle (`foreignSources` is deduped by `resourcePath`),
      // so patch each bundle's `<family>_pkg_mcode.vhdl` once.
      foreignSources.foreach { fsrc =>
        if (fsrc.vhpiLib.nonEmpty)
          val soname = s"lib${fsrc.vhpiLib}.so"
          val target = foreignSharedLibFile(fsrc.vhpiLib)
          if (soname != target)
            val familyName = fsrc.resourcePath.split('/').last
            val pkg = os.Path(execPath, os.pwd) / os.RelPath(fsrc.resourcePath) /
              s"${familyName}_pkg_mcode.vhdl"
            if (os.exists(pkg))
              val content = os.read(pkg)
              if (content.contains(soname))
                os.write.over(pkg, content.replace(soname, target))
      }

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
    "--elab-run",
    // Foreign IP VHPIDIRECT integration (elaboration options, before the unit name). The LLVM/GCC
    // back-ends link each IP's VHPI shared library at elaboration and embed its dir as an rpath. mcode
    // rejects `-Wl,` flags entirely and instead dlopens the library (named in the wrapper's foreign
    // attribute) at run time, so no link flags are emitted — the loader path the base `simulate`
    // prepends locates the binary.
    if (isMcode) ""
    else
      constructCommand(
        foreignSources.filter(_.vhpiLib.nonEmpty).flatMap { f =>
          val dir = foreignLibDir(f)
          Seq(s"-Wl,-L$dir", s"-Wl,-l${f.vhpiLib}", s"-Wl,-rpath,$dir")
        }*
      )
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
              val timePattern = """@(\d+\w+)""".r
              val time = timePattern.findFirstMatchIn(line).map(_.group(1)).get
              finishedSuccessfully = true
              println(s"simulation finished @$time")
              true
            else finishedSuccessfully // suppress all other lines after the first finish
          else false,
        // GHDL does not report error codes for runtime errors, so we need to detect errors manually
        // even when using VHDL'2008 and later
        lineIsErrorOpt = Some((line: String) =>
          line.contains(":error:") || line.contains(":(report failure):") ||
            line.contains(":(report error):")
        )
      )
    )
  end simulateLogger

  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    lintCmdLanguageFlag(dialect)

end GHDL
