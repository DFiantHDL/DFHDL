package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import scala.sys.process.*

trait NVCCommon extends Linter, Simulator:
  final override val simRunsLint: Boolean = true
  final val toolName: String = "NVC"
  final protected def binExec: String = "nvc"
  final protected def versionCmd: String = s"--version"
  final protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """nvc\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  // The installed version as a comparable major.minor double (e.g. "1.22.1" -> 1.22).
  final protected def installedVersionDouble(using ToolOptions): Double =
    getInstalledVersion.split("\\.").take(2).mkString(".").toDouble

  // Expected when mixing multiple simulators/linters all using the same "work" folder.
  final protected def lineIsForeignWorkDirWarning(line: String): Boolean =
    line == "** Warning: directory work already exists and is not an NVC library"

  // Elaborate the analyzed top before running it (both languages share the same
  // analyze -> elaborate -> run flow).
  override protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    val ret = super.simulatePreprocess(cd)
    given MemberGetSet = ret.stagedDB.getSet
    exec(constructCommand("-e", topName))
    ret
end NVCCommon

object NVCVHDL extends NVCCommon, VHDLLinter, VHDLSimulator:
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    val std = dialect match
      case VHDLDialect.v93   => "93"
      case VHDLDialect.v2008 => "08"
      case VHDLDialect.v2019 => "19"
    s"--std=$std"

  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      so: SimulatorOptions
  ): List[String] =
    val designWorkFiles = getSet.designDB.designMemberList.view.map(_._1)
      // Foreign IP wrappers are external VHDL whose architecture name DFHDL does not control (the
      // vga-monitor wrapper uses `rtl`, not the `<name>_arch` DFHDL emits for its own designs), so
      // the predicted `WORK.<NAME>-<NAME>_ARCH` file never exists and `cacheFiles` would fail trying
      // to cache it. They are not needed as cached intermediates anyway: NVC runs from the
      // elaborated `.elab`, which already embeds the foreign design unit.
      .filterNot(_.isForeignIPBlackbox)
      .map(_.dclName)
      .flatMap(name =>
        val nameUC = name.toUpperCase()
        List(s"WORK.${nameUC}", s"WORK.${nameUC}-${nameUC}_ARCH")
      ).toList
    val topNameUC = topName.toUpperCase()
    // TODO: this is kind of a hack. We assume if this number is 2 then
    // we have a global def, and not just the DFHDL package
    val globalDefCount = getSet.designDB.srcFiles.count { src =>
      src.sourceType match
        case SourceType.GlobalDef => true
        case _                    => false
    }
    val dsnPackageWorkFiles = List(
      "WORK.DFHDL_PKG",
      "WORK.DFHDL_PKG-body"
    ) ++ (
      if (globalDefCount >= 1)
        List(
          s"WORK.${topNameUC}_PKG",
          s"WORK.${topNameUC}_PKG-body"
        )
      else Nil
    )
    val versionDouble = installedVersionDouble
    val topElabFile =
      if (versionDouble >= 1.20) ""
      else if (versionDouble >= 1.17) s"_WORK.${topNameUC}.elab.pack"
      else s"_WORK.${topNameUC}.pack"
    val topWorkFiles =
      if (topElabFile.nonEmpty) List(s"WORK.${topNameUC}.elab", topElabFile)
      else List(s"WORK.${topNameUC}.elab")
    val extraFiles = List("_index", "_NVC_LIB")
    val allFiles = extraFiles ++ topWorkFiles ++ dsnPackageWorkFiles ++ designWorkFiles
    allFiles.map(name => s"work${separatorChar}${name}")
  end producedFiles

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    var insideWarning = false
    // Create a process logger to suppress the shared variable warning
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("** Warning:"),
        lineIsSuppressed = (line: String) =>
          if (line.matches("\\*\\* Warning: shared variable .* must have protected type"))
            // Start suppressing lines
            insideWarning = true
            true
          else if (insideWarning)
            // hit the end of the warning
            if (line.trim.endsWith("^")) insideWarning = false
            true
          else lineIsForeignWorkDirWarning(line)
      )
    )
  end lintLogger

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    val inVHDL93 =
      summon[CompilerOptions].backend.asInstanceOf[backends.vhdl].dialect == VHDLDialect.v93
    Some(
      new Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.contains("** Warning:"),
        lineIsSuppressed = (line: String) =>
          // VHDL'93 does not have a standard finish, so we detect the DFHDL generated
          // fatal report and convert it to the same behavior as in VHDL'2008 and later in NVC
          if (inVHDL93)
            if (line.endsWith(": Finished successfully (not an error)"))
              // Extract the time from the line.
              val timePattern = """\*\* Failure\: (\d+\w+)""".r
              val time = timePattern.findFirstMatchIn(line).map(_.group(1)).get
              println(s"** Note: $time+1: FINISH called")
              true
            else false
          else false,
        lineIsErrorOpt =
          if (inVHDL93)
            Some((line: String) => line.contains("** Error:") || line.contains("** Failure:"))
          else None
      )
    )
  end simulateLogger

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-a",
    "--relaxed"
  )

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "-r",
    // Foreign IP VHPIDIRECT integration: load each IP's VHPI shared library at run time.
    constructCommand(
      foreignSources.filter(_.vhpiLib.nonEmpty).map { f =>
        s"--load ${foreignLibDir(f)}/${foreignSharedLibFile(f.vhpiLib)}"
      }*
    ),
    topName,
    "--ieee-warnings=off"
  )

  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    lintCmdLanguageFlag(dialect)

end NVCVHDL

object NVCVerilog extends NVCCommon, VerilogLinter, VerilogSimulator:
  protected def includeFolderFlag: String = "-I"

  // NVC's Verilog frontend covers plain Verilog; the SystemVerilog constructs DFHDL emits for the
  // sv dialects (size casts, unpacked-array typedefs) are not supported yet.
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    val keywords = dialect match
      case VerilogDialect.v95   => "1364-1995"
      case VerilogDialect.v2001 => "1364-2001"
      case _                    =>
        throw new java.lang.IllegalArgumentException(
          "Current dialect is not supported for NVC Verilog linting."
        )
    s"--keywords=$keywords"

  // `--keywords` is an analysis option, so `-a` must precede the language flag (unlike the VHDL
  // front-end's `--std`, which is a global option placed before `-a`).
  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = "-a"

  // Verilog analysis matured in NVC 1.22; older versions cannot parse the generated files.
  // Note: released NVC versions (through 1.22) still reject a block comment inside a `define
  // macro body, which dfhdl_defs.vh uses for its Verilator lint pragmas, so linting DFHDL
  // output requires an NVC build that fixes https://github.com/nickg/nvc/issues/1636 (once a
  // release carries the fix, raise this version floor to it).
  override protected def lintPrepare()(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    if (installedVersionDouble < 1.22)
      error(
        s"NVC version 1.22 or later is required for Verilog support, but version ${getInstalledVersion} was found."
      )

  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      so: SimulatorOptions
  ): List[String] =
    // A Verilog module is a single library unit (no VHDL-style secondary architecture unit).
    // Foreign IP wrappers are excluded like in the VHDL front-end (NVC runs from the elaborated
    // `.elab`, which already embeds them).
    val designWorkFiles = getSet.designDB.designMemberList.view.map(_._1)
      .filterNot(_.isForeignIPBlackbox)
      .map(design => s"WORK.${design.dclName.toUpperCase()}")
      .toList
    val topWorkFiles = List(s"WORK.${topName.toUpperCase()}.elab")
    val extraFiles = List("_index", "_NVC_LIB")
    val allFiles = extraFiles ++ topWorkFiles ++ designWorkFiles
    allFiles.map(name => s"work${separatorChar}${name}")
  end producedFiles

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("** Warning:"),
        lineIsSuppressed = lineIsForeignWorkDirWarning
      )
    )

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.contains("** Warning:"),
        lineIsSuppressed = (line: String) => false
      )
    )

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "-r",
    topName
  )

  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String = ""

end NVCVerilog
