package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import scala.sys.process.*

trait VivadoSimCommon extends Linter, Simulator:
  override val simRunsLint: Boolean = true
  final val toolName: String = s"Vivado Simulator $binExec"
  final protected def versionCmd: String = "-version"
  final override protected def windowsBinExec: String = s"$binExec.bat"
  final protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Vivado Simulator\s+v(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))
  protected def suppressLine(line: String): Boolean = line.startsWith("INFO:")

  final override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] = Some(
    Tool.ProcessLogger(
      lineIsWarning = (line: String) => line.startsWith("WARNING:"),
      lineIsSuppressed = (line: String) => suppressLine(line)
    )
  )

  protected def xsimFolder(using MemberGetSet): String =
    s"xsim.dir${separatorChar}work.${topName}"
  private val workFolder = s"xsim.dir${separatorChar}work"

  // TODO: check if the script is indeed shell script
  val axsimScript = if (osIsWindows) "axsim.bat" else "axsim.sh"

  // must clean up work folder when switching between verilog and vhdl backends
  private def cleanUp()(using
      getSet: MemberGetSet,
      co: CompilerOptions
  ): Unit =
    import java.nio.file.{Files, Paths, DirectoryStream}
    import scala.jdk.CollectionConverters._

    val dir = Paths.get(co.topCommitPath(topName)).resolve(workFolder)
    if (Files.exists(dir) && Files.isDirectory(dir))
      val removePattern = co.backend match
        case _: backends.verilog => "*.vdb"
        case _: backends.vhdl    => "*.sdb"
      if (removePattern.nonEmpty)
        val stream: DirectoryStream[java.nio.file.Path] =
          java.nio.file.Files.newDirectoryStream(dir, removePattern)
        try
          for (file <- stream.asScala)
            try Files.deleteIfExists(file)
            catch case _: Throwable => ()
        finally
          stream.close()
  end cleanUp

  override protected[dfhdl] def cleanUpBeforeFileRestore()(using
      getSet: MemberGetSet,
      co: CompilerOptions
  ): Unit = cleanUp()

  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      so: SimulatorOptions
  ): List[String] =
    val folder = xsimFolder
    val axsimRunExec = s"${folder}${separatorChar}axsim${if (osIsWindows) ".exe" else ""}"
    val xsimSuffixes = List("dbg", "mem", "reloc", "rtti", "svtype", "type", "xdbg")
    val xsimSupportFiles =
      xsimSuffixes.map(suffix => s"${folder}${separatorChar}xsim.${suffix}")
    def compiledFilePath(dclName: String): String =
      val name = co.backend match
        case _: backends.vhdl    => s"${dclName.toLowerCase()}.vdb"
        case _: backends.verilog =>
          dclName.flatMap { c =>
            if (c.isUpper) s"@${c.toLower}" else s"$c"
          } + ".sdb"
      s"${workFolder}${separatorChar}${name}"
    def compiledVHDLPackageFiles: List[String] =
      co.backend match
        case _: backends.vhdl =>
          this.designDefFiles.map { path =>
            val fileName = path.split(separatorChar).last.toLowerCase()
            s"${workFolder}${separatorChar}${fileName.replace(".vhd", ".vdb")}"
          }
        case _: backends.verilog => Nil
    val compiledFiles =
      s"$workFolder${separatorChar}work.rlx" :: compiledVHDLPackageFiles ++
        getSet.designDB.uniqueDesignMemberList.view.map(_._1.dclName).map(compiledFilePath).toList
    axsimScript :: axsimRunExec :: xsimSupportFiles ++ compiledFiles
  end producedFiles

  override protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign =
    cleanUp()(using cd.stagedDB.getSet)
    val ret = super.simulatePreprocess(cd)
    given MemberGetSet = ret.stagedDB.getSet
    val runExec = if (osIsWindows) "xelab.bat" else "xelab"
    val inVHDL93 = co.backend match
      case be: backends.vhdl => be.dialect == VHDLDialect.v93
      case _                 => false
    val cmd = constructCommand(
      "-a",
      topName,
      if (inVHDL93) "--93_mode" else ""
    )
    exec(cmd = cmd, runExec = runExec)
    ret
  end simulatePreprocess

  override protected def simRunExec(using MemberGetSet): String = s".${separatorChar}${axsimScript}"
end VivadoSimCommon

object VivadoSimVerilog extends VivadoSimCommon, VerilogLinter, VerilogSimulator:
  protected def binExec: String = "xvlog"
  protected def includeFolderFlag: String = "-i "
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => ""
      case _                                         => "--sv"
  // suppress info messages and initial value omission
  override protected def suppressLine(line: String): Boolean =
    super.suppressLine(line) || line.matches("WARNING: \\[VRFC 10\\-3467\\].*")
  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    ""
  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning =
          (line: String) => line.startsWith("Warning:") || line.startsWith("WARNING:"),
        lineIsSuppressed = (line: String) => false,
        lineIsErrorOpt =
          Some((line: String) => line.startsWith("Error:") || line.startsWith("Fatal:"))
      )
    )
end VivadoSimVerilog

object VivadoSimVHDL extends VivadoSimCommon, VHDLLinter, VHDLSimulator:
  protected def binExec: String = "xvhdl"
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    dialect match
      case VHDLDialect.v93   => "--93_mode"
      case VHDLDialect.v2008 => "--2008"
      case VHDLDialect.v2019 => "--2019"
  // suppress info messages and shared variable warnings
  override protected def suppressLine(line: String): Boolean =
    super.suppressLine(line) || line.matches("WARNING: \\[VRFC 10\\-2115\\].*")
  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    ""
  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    val inVHDL93 =
      summon[CompilerOptions].backend.asInstanceOf[backends.vhdl].dialect == VHDLDialect.v93
    Some(
      new Tool.ProcessLogger(
        lineIsWarning =
          (line: String) => line.startsWith("Warning:") || line.startsWith("WARNING:"),
        lineIsSuppressed = (line: String) =>
          // VHDL'93 does not have a standard finish, so we detect the DFHDL generated
          // fatal report and convert it to the same behavior as in VHDL'2008 and later in Vivado
          if (inVHDL93)
            if (line.endsWith(": Finished successfully (not an error)")) true
            else false
          else false,
        lineIsErrorOpt =
          Some((line: String) => line.startsWith("Error:") || line.startsWith("Failure:"))
      )
    )
  end simulateLogger

end VivadoSimVHDL
