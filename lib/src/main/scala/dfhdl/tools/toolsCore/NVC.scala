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
import java.io.File.separatorChar
import scala.sys.process.*

object NVC extends VHDLLinter, VHDLSimulator:
  override val simRunsLint: Boolean = true
  val toolName: String = "NVC"
  protected def binExec: String = "nvc"
  protected def versionCmd: String = s"--version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """nvc\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

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
      .filterNot(_.isDuplicate).map(_.dclName)
      .flatMap(name =>
        val nameUC = name.toUpperCase()
        List(s"WORK.${nameUC}", s"WORK.${nameUC}-${nameUC}_ARCH")
      ).toList
    val topNameUC = topName.toUpperCase()
    val dsnPackageWorkFiles = List(
      "WORK.DFHDL_PKG",
      "WORK.DFHDL_PKG-body",
      s"WORK.${topNameUC}_PKG",
      s"WORK.${topNameUC}_PKG-body"
    )
    val versionDouble = this.installedVersion.get.split("\\.").take(2).mkString(".").toDouble
    val topElabFile =
      if (versionDouble >= 1.17) s"_WORK.${topNameUC}.elab.pack"
      else s"_WORK.${topNameUC}.pack"
    val topWorkFiles = List(s"WORK.${topNameUC}.elab", topElabFile)
    val extraFiles = List("_index", "_NVC_LIB")
    val allFiles = extraFiles ++ topWorkFiles ++ dsnPackageWorkFiles ++ designWorkFiles
    allFiles.map(name => s"work${separatorChar}${name}")
  end producedFiles

  override protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    val ret = super.simulatePreprocess(cd)
    given MemberGetSet = ret.stagedDB.getSet
    exec(constructCommand("-e", topName))
    ret

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
          // this is expected when mixing multiple simulators/linters all using "work" folder
          else if (line == "** Warning: directory work already exists and is not an NVC library")
            true
          else false
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
    topName,
    "--ieee-warnings=off"
  )

  override protected def simulateCmdLanguageFlag(dialect: VHDLDialect): String =
    lintCmdLanguageFlag(dialect)

end NVC
