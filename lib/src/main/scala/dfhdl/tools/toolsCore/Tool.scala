package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.options.CompilerOptions
import dfhdl.options.ToolOptions
import dfhdl.options.LinterOptions
import dfhdl.options.BuilderOptions
import dfhdl.options.OnError
import java.io.IOException
import scala.sys.process.*
import dfhdl.internals.*
import java.nio.file.Paths
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect

trait Tool:
  val toolName: String
  private def runExec: String =
    val osName: String = sys.props("os.name").toLowerCase
    if (osName.contains("windows")) windowsBinExec else binExec
  protected def binExec: String
  protected def windowsBinExec: String = s"$binExec.exe"
  protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign[D] = cd
  final protected def addSourceFiles[D <: Design](
      cd: CompiledDesign[D],
      sourceFiles: List[SourceFile]
  )(using CompilerOptions): CompiledDesign[D] =
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
    if (preCheckDone) {} else
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
            SourceType.Design.Regular | SourceType.Design.BlackBox,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def toolFiles(using getSet: MemberGetSet): List[String] = Nil

  protected def designDefFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.DFHDLDef | SourceType.Design.GlobalDef,
            path,
            _
          ) =>
        path.convertWindowsToLinuxPaths
    }

  protected def designDefFolders(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.DFHDLDef | SourceType.Design.GlobalDef,
            path,
            _
          ) =>
        Paths.get(path).getParent.toString.convertWindowsToLinuxPaths
    }.distinct

  protected def constructCommand(args: String*): String =
    args.filter(_.nonEmpty).mkString(" ")

  final protected def exec[D <: Design](
      cmd: String,
      prepare: => Unit = (),
      logger: Option[ProcessLogger] = None
  )(using CompilerOptions, ToolOptions, MemberGetSet): Unit =
    preCheck()
    prepare
    val fullExec = s"$runExec $cmd"
    val process = Process(fullExec, new java.io.File(execPath))
    val errCode = logger.map(process.!).getOrElse(process.!)
    if (errCode != 0)
      error(
        s"${toolName} exited with the error code ${errCode} while attempting to run:\n$fullExec"
      )
  end exec
end Tool

trait Linter extends Tool:
  final def lint[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, LinterOptions): CompiledDesign[D] =
    given MemberGetSet = cd.stagedDB.getSet
    exec(lintCmdFlags, lintPrepare(), lintLogger)
    cd
  protected def lintPrepare()(using CompilerOptions, LinterOptions, MemberGetSet): Unit = {}
  protected def lintLogger(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): Option[ProcessLogger] = None
  protected def lintCmdLanguageFlag(using co: CompilerOptions): String
  protected def lintCmdSources(using CompilerOptions, LinterOptions, MemberGetSet): String
  protected def lintCmdPreLangFlags(using CompilerOptions, LinterOptions, MemberGetSet): String = ""
  protected def lintCmdPostLangFlags(using CompilerOptions, LinterOptions, MemberGetSet): String =
    ""
  final protected def lintCmdFlags(using CompilerOptions, LinterOptions, MemberGetSet): String =
    constructCommand(lintCmdPreLangFlags, lintCmdLanguageFlag, lintCmdPostLangFlags, lintCmdSources)
end Linter
trait VerilogLinter extends Linter:
  // Converts the selected compiler verilog dialect to the relevant lint flag
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String
  final protected def lintCmdLanguageFlag(using co: CompilerOptions): String =
    lintCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.verilog].dialect)
  // The include flag to be attached before each included folder
  protected def lintIncludeFolderFlag: String
  final protected def lintCmdSources(using CompilerOptions, LinterOptions, MemberGetSet): String =
    (designDefFolders.map(lintIncludeFolderFlag + _) ++ toolFiles ++ designFiles).mkString(" ")

trait VHDLLinter extends Linter:
  // Converts the selected compiler vhdl dialect to the relevant lint flag
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String
  final protected def lintCmdSources(using CompilerOptions, LinterOptions, MemberGetSet): String =
    (designDefFiles ++ toolFiles ++ designFiles).mkString(" ")
  final protected def lintCmdLanguageFlag(using co: CompilerOptions): String =
    lintCmdLanguageFlag(co.backend.asInstanceOf[dfhdl.backends.vhdl].dialect)

trait Builder extends Tool:
  def build[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, BuilderOptions): CompiledDesign[D]
object Builder:
  // default linter will be vivado
  given Builder = dfhdl.tools.builders.vivado
