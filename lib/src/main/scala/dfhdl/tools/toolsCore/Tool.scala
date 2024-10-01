package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.SourceFile
import dfhdl.options.CompilerOptions
import dfhdl.options.ToolOptions
import dfhdl.options.LinterOptions
import dfhdl.options.BuilderOptions
import dfhdl.options.OnError
import java.io.IOException

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
    import scala.sys.process.*
    val getVersionFullCmd = s"$runExec $versionCmd"
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

  final protected def exec[D <: Design](cd: CompiledDesign[D], cmd: String)(using
      co: CompilerOptions,
      to: ToolOptions
  ): CompiledDesign[D] =
    import scala.sys.process.*
    preCheck()
    val pwd = new java.io.File(co.topCommitPath(cd.stagedDB))
    val fullExec = s"$runExec $cmd"
    val errCode = Process(fullExec, pwd).!
    if (errCode != 0)
      error(s"${toolName} exited with the error code ${errCode} attempting to run:\n$fullExec")
    cd
  end exec
end Tool

trait Linter extends Tool:
  def lint[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, LinterOptions): CompiledDesign[D]
trait VerilogLinter extends Linter

trait VHDLLinter extends Linter

trait Builder extends Tool:
  def build[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, BuilderOptions): CompiledDesign[D]
object Builder:
  // default linter will be vivado
  given Builder = dfhdl.tools.builders.vivado
