package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.compiler.ir.SourceFile
import dfhdl.options.CommitOptions

trait Tool:
  protected[dfhdl] def preprocess[D <: Design](cd: CommittedDesign[D])(using
      CommitOptions
  ): CommittedDesign[D] = cd
  final protected def addSourceFiles[D <: Design](
      cd: CommittedDesign[D],
      sourceFiles: List[SourceFile]
  )(using CommitOptions): CommittedDesign[D] =
    val stagedDB = cd.stagedDB
    cd.newStage(stagedDB.copy(srcFiles = stagedDB.srcFiles ++ sourceFiles)).commit

  final protected def exec[D <: Design](cd: CommittedDesign[D], cmd: String)(using
      CommitOptions
  ): CommittedDesign[D] =
    import scala.sys.process.*
    Process(cmd).!
    cd
end Tool

trait Linter extends Tool:
  def lint[D <: Design](cd: CommittedDesign[D])(using CommitOptions): CommittedDesign[D]
object Linter:
  // default linter will be verilator
  given Linter = dfhdl.tools.linters.verilator
