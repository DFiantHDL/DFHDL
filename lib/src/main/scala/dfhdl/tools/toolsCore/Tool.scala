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
  protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions
  ): CompiledDesign[D] = cd
  final protected def addSourceFiles[D <: Design](
      cd: CompiledDesign[D],
      sourceFiles: List[SourceFile]
  )(using CompilerOptions): CompiledDesign[D] =
    val stagedDB = cd.stagedDB
    cd.newStage(stagedDB.copy(srcFiles = stagedDB.srcFiles ++ sourceFiles)).commit

  final protected def exec[D <: Design](cd: CompiledDesign[D], cmd: String)(using
      co: CompilerOptions,
      to: ToolOptions
  ): CompiledDesign[D] =
    import scala.sys.process.*
    val pwd = new java.io.File(co.topCommitPath(cd.stagedDB))
    val errCode = Process(cmd, pwd).!
    if (errCode != 0)
      val msg = s"${toolName} exited with the error code ${errCode}."
      to.onError match
        case OnError.Exit =>
          println("msg")
          sys.exit(errCode)
        case OnError.Exception => sys.error(msg)
    cd
  end exec
end Tool

trait Linter extends Tool:
  type LO <: LinterOptions
  def lint[D <: Design](cd: CompiledDesign[D])(using CompilerOptions, LO): CompiledDesign[D]
trait VerilogLinterOptions extends LinterOptions
trait VerilogLinter extends Linter:
  type LO <: VerilogLinterOptions
object VerilogLinter:
  // default verilog linter will be verilator
  export dfhdl.tools.linters.verilator
trait VHDLLinterOptions extends LinterOptions
trait VHDLLinter extends Linter:
  type LO <: VHDLLinterOptions
object VHDLLinter:
  // default vhdl linter will be ghdl
  export dfhdl.tools.linters.ghdl

trait Builder extends Tool:
  type BO <: BuilderOptions
  def build[D <: Design](cd: CompiledDesign[D])(using CompilerOptions, BO): CompiledDesign[D]
object Builder:
  // default linter will be vivado
  given Builder = dfhdl.tools.builders.vivado
