package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.SourceFile
import dfhdl.options.CompilerOptions

trait Tool:
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
      co: CompilerOptions
  ): CompiledDesign[D] =
    import scala.sys.process.*
    val pwd = new java.io.File(co.topCommitPath(cd.stagedDB))
    val errCode = Process(cmd, pwd).!
    if (errCode != 0)
      sys.exit(errCode)
    cd
end Tool

trait Linter extends Tool:
  def lint[D <: Design](cd: CompiledDesign[D])(using CompilerOptions): CompiledDesign[D]
trait VerilogLinter extends Linter
object VerilogLinter:
  // default verilog linter will be verilator
  given VerilogLinter = dfhdl.tools.linters.verilator
trait VHDLLinter extends Linter
object VHDLLinter:
  // default vhdl linter will be ghdl
  given VHDLLinter = dfhdl.tools.linters.ghdl

trait Builder extends Tool:
  def build[D <: Design](cd: CompiledDesign[D])(using CompilerOptions): CompiledDesign[D]
object Builder:
  // default linter will be vivado
  given Builder = dfhdl.tools.builders.vivado
