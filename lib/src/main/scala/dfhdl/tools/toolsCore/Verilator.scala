package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.compiler.ir

object Verilator extends Linter:
  def binExec: String = "verilator_bin"
  def commonFlags: String = "-Wall"
  def filesCmdPart[D <: Design](cd: CommittedDesign[D]): String =
    import ir.{SourceFile, SourceType}

    val filePaths = cd.staged.stagedDB.srcFiles.collect {
      case SourceFile(SourceType.Committed, path, _) =>
        path
    }
    // We drop the global definition file (it is included)
    // We translate the windows `\` to unix `/` to fit the verilator needs
    val filesInCmd = filePaths.drop(1).mkString(" ").replaceAll("""\\""", "/")
    // Global include:
    val globalInclude =
      java.nio.file.Paths.get(filePaths.head).getParent.toString.replaceAll("""\\""", "/")
    s"-I${globalInclude} ${filesInCmd}"

  def lint[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D] =
    exec(
      cd,
      s"$binExec --lint-only $commonFlags ${filesCmdPart(cd)}"
    )
  end lint
end Verilator
