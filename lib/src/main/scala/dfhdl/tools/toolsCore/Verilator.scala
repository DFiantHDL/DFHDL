package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*

object Verilator extends Linter:
  def binExec: String = "verilator_bin"
  def commonFlags: String = "-Wall"
  def filesCmdPart[D <: Design](cd: CommittedDesign[D]): String =

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
  override def preprocess[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D] = ???
  def lint[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D] =
    exec(
      cd,
      s"$binExec --lint-only $commonFlags ${filesCmdPart(cd)}"
    )
  end lint
end Verilator

class VerilatorConfigPrinter(using getSet: MemberGetSet):
  val designDB: DB = getSet.designDB
  def configFileName: String = s"${designDB.top.dclName}.vlt"
  def contents: String =
    s"""`verilator_config
       |$commands
       |""".stripMargin
  def commands: String = ???
  def lintOffCommand(rule: String = "", file: String = "", lines: String = ""): String =
    s"lint_off${rule.emptyOr(" -rule " + _)}${file.emptyOr(" -file " + _)}${lines.emptyOr(" -lines " + _)}"
  def lintOffBlackBoxes: String = ???
