package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*

object Verilator extends Linter:
  def binExec: String = "verilator_bin"
  def commonFlags: String = "-Wall"
  def filesCmdPart[D <: Design](cd: CommittedDesign[D]): String =

    val filesInCmd = cd.stagedDB.srcFiles.view.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.Regular | SourceType.Design.BlackBox,
            path,
            _
          ) =>
        path.forceWindowsToLinuxPath
    }.mkString(" ")
    // We drop the global definition file (it is included)
    // We translate the windows `\` to unix `/` to fit the verilator needs
    val globalIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        java.nio.file.Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    s"-I${globalIncludeFolder} ${filesInCmd}"
  end filesCmdPart
  override def preprocess[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D] = cd
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
