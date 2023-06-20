package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.{CommittedDesign, CompiledDesign}
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CommitOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths

object Vivado extends Builder:
  def binExec: String = "vivado"
  def filesCmdPart[D <: Design](cd: CommittedDesign[D]): String = ???
  override protected[dfhdl] def preprocess[D <: Design](cd: CommittedDesign[D])(using
      CommitOptions
  ): CommittedDesign[D] =
    addSourceFiles(
      cd,
      List(new VivadoProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile)
    )
  def build[D <: Design](cd: CommittedDesign[D])(using CommitOptions): CommittedDesign[D] =
    exec(
      cd,
      s"$binExec -mode batch -source ${cd.stagedDB.top.dclName}.tcl"
    )
end Vivado

case object VivadoProjectTclConfig extends SourceType.ToolConfig

class VivadoProjectTclConfigPrinter(using getSet: MemberGetSet):
  val designDB: DB = getSet.designDB
  def configFileName: String = s"${designDB.top.dclName}.tcl"
  def contents: String =
    s"""
       |
       |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectTclConfig, configFileName, contents)
end VivadoProjectTclConfigPrinter
