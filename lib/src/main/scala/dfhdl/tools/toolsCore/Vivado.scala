package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, BuilderOptions, ToolOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths

object Vivado extends Builder:
  val toolName: String = "Vivado"
  protected def binExec: String = "vivado"
  override protected def windowsBinExec: String = "vivado.bat"
  protected def versionCmd: String = s"-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Vivado\s+v(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def filesCmdPart[D <: Design](cd: CompiledDesign[D]): String = ???
  override protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign[D] =
    addSourceFiles(
      cd,
      List(new VivadoProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile)
    )
  def build[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, BuilderOptions): CompiledDesign[D] =
    exec(
      cd,
      s"-mode batch -source ${cd.stagedDB.top.dclName}.tcl"
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
