package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, BuilderOptions, ToolOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import dfhdl.backends
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect

object Vivado extends Builder:
  val toolName: String = "Vivado"
  protected def binExec: String = "vivado"
  override protected def windowsBinExec: String = "vivado.bat"
  protected def versionCmd: String = s"-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """vivado\s+v(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(new VivadoProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile)
    )
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(
      s"-mode batch -source ${topName}.tcl"
    )
    cd
end Vivado

val VivadoProjectTclConfig = SourceType.Tool("Vivado", "ProjectTclConfig")

class VivadoProjectTclConfigPrinter(using getSet: MemberGetSet, co: CompilerOptions):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val targetLanguage: String = co.backend match
    case _: backends.verilog => "Verilog"
    case _: backends.vhdl    => "VHDL"
  val part: String = "xc7a100tcsg324-1" // TODO: make it configurable
  val fileType: String = co.backend match
    case backend: backends.verilog => backend.dialect match
        case VerilogDialect.v95 | VerilogDialect.v2001 => "Verilog"
        case _                                         => "SystemVerilog"
    case backend: backends.vhdl => backend.dialect match
        case VHDLDialect.v93   => "VHDL"
        case VHDLDialect.v2008 => "VHDL 2008"
        case VHDLDialect.v2019 => "VHDL 2019"
  val hdlFiles: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.Design | SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) =>
      path.forceWindowsToLinuxPath
  }
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    s"""|create_project $topName . -part $part -force
        |set_property target_language $targetLanguage [current_project]
        |add_files -norecurse ${hdlFiles.mkString("{\n  ", "\n  ", "\n}")}
        |set_property file_type {${fileType}} [get_files  *]
        |######################################################################
        |# Suppress warnings
        |######################################################################
        |# the warning "Parallel synthesis criteria is not met" should not be a warning
        |set_msg_config -id {Synth 8-7080} -new_severity {INFO}
        |# redundant warning that default value for parameter is not defined
        |set_msg_config -suppress -id {Synth 8-9661} 
        |# bug in 2024.1
        |if {[version -short] == "2024.1"} {
        |    set_msg_config -suppress -id {Device 21-9320} -string {{WARNING: [Device 21-9320] Failed to find the Oracle tile group with name 'HSR_BOUNDARY_TOP'. This is required for Clock regions and Virtual grid.}} 
        |    set_msg_config -suppress -id {Device 21-2174} -string {{WARNING: [Device 21-2174] Failed to initialize Virtual grid.}} 
        |}
        |######################################################################
        |launch_runs impl_1
        |wait_on_run impl_1
        |open_run impl_1
        |write_bitstream -file ./$topName.bit -force
        |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectTclConfig, configFileName, contents)
end VivadoProjectTclConfigPrinter
