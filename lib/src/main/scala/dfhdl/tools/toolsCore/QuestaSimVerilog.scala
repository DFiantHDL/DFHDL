package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect

object QuestaSimVerilog extends VerilogLinter:
  val toolName: String = "QuestaSim vlog"
  protected def binExec: String = "vlog"
  protected def versionCmd: String = "-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """.*vlog\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def commonFlags(using co: CompilerOptions, lo: LinterOptions): String =
    val language = (co.backend: @unchecked) match
      case be: backends.verilog =>
        be.dialect match
          case VerilogDialect.v95    => "-vlog95compat"
          case VerilogDialect.v2001  => "-vlog01compat"
          case VerilogDialect.sv2005 => "-sv05compat"
          case VerilogDialect.sv2009 => "-sv09compat"
          case VerilogDialect.sv2012 => "-sv12compat"
          case VerilogDialect.sv2017 => "-sv17compat"
    s"-quiet -suppress 2892 -warning error $language"
  end commonFlags
  def filesCmdPart[D <: Design](cd: CompiledDesign[D]): String =
    // We use `forceWindowsToLinuxPath` fit the verilator needs
    val designsInCmd = cd.stagedDB.srcFiles.view.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.Regular | SourceType.Design.BlackBox,
            path,
            _
          ) =>
        path.forceWindowsToLinuxPath
    }.mkString(" ")

    val dfhdlDefsIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.DFHDLDef, path, _) =>
        Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    val globalIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    val includes =
      List(dfhdlDefsIncludeFolder, globalIncludeFolder).distinct.map(i => s"+incdir+$i").mkString(
        " "
      )

    s"$includes $designsInCmd"
  end filesCmdPart
  def lint[D <: Design](
      cd: CompiledDesign[D]
  )(using co: CompilerOptions, lo: LinterOptions): CompiledDesign[D] =
    import scala.sys.process.*
    val pwd = co.topCommitPath(cd.stagedDB)
    val work = new java.io.File(s"${pwd}${separatorChar}work${separatorChar}_info")
    // creating a questa sim work lib if the work/_info file is missing
    if (!work.exists())
      Process("vlib work", new java.io.File(pwd)).!
    exec(
      cd,
      s"$commonFlags ${filesCmdPart(cd)}"
    )
  end lint
end QuestaSimVerilog
