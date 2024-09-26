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

object IcarusVerilog extends VerilogLinter:
  val toolName: String = "Icarus Verilog"
  protected def binExec: String = "iverilog"
  protected def versionCmd: String = "-V"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Icarus Verilog version\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def commonFlags(using co: CompilerOptions, lo: LinterOptions): String =
    val generation = co.backend match
      case be: backends.verilog =>
        be.dialect match
          case VerilogDialect.v95    => "1995"
          case VerilogDialect.v2001  => "2001"
          case VerilogDialect.sv2005 => "2005"
          case VerilogDialect.sv2009 => "2009"
          case VerilogDialect.sv2012 => "2012"
          case _ =>
            throw new java.lang.IllegalArgumentException(
              "Current dialect is not supported for Icarus Verilog linting."
            )
      case _ =>
        throw new java.lang.IllegalArgumentException(
          "Current backend is not supported for Icarus Verilog linting."
        )
    s"-g$generation"
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
      List(dfhdlDefsIncludeFolder, globalIncludeFolder).distinct.map(i => s"-I$i").mkString(" ")

    s"$includes $designsInCmd"
  end filesCmdPart
  def lint[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, LinterOptions): CompiledDesign[D] =
    exec(
      cd,
      s"$commonFlags -o ${cd.stagedDB.top.dclName} -Wall ${filesCmdPart(cd)}"
    )
  end lint
end IcarusVerilog
