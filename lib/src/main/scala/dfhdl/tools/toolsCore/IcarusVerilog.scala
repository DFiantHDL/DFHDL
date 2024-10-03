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

  protected def lintIncludeFolderFlag: String = "-I"

  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    val generation = dialect match
      case VerilogDialect.v95    => "1995"
      case VerilogDialect.v2001  => "2001"
      case VerilogDialect.sv2005 => "2005"
      case VerilogDialect.sv2009 => "2009"
      case VerilogDialect.sv2012 => "2012"
      case _ =>
        throw new java.lang.IllegalArgumentException(
          "Current dialect is not supported for Icarus Verilog linting."
        )
    s"-g$generation"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    s"-o $topName"
  )

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-Wall"
  )
end IcarusVerilog
