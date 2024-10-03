package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import java.io.FileWriter
import java.io.File.separatorChar
import scala.sys.process.*

trait QuestaSimCommon extends Linter:
  final val toolName: String = s"QuestaSim $binExec"
  final protected def versionCmd: String = "-version"
  final protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = s""".*$binExec\\s+(\\d+\\.\\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-quiet",
    summon[LinterOptions].fatalWarnings.toFlag("-warning error")
  )

  // creating a questa sim work lib if the work/_info file is missing
  final override protected def lintPrepare()(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): Unit =
    val work = new java.io.File(s"${execPath}${separatorChar}work${separatorChar}_info")
    if (!work.exists())
      Process("vlib work", new java.io.File(execPath)).!
end QuestaSimCommon

object QuestaSimVerilog extends QuestaSimCommon, VerilogLinter:
  protected def binExec: String = "vlog"
  protected def lintIncludeFolderFlag: String = "+incdir+"
  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    dialect match
      case VerilogDialect.v95    => "-vlog95compat"
      case VerilogDialect.v2001  => "-vlog01compat"
      case VerilogDialect.sv2005 => "-sv05compat"
      case VerilogDialect.sv2009 => "-sv09compat"
      case VerilogDialect.sv2012 => "-sv12compat"
      case VerilogDialect.sv2017 => "-sv17compat"
end QuestaSimVerilog

object QuestaSimVHDL extends QuestaSimCommon, VHDLLinter:
  protected def binExec: String = "vcom"
  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    dialect match
      case VHDLDialect.v93   => "-93"
      case VHDLDialect.v2008 => "-2008"
      case VHDLDialect.v2019 => "-2019"
  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-pedantic",
    // suppressing shared variable warnings
    "-suppress 1236"
  )
end QuestaSimVHDL
