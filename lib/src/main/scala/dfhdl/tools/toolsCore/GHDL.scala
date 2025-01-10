package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, LinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar

object GHDL extends VHDLLinter:
  val toolName: String = "GHDL"
  protected def binExec: String = "ghdl"
  protected def versionCmd: String = s"version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """GHDL\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  protected def lintCmdLanguageFlag(dialect: VHDLDialect): String =
    val std = dialect match
      case VHDLDialect.v93   => "93"
      case VHDLDialect.v2008 => "08"
      case VHDLDialect.v2019 => "19"
    s"--std=$std"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-a",
    summon[LinterOptions].fatalWarnings.toFlag("--warn-error")
  )

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-frelaxed",
    "-Wno-shared"
  )
end GHDL
