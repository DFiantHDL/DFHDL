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

object NVC extends VHDLLinter:
  val toolName: String = "NVC"
  protected def binExec: String = "nvc"
  protected def versionCmd: String = s"--version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """nvc\s+(\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def filesCmdPart[D <: Design](cd: CompiledDesign[D]): String =
    val designsInCmd = cd.stagedDB.srcFiles.view.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.Regular | SourceType.Design.BlackBox,
            path,
            _
          ) =>
        path
    }.mkString(" ")

    val dfhdlPackage = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.DFHDLDef, path, _) =>
        path
    }.get

    val globalPackage = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        path
    }.get

    // config files must be placed before the design sources
    s"$dfhdlPackage $globalPackage $designsInCmd"
  end filesCmdPart
  def lint[D <: Design](
      cd: CompiledDesign[D]
  )(using co: CompilerOptions, lo: LinterOptions): CompiledDesign[D] =
    val std = co.backend match
      case be: backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93   => "93"
          case VHDLDialect.v2008 => "08"
          case VHDLDialect.v2019 => "19"
      case _ =>
        throw new java.lang.IllegalArgumentException(
          "Current backend is not supported for NVC linting."
        )
    exec(
      cd,
      s"--std=$std -a --relaxed ${filesCmdPart(cd)}"
    )
  end lint
end NVC
