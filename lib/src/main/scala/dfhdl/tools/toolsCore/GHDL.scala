package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar

object GHDL extends VHDLLinter:
  def binExec: String = "ghdl"

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

    val globalPackage = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        path
    }.get

    // config files must be placed before the design sources
    s"$globalPackage $designsInCmd"
  end filesCmdPart
  def lint[D <: Design](cd: CompiledDesign[D])(using co: CompilerOptions): CompiledDesign[D] =
    val std = co.backend match
      case be: backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93   => "93"
          case VHDLDialect.v2008 => "08"
          case VHDLDialect.v2019 => "08" // uses 2008 standard until GHDL supports this dialect
      case _ =>
        throw new java.lang.IllegalArgumentException(
          "Current backend is not supported for GHDL linting."
        )
    exec(
      cd,
      s"$binExec -a --std=$std ${filesCmdPart(cd)}"
    )
  end lint
end GHDL
