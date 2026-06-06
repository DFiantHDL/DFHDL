package dfhdl.options
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler
import dfhdl.core.Design

import java.io.File.separatorChar

import CompilerOptions.*
import dfhdl.internals.scastieIsRunning
final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: _Backend,
    logLevel: _LogLevel,
    printDFHDLCode: PrintDFHDLCode,
    printBackendCode: PrintBackendCode,
    dropUserOpaques: DropUserOpaques
)
object CompilerOptions:
  opaque type Defaults[-T] <: CompilerOptions = CompilerOptions
  object Defaults:
    given (using
        commitFolder: CommitFolder,
        newFolderForTop: NewFolderForTop,
        backend: Backend,
        logLevel: LogLevel,
        printDFHDLCode: PrintDFHDLCode,
        printBackendCode: PrintBackendCode,
        dropUserOpaques: DropUserOpaques
    ): Defaults[Any] = CompilerOptions(
      commitFolder = commitFolder, newFolderForTop = newFolderForTop,
      backend = backend(dfhdl.backends),
      logLevel = logLevel(wvlet.log.LogLevel), printDFHDLCode = printDFHDLCode,
      printBackendCode = printBackendCode,
      dropUserOpaques = dropUserOpaques
    )
  end Defaults
  given (using defaults: Defaults[Design]): CompilerOptions = defaults

  extension (co: CompilerOptions)
    def topCommitPath(topName: String): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${topName}"
      else co.commitFolder
    def topCommitPath(stagedDB: ir.DB): String = co.topCommitPath(stagedDB.top.dclName)
    def cachePath(topName: String): String =
      s"${co.topCommitPath(topName)}${separatorChar}cache"

  into opaque type CommitFolder <: String = String
  object CommitFolder:
    given CommitFolder = "sandbox"
    given Conversion[String, CommitFolder] = identity

  into opaque type NewFolderForTop <: Boolean = Boolean
  object NewFolderForTop:
    given NewFolderForTop = true
    given Conversion[Boolean, NewFolderForTop] = identity

  type Backend = dfhdl.backends.type => _Backend
  private[dfhdl] into opaque type _Backend <: BackendCompiler = BackendCompiler
  object _Backend:
    given Backend = _ => dfhdl.backends.verilog.sv2009
    given Conversion[BackendCompiler, _Backend] = identity
    extension (backend: _Backend)
      def isVHDL: Boolean = backend match
        case _: dfhdl.backends.vhdl => true
        case _                      => false
      def isVerilog: Boolean = backend match
        case _: dfhdl.backends.verilog => true
        case _                         => false

  type LogLevel = wvlet.log.LogLevel.type => _LogLevel
  private[dfhdl] into opaque type _LogLevel <: dfhdl.options._LogLevel =
    dfhdl.options._LogLevel
  object _LogLevel:
    given Conversion[wvlet.log.LogLevel, _LogLevel] = x => x.asInstanceOf[_LogLevel]
    given (using logLevel: dfhdl.options.LogLevel): LogLevel = logLevel

  into opaque type PrintDFHDLCode <: Boolean = Boolean
  object PrintDFHDLCode:
    given PrintDFHDLCode = false
    given Conversion[Boolean, PrintDFHDLCode] = identity

  into opaque type PrintBackendCode <: Boolean = Boolean
  object PrintBackendCode:
    given PrintBackendCode = if (scastieIsRunning) true else false
    given Conversion[Boolean, PrintBackendCode] = identity

  into opaque type DropUserOpaques <: Boolean = Boolean
  object DropUserOpaques:
    given DropUserOpaques = false
    given Conversion[Boolean, DropUserOpaques] = identity
end CompilerOptions
