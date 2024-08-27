package dfhdl.options
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler
import dfhdl.core.{ClkCfg, RstCfg}

import java.io.File.separatorChar

import CompilerOptions.*
import dfhdl.internals.scastieIsRunning
final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: Backend,
    logLevel: LogLevel,
    printDesignCodeAfter: PrintDesignCodeAfter,
    printGenFiles: PrintGenFiles,
    dropUserOpaques: DropUserOpaques
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder,
      newFolderForTop: NewFolderForTop,
      backend: Backend,
      logLevel: LogLevel,
      printDesignCodeAfter: PrintDesignCodeAfter,
      printGenFiles: PrintGenFiles,
      dropUserOpaques: DropUserOpaques
  ): CompilerOptions =
    CompilerOptions(
      commitFolder = commitFolder, newFolderForTop = newFolderForTop, backend = backend,
      logLevel = logLevel, printDesignCodeAfter = printDesignCodeAfter,
      printGenFiles = printGenFiles, dropUserOpaques = dropUserOpaques
    )

  extension (co: CompilerOptions)
    def topCommitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder
    def compilePath(stagedDB: ir.DB): String =
      s"${co.topCommitPath(stagedDB)}${separatorChar}hdl"

  opaque type CommitFolder <: String = String
  object CommitFolder:
    given CommitFolder = "sandbox"
    given Conversion[String, CommitFolder] = identity

  opaque type NewFolderForTop <: Boolean = Boolean
  object NewFolderForTop:
    given NewFolderForTop = true
    given Conversion[Boolean, NewFolderForTop] = identity

  opaque type Backend <: BackendCompiler = BackendCompiler
  object Backend:
    given Backend = dfhdl.backends.verilog.sv2005
    given Conversion[BackendCompiler, Backend] = identity
    extension (backend: Backend)
      def isVHDL: Boolean = backend match
        case _: dfhdl.backends.vhdl => true
        case _                      => false
      def isVerilog: Boolean = backend match
        case _: dfhdl.backends.verilog => true
        case _                         => false
    export dfhdl.backends.*

  opaque type LogLevel <: dfhdl.options.LogLevel = dfhdl.options.LogLevel
  given Conversion[wvlet.log.LogLevel, LogLevel] = x => x.asInstanceOf[LogLevel]
  object LogLevel:
    given (using logLevel: dfhdl.options.LogLevel): LogLevel = logLevel
    export dfhdl.options.LogLevel.*

  opaque type PrintDesignCodeAfter <: Boolean = Boolean
  object PrintDesignCodeAfter:
    given PrintDesignCodeAfter = false
    given Conversion[Boolean, PrintDesignCodeAfter] = identity

  opaque type PrintGenFiles <: Boolean = Boolean
  object PrintGenFiles:
    given PrintGenFiles = if (scastieIsRunning) true else false
    given Conversion[Boolean, PrintGenFiles] = identity

  opaque type DropUserOpaques <: Boolean = Boolean
  object DropUserOpaques:
    given DropUserOpaques = false
    given Conversion[Boolean, DropUserOpaques] = identity
end CompilerOptions
