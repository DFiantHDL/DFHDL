package dfhdl.options
import CompilerOptions.*
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler
export wvlet.log.LogLevel

import java.io.File.separatorChar

final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: BackendCompiler,
    logLevel: CompilerLogLevel
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder = "sandbox",
      newFolderForTop: NewFolderForTop = true,
      backend: BackendCompiler,
      logLevel: CompilerLogLevel = LogLevel.WARN
  ): CompilerOptions =
    CompilerOptions(
      commitFolder = commitFolder,
      newFolderForTop = newFolderForTop,
      backend = backend,
      logLevel = logLevel
    )

  extension (co: CompilerOptions)
    def topCommitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder
    def compilePath(stagedDB: ir.DB): String =
      s"${co.topCommitPath(stagedDB)}${separatorChar}hdl"

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = x => x

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = x => x

  opaque type CompilerLogLevel <: LogLevel = LogLevel
  given Conversion[LogLevel, CompilerLogLevel] = x => x
end CompilerOptions
