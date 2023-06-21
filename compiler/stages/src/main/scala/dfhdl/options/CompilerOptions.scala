package dfhdl.options
import CompilerOptions.*
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler

import java.io.File.separatorChar

final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: BackendCompiler
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder = "sandbox",
      newFolderForTop: NewFolderForTop = true,
      backend: BackendCompiler
  ): CompilerOptions =
    CompilerOptions(
      commitFolder = commitFolder,
      newFolderForTop = newFolderForTop,
      backend = backend
    )

  extension (co: CompilerOptions)
    def commitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = x => x

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = x => x
end CompilerOptions
