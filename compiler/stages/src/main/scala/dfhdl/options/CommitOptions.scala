package dfhdl.options

import dfhdl.options.CommitOptions.*
import dfhdl.compiler.ir
import java.io.File.separatorChar

final case class CommitOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop
)
object CommitOptions:
  given default(using
      commitFolder: CommitFolder = "sandbox",
      newFolderForTop: NewFolderForTop = true
  ): CommitOptions =
    CommitOptions(
      commitFolder = commitFolder,
      newFolderForTop = newFolderForTop
    )

  extension (co: CommitOptions)
    def commitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = x => x

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = x => x
end CommitOptions
