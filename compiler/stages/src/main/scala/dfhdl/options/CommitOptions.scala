package dfhdl.options

import dfhdl.options.CommitOptions.*

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

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = x => x

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = x => x
end CommitOptions
