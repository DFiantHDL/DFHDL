package dfhdl.compiler.ir
import upickle.default.*

final case class SourceFile(
    sourceOrigin: SourceOrigin,
    sourceType: SourceType,
    path: String,
    contents: String
) derives CanEqual,
      ReadWriter

enum SourceType derives CanEqual, ReadWriter:
  case Design
  case BlackBox
  case GlobalDef
  case DFHDLDef
  // An external data file loaded during elaboration (`initFile` memory contents), recorded with
  // the loaded contents under `SourceOrigin.External`. Elaboration caches re-read the file and
  // reject an entry whose file has since changed (see `DB.initFilesUnchanged`).
  case InitFile
  case Tool(toolName: String, srcType: String)

enum SourceOrigin derives CanEqual, ReadWriter:
  // Compiled files are a result from a compilation process.
  // These files exist only just in memory until they are committed.
  case Compiled
  // Committed files are compiled files that were committed to disk.
  case Committed
  // External files are existing files that are actively integrated
  // in the build or simulation.
  case External
  // Dependency is any dependency of files
  case Dependency
