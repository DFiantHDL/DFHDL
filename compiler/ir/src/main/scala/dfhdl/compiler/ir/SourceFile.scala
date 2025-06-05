package dfhdl.compiler.ir
import dfhdl.internals.StableEnum
import upickle.default.*

final case class SourceFile(
    sourceOrigin: SourceOrigin,
    sourceType: SourceType,
    path: String,
    contents: String
) derives CanEqual,
      ReadWriter

enum SourceType extends StableEnum derives CanEqual, ReadWriter:
  case Design
  case BlackBox
  case GlobalDef
  case DFHDLDef
  case Tool(toolName: String, srcType: String)

enum SourceOrigin extends StableEnum derives CanEqual, ReadWriter:
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
