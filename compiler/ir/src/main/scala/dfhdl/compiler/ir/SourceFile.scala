package dfhdl.compiler.ir

final case class SourceFile(
    sourceOrigin: SourceOrigin,
    sourceType: SourceType,
    path: String,
    contents: String
) derives CanEqual

sealed trait SourceType extends Product with Serializable derives CanEqual
object SourceType:
  enum Design extends SourceType:
    case Regular
    case BlackBox
    case GlobalDef
    case DFHDLDef
  trait ToolConfig extends SourceType

enum SourceOrigin derives CanEqual:
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
