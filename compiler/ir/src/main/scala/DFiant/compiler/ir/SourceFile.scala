package DFiant.compiler.ir

final case class SourceFile(sourceType: SourceType, path: String) derives CanEqual
enum SourceType:
  // Compiled files are a result from a compilation process.
  // These files exist only just in memory until they are committed.
  case Compiled
  // Committed files are compiled files that were committed to disk.
  case Committed
  // External RTL files are existing RTL files that are actively integrated
  // in the build or simulation.
  case ExternalRTL
  // Dependency is any dependency of files
  case Dependency
