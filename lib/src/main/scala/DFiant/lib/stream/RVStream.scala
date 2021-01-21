package DFiant.lib.stream

import DFiant._

abstract class RVStream(streamDir: StreamDir, nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix)(
  implicit ctx : ContextOf[RVStream]
) extends DFInterface(nameFlatten) {
  private def SourceDir(portDir : PortDir) : DclDir = streamDir match {
    case SOURCE => portDir
    case SINK => portDir match {
      case IN => OUT
      case OUT => IN
    }
    case FLOW => VAR
  }
  final val valid       = DFBit <> SourceDir(OUT) init false
  final val ready       = DFBit <> SourceDir(IN)  init false
  DEFAULT_DIR <> SourceDir(OUT)}
