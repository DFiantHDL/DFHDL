package ZFiant

trait DFSimulator extends DFDesign {
  final private[ZFiant] override lazy val simMode : DFSimulator.Mode = DFSimulator.Mode.On
}

object DFSimulator {
  sealed trait Mode extends Product with Serializable
  object Mode {
    case object Off extends Mode
    case object On extends Mode
  }
}
