package ZFiant

trait DFSimulator extends DFDesign {
  final private[ZFiant] override lazy val simMode : DFSimulator.Mode = DFSimulator.Mode.On

//  protected implicit class SingletonExtender[T <: Singleton](any : T) {
//    def :# (time : Int) : T = any
//  }
//  protected implicit class AnyExtender[T](any : T) {
//    def after (time : Int) : T = any
//  }
}

object DFSimulator {
  private object Tag {
    final case class After(time : Int) extends DFAny.CustomTag
  }

  sealed trait Mode extends Product with Serializable
  object Mode {
    case object Off extends Mode
    case object On extends Mode
  }
}
