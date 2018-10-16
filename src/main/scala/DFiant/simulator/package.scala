package DFiant

package object simulator {

  implicit class DFPortInSim[T <: DFAny.Port[_, _ <: IN]](dfVal : T) { //(implicit simCtx : DFSimulator)
    object sim {
      def inject(token : dfVal.TToken) : Unit = {}

    }
  }

  implicit class DFPortOutSim[T <: DFAny.Port[_, _ <: OUT]](dfVal : T) { //(implicit simCtx : DFSimulator)
    object sim {
      def watch() : Unit = {}
    }
  }
}
