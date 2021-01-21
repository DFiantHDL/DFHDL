package DFiant.lib.util
import DFiant._

protected[util] object ActivationBit {
  @df object ActiveHigh extends DFOpaque.Of(DFBit)
  @df object ActiveLow extends DFOpaque.Of(DFBit)
  sealed trait Status
  object Status {
    object Active extends Status
    object Inactive extends Status
  }
}
