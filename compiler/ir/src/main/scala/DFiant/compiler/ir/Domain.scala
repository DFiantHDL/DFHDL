package DFiant.compiler.ir

sealed trait Domain derives CanEqual
object Domain:
  // dataflow domain
  sealed trait DF extends Domain
  object DF extends DF
  // real-time / register-transfer domain
  sealed trait RT extends Domain
  object RT:
    class LL() extends RT
    class HL(
        clkParams: ClockParams = ClockParams(),
        rstParams: ResetParams = ResetParams()
    ) extends RT
    final case class ResetParams(
        name: String = "rst",
        mode: ResetParams.Mode = ResetParams.Mode.Async,
        active: ResetParams.Active = ResetParams.Active.Low
    )
    object ResetParams:
      enum Mode:
        case Async, Sync
      enum Active:
        case Low, High

    final case class ClockParams(
        name: String = "clk",
        edge: ClockParams.Edge = ClockParams.Edge.Rising
    )
    object ClockParams:
      enum Edge:
        case Rising, Falling
  end RT
end Domain
