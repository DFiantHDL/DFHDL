package DFiant.compiler.ir

sealed trait DomainType derives CanEqual
object DomainType:
  // dataflow domain
  sealed trait DF extends DomainType
  object DF extends DF
  // real-time / register-transfer domain
  sealed trait RT extends DomainType
  object RT:
    class LL() extends RT
    class HL(
        clkParams: ClockParams = ClockParams(),
        rstParams: ResetParams = ResetParams()
    ) extends RT
    sealed trait ResetParams extends Product with Serializable derives CanEqual
    case object NoReset extends ResetParams
    final case class WithReset(
        name: String,
        mode: ResetParams.Mode,
        active: ResetParams.Active
    ) extends ResetParams
    object ResetParams:
      def apply(
          name: String = "rst",
          mode: Mode = Mode.Async,
          active: Active = Active.Low
      ): ResetParams = WithReset(name, mode, active)
      enum Mode derives CanEqual:
        case Async, Sync
      enum Active derives CanEqual:
        case Low, High
    end ResetParams

    sealed trait ClockParams extends Product with Serializable derives CanEqual
    case object NoClock extends ClockParams
    final case class WithClock(
        name: String,
        edge: ClockParams.Edge
    ) extends ClockParams
    object ClockParams:
      def apply(
          name: String = "clk",
          edge: ClockParams.Edge = Edge.Rising
      ): ClockParams = WithClock(name, edge)
      enum Edge derives CanEqual:
        case Rising, Falling
  end RT
end DomainType
